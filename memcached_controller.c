/* Verificar mallocs no NULL y liberar memoria
 * Verificar lecturas (t < 0)
 * Crear funciones que manejan la cache (put, get, ...)
 * Verificar si los valores son representables como strings
 * Verificar el tamaño de los valores en binario recibidos de la cache
 */

#include <unistd.h>
#include <netinet/in.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <stdlib.h>
#include "memcached_service.h"
#include "memcached_controller.h"

int get_length(int fd) {
	int len;
	char lbuf[4];

	/* Recibir 4 bytes de la clave */
	read(fd, lbuf, 4);

	/* Castear esos bytes a un int */
	int len_net = *(int*)lbuf;
	/* Pero eran big-endian, así que llevar al endianness de la máquina */
	len = ntohl(len_net);
	return len;
}

void send_length(int fd, int len) {
	int len_net = htonl(len);
	write(fd, &len_net, 4);
}

bool validate_operation(int op){
	return op == PUT || op == DEL || op == GET || op == TAKE || op == STATS;
}

// Lee del fd y va llenando los campos correspondientes al comando
int binary_read_handler(int fd, struct bin_state *bin, Memcached table){
	int t;
	switch (bin->reading)
	{
	case OPERATOR:
		t = read(fd, &bin->command, 1);
		if(t <= 0) goto error_input;
		if(!validate_operation(bin->command)){
			fprintf(stderr, "EINVAL BINARY\n");
			int code = EINVAL;
			write(fd, &code, 1);
			return 0;
		}
		bin->reading = KEY_SIZE;
		break;
	case KEY_SIZE:

		t = read(fd, bin->sizeBuf + bin->cursor, 4 - bin->cursor);
		if(t <= 0) goto error_input;
		bin->cursor += t;
		if(bin->cursor == 4){
			bin->keyLen = ntohl(*(unsigned *)bin->sizeBuf);
			bin->key = custom_malloc(table->ht, bin->keyLen);
			if(bin->key == NULL) return -1;
			bin->reading = KEY;
			bin->cursor = 0;
		}
		break;
	case KEY:
		t = read(fd, bin->key + bin->cursor, bin->keyLen - bin->cursor);
		if(t <= 0) goto error_input;
		bin->cursor += t;
		if(bin->cursor == bin->keyLen){
			bin->reading = VALUE_SIZE;
			bin->cursor = 0;
		}
		break;
	case VALUE_SIZE:
		t = read(fd, bin->sizeBuf + bin->cursor, 4 - bin->cursor);
		if(t <= 0) goto error_input;
		bin->cursor += t;
		if(bin->cursor == 4){
			bin->reading = VALUE;
			bin->valueLen = ntohl(*(unsigned *)bin->sizeBuf);
			bin->value = custom_malloc(table->ht, bin->valueLen);
			if(bin->value == NULL){
				free(bin->key);
				return -1;
			}
			bin->cursor = 0;
		}
		break;
	case VALUE:
		t = read(fd, bin->value + bin->cursor, bin->valueLen - bin->cursor);
		if(t <= 0) goto error_input;
		bin->cursor += t;
		if(bin->cursor == bin->valueLen){
			bin->reading = COMPLETED;
			bin->cursor = 0;
		}
		break;
	}
	return 1;
	
	error_input:

	/* EOF */
	if (t == 0) {
		close(fd);
		goto error_die;
	}
	/* No hay más nada por ahora */
	if (t < 0) {
		if(errno == EAGAIN || errno == EWOULDBLOCK) {
			return 0;
		} else {
			close(fd);
			fprintf(stderr, "Error. Kill socket\n");
			goto error_die;
		}
	}

	error_die:
	if(bin->reading > KEY_SIZE){
		free(bin->key);
		if(bin->reading == VALUE){
			free(bin->value);
		}
	}
	return -1;
}

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, struct bin_state *bin, Memcached table) {
	int t;
	start:
	
	if (bin->command == PUT) {
		if(bin->reading < COMPLETED) goto read;
		int i = memcached_put(table, bin->key, bin->keyLen, bin->value, bin->valueLen);
		if (i == 0) {
			int code = OK;
			write(fd, &code, 1);
		}
		bin->cursor = 0;
		bin->reading = OPERATOR;
		bin->command = EMPTY;
	} 
	else if (bin->command == DEL)  {
		if(bin->reading < VALUE_SIZE) goto read;
		int i = memcached_delete(table, bin->key, bin->keyLen);
		if (i == 0) {
			int code = OK;
			write(fd, &code, 1);
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
		free(bin->key);
		bin->cursor = 0;
		bin->reading = OPERATOR;
		bin->command = EMPTY;
	} 
	else if (bin->command == GET) {
		if(bin->reading < VALUE_SIZE) goto read;
		void *value;
		unsigned valueLen;
		memcached_get(table, bin->key, bin->keyLen, &value, &valueLen);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			send_length(fd, valueLen);
			write(fd, value, valueLen);
			free(value);
		} 
		else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
		free(bin->key);
		bin->cursor = 0;
		bin->reading = OPERATOR;
		bin->command = EMPTY;
	} 
	else if (bin->command == TAKE) {
		if(bin->reading < VALUE_SIZE) goto read;
		void *value;
		unsigned valueLen;
		memcached_take(table, bin->key, bin->keyLen, &value, &valueLen);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			send_length(fd, valueLen);
			write(fd, value, valueLen);
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
		free(bin->key);
		free(value);
		bin->cursor = 0;
		bin->reading = OPERATOR;
		bin->command = EMPTY;
	} 
	else if (bin->command == STATS) {
		char* line = memcached_stats(table);
		if(line != NULL){
			char code = OK;
			write(fd, &code, 1);
			send_length(fd, strlen(line));
			write(fd, line, strlen(line));
			free(line);
		} else {
			// Tuve un error, me olvido del comando
			// y mando "error unknown"
			char code = EUNK;
			write(fd, &code, 1);
		}
		bin->cursor = 0;
		bin->reading = OPERATOR;
		bin->command = EMPTY;
	} else{
		read:
		t = binary_read_handler(fd, bin, table);

		if(t < 0) return false;
		else if(t == 0) return true;
		else goto start;
	}
	return true;
}

/* Retorna cuantos caracteres se movio hasta encontrar \n o 0 si no se encontro */
int get_input_commands(struct text_state *text, int t){
	for (int i = 0; i < t; i++) {
		if(text->buf[text->cursor + i] == ' '){
			text->buf[text->cursor + i] = '\0';
			if (text->wordsCount < 3) {
				text->comm[text->wordsCount] = text->lastReference;
			}
			text->wordsCount++;
			text->lastReference = text->buf + text->cursor + i + 1;
		} else if (text->buf[text->cursor + i] == '\n') {
			text->buf[text->cursor + i] = '\0';
			if (text->wordsCount < 3) {
				text->comm[text->wordsCount] = text->lastReference;
			}
			text->wordsCount++;
			text->cursor += i + 1;
			text->lastReference = text->buf + text->cursor;
			return i + 1;
		}
	}
	text->cursor += t;
	return 0;
}

void reset_input_buffer(struct text_state *text, int t, int rc) {
	for (int i = 0; rc + i < t; i++)	{
		text->buf[i] = text->buf[text->cursor + i];
	}
	text->cursor = 0;
	text->lastReference = text->buf;
	text->wordsCount = 0;
}

// Handler de una conexion a cliente en modo texto
bool text_handler(int fd, struct text_state *text, Memcached table) {

	int t = read(fd, text->buf+text->cursor, 2048 - text->cursor);

	/* EOF */
	if (t == 0){
		close(fd);
		return false;
	}

	/* No hay más nada por ahora */
	if (t < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)){
		return true;
	}

	/* Algún error */
	if (t < 0) {
		fprintf(stderr, "Error. Kill socket\n");
		close(fd);
		return false;
	}
again:
	int rc = get_input_commands(text, t);
	// fprintf(stderr, "'%s' '%s' '%s'\n", text->comm[0], text->comm[1], text->comm[2]);
	// fprintf(stderr, "'%s' '%s' '%ld'\n", text->buf, text->lastReference, text->cursor);
	if (rc == 0) return true; // falta leer
	if (strcmp(text->comm[0], "STATS") == 0 && text->wordsCount == 1) {
		char* stats = memcached_stats(table);
		write(fd, "OK ", 3);
		write(fd, stats, strlen(stats));
		write(fd, "\n", 1);
	} else if(text->wordsCount > 1){
		
		if (strcmp(text->comm[0], "DEL") == 0 && text->wordsCount == 2) {

			int i = memcached_delete(table, text->comm[1], strlen(text->comm[1]));
			if (i == 0) {
				write(fd, "OK\n", 3);
			} else {
				write(fd, "ENOTFOUND\n", 10);
			}
		} 
		else if (strcmp(text->comm[0], "GET") == 0 && text->wordsCount == 2) {
			void *value;
			unsigned valueLen;
			memcached_get(table, text->comm[1], strlen(text->comm[1]), &value, &valueLen);
			if (value) {
				write(fd, "OK ", 3);
				write(fd, value, valueLen);
				write(fd, "\n", 1);
				free(value);
			} 
			else {
				write(fd, "ENOTFOUND\n", 11);
			}
		} 
		else if (strcmp(text->comm[0], "TAKE") == 0 && text->wordsCount == 2) {
			void *value = NULL;
			unsigned valueLen;
			memcached_take(table, text->comm[1], strlen(text->comm[1]), &value, &valueLen);
			if (value) {
				write(fd, "OK ", 3);
				write(fd, value, valueLen);
				write(fd, "\n", 1);
				free(value);
			} else {
				write(fd, "ENOTFOUND\n", 10);
			}
		} 
		else if (strcmp(text->comm[0], "PUT") == 0 && text->wordsCount == 3) {
			char *key = custom_malloc(table->ht, strlen(text->comm[1]) + 1);
			if(key == NULL){
				return true; // error
			}
			strcpy(key, text->comm[1]);

			char *value = custom_malloc(table->ht, strlen(text->comm[2]) + 1);
			if(value == NULL) {
				free(key);
				return true; // error
			}
			strcpy(value, text->comm[2]);
			int i = memcached_put(table, key, strlen(key), value, strlen(value));
			if (i == 0)
				write(fd, "OK\n", 3);
		} 
		else {
			write(fd, "EINVAL\n", 7);
		}
	} else {
		write(fd, "EINVAL\n", 7);
	}
	reset_input_buffer(text, t, rc);
	t -= rc;
	goto again;
	return true;
}