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

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, struct bin_state *bin, Memcached table) {
	int t;
	again:
	switch (bin->reading)
	{
	case OPERATOR:
		t = read(fd, &bin->command, 1);
		if(t <= 0) goto error_input;
		if(!validate_operation(bin->command)){
			fprintf(stderr, "einval\n");
			int code = EINVAL;
			write(fd, &code, 1);
		}
		bin->cursor = t;
		bin->reading = KEY_SIZE;
		goto again;
		break;
	case KEY_SIZE:
		char buf[4];
		t = read(fd, &buf, 5 - bin->cursor);
		if(t <= 0) goto error_input;
		for(int i = 0; i < t; i++){
			bin->sizeBuf[bin->cursor] = buf[i];
			bin->cursor++;
		}
		if(bin->cursor == 5){
			bin->reading = KEY;
			for(int i = 0; i < 4; i++){
				bin->keyLen = bin->sizeBuf[i]
			}
		}
	default:
		break;
	}
	
	int t;
	char buf = 0;
	t = read(fd, &buf, 1);

	/* EOF */
	if (t == 0){
		close(fd);
		fprintf(stderr, "socket closed. fd = %i\n", fd);
		return false;
	}

	/* No hay más nada por ahora */
	if (t < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)) {
		fprintf(stderr, "nothing to read. fd = %i\n", fd);
		return true;
	}

	/* Algún error */
	if (t < 0) {
		fprintf(stderr, "error en fd %i? %i\n", fd, errno);
		return false;
	}

	if (buf == PUT) {
		int keyLength = get_length(fd);
		void *key = custom_malloc(table->ht, keyLength);
		if (key == NULL) return true;
		t = read(fd, key, keyLength);

		int valueLength = get_length(fd);
		void *value = custom_malloc(table->ht, valueLength);
		if (value == NULL) {
			free(key);
			return true;
		}
		t = read(fd, value, valueLength);

		int i = memcached_put(table, key, keyLength, value);
		if (i == 0) {
			int code = OK;
			write(fd, &code, 1);
		}
	} 
	else if (buf == DEL)  {
		int keyLength = get_length(fd);
		void *key = custom_malloc(table->ht, keyLength);
		if (key == NULL) return true;
		t = read(fd, key, keyLength);

		int i = memcached_delete(table, key, keyLength);
		if (i == 0) {
			int code = OK;
			write(fd, &code, 1);
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
		free(key);
	} 
	else if (buf == GET) {
		int keyLength = get_length(fd);
		// fprintf(stderr, "length: %d\n", keyLength);
		void *key = custom_malloc(table->ht, keyLength);
		if (key == NULL) return true;
		t = read(fd, key, keyLength);

		void *value = memcached_get(table, key, keyLength);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			send_length(fd, sizeof(value));
			write(fd, value, sizeof(value));
		} 
		else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
		free(key);
	} 
	else if (buf == TAKE) {
		int keyLength = get_length(fd);
		void *key = custom_malloc(table->ht, keyLength);
		if (key == NULL) return true;
		t = read(fd, key, keyLength);

		void *value = memcached_take(table, key, keyLength);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			send_length(fd, sizeof(value));
			write(fd, value, sizeof(value));
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
		free(key);
		free(value);
	} 
	else if (buf == STATS) {
		char* line = memcached_stats(table);
		char code = OK;
		write(fd, &code, 1);
		send_length(fd, strlen(line)+1);
		write(fd, line, strlen(line)+1);
		free(line);
	} 
	else {
		fprintf(stderr, "einval\n");
		int code = EINVAL;
		write(fd, &code, 1);
	}
	return true;
}

int get_input_commands(struct text_state *text, char **commads){
	char *lastReference = text->buf, *input = text->buf;
	int wordsCount = 1;
	while(*input != '\n'){
		if(*input == ' '){
			*input = '\0';
			*commads = lastReference;
			commads++;
			input++;
			lastReference = input;
			wordsCount++;
		} else input++;
	}
	*input = '\0';
	*commads = lastReference;
	return wordsCount;
}

void reset_input_buffer(struct text_state *text, char **commads, int wordCount){
	int commandsLen = 0;
	int i = 0;
	while(i < wordCount)
		commandsLen += (strlen(commads[i++]) + 1);

	i = 0;
	while(i <= (text->cursor - commandsLen)){
		text->buf[i] = text->buf[commandsLen + i];
		i++;
	}
	text->cursor -= commandsLen;
}

// Handler de una conexion a cliente en modo texto
bool text_handler(int fd, struct text_state *text, Memcached table) {

	int t = read(fd, text->buf+text->cursor, 2048 - text->cursor);

	/* EOF */
	if (t == 0){
		fprintf(stdin, "socket closed. fd = %i\n", fd);
		close(fd);
		return false;
	}

	/* No hay más nada por ahora */
	if (t < 0 && (errno == EAGAIN || errno == EWOULDBLOCK)){
		fprintf(stdin, "nothing to read. fd = %i\n", fd);
		return true;
	}

	/* Algún error */
	if (t < 0) {
		fprintf(stderr, "error en fd %i? %i\n", fd, errno);
		close(fd);
		return false;
	}

	text->cursor += t;

	if(text->cursor == 2048 && text->buf[2047] != '\n'){
		write(fd, "EINVAL\n", 8);
		text->cursor = 0;
		return true;
	}

	char *comm[3];
	int wordsCount = get_input_commands(text, comm);
	if (strcmp(comm[0], "STATS") == 0 && wordsCount == 1) {
		char* stats = memcached_stats(table);
		write(fd, "OK\n", 4);
		write(fd, stats, strlen(stats)+1);
	} else if(wordsCount > 1){
		
		if (strcmp(comm[0], "DEL") == 0 && wordsCount == 2) {

			int i = memcached_delete(table, comm[1], strlen(comm[1]));
			if (i == 0) {
				write(fd, "OK\n", 4);
			} else {
				write(fd, "ENOTFOUND\n", 11);
			}
		} 
		else if (strcmp(comm[0], "GET") == 0 && wordsCount == 2) {
			char *value = memcached_get(table, comm[1], strlen(comm[1]));
			if (value) {
				if (1/*representable*/) {
					write(fd, "OK\n", 4);
					write(fd, value, strlen(value)+1);
				}
				else {
					write(fd, "EBINARY\n", 9);
				}
			} 
			else {
				write(fd, "ENOTFOUND\n", 11);
			}
		} 
		else if (strcmp(comm[0], "TAKE") == 0 && wordsCount == 2) {

			char *value = memcached_take(table, comm[1], strlen(comm[1]));
			if (value) {
				if (1/*representable*/) {
					write(fd, "OK\n", 4);
					write(fd, value, strlen(value)+1);
				}
				else {
					write(fd, "EBINARY\n", 9);
				}
				free(value);
			} else {
				write(fd, "ENOTFOUND\n", 11);
			}
		} 
		else if (strcmp(comm[0], "PUT") == 0 && wordsCount == 3) {
			char *key = custom_malloc(table->ht, strlen(comm[1]) + 1);
			if(key == NULL){
				return true; // error
			}
			strcpy(key, comm[1]);

			char *value = custom_malloc(table->ht, strlen(comm[2]) + 1);
			if(value == NULL) {
				free(key);
				return true; // error
			}
			strcpy(value, comm[2]);
			int i = memcached_put(table, key, strlen(key), value);
			if (i == 0)
				write(fd, "OK\n", 4);
		} 
		else {
			write(fd, "EINVAL\n", 8);
		}
	}
	reset_input_buffer(text, comm, wordsCount);
	return true;
}