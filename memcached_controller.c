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

struct fdinfo {
	enum {
		LSOCK,
		CLIENT
	} type;
	int fd;
	struct sockaddr_in sin;
};

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

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, Memcached table) {
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
		t = read(fd, key, keyLength);

		int valueLength = get_length(fd);
		void *value = custom_malloc(table->ht, valueLength);
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

int get_input_commands(char *input, char **commads){
	char *lastReference = input;
	int wordsCount = 1;
	while(*input != '\n'){
		if(*input == ' '){
			*input = '\0';
			*commads = lastReference;
			commads++;
			input++;
			wordsCount++;
			lastReference = input;
		} else input++;
	}
	*input = '\0';
	*commads = lastReference;
	return wordsCount;
}

// Handler de una conexion a cliente en modo texto
bool text_handler(int fd, Memcached table) {
	int t;
	char input[2048];

	// inet_ntop(AF_INET, &fdinfo->sin.sin_addr, buf1, sizeof fdinfo->sin);
	t = read(fd, input, 1);

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

	t = read(fd, input+1, 2047);

	if (input[t] != '\n')
		return false; /// mal formato

	char *comm[3];
	int wordsCount = get_input_commands(input, comm);
	// fprintf(stderr, "'%s' '%s' '%s'\n", comm[0], comm[1], comm[2]);
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
			// fprintf(stderr, "'%s'\n", value);
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
			if(key == NULL) return true; // error
			strcpy(key, comm[1]);

			char *value = custom_malloc(table->ht, strlen(comm[2]) + 1);
			if(value == NULL) {
				free(key);
				return true; // error
			}
			strcpy(value, comm[2]);
			int i = memcached_put(table, key, strlen(key) + 1, value);
			if (i == 0)
				write(fd, "OK\n", 4);
		} 
		else {
			write(fd, "EINVAL\n", 8);
		}
	}
	return true;
}