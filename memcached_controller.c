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

// Lee el tamaño del dato
int get_length(int fd) {
	int t;
	int length = 0;
	int buf;
	for (int i = 0; i < 4; i++) {
		t = read(fd, &buf, 1);
		if (t < 0) {
			fprintf(stderr, "error en fd %i? %i\n", fd, errno);
			return -1;
		}
		length += buf << (8*(3-i));
	}
}

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, Memcached table) {
	int t;
	int buf;
	t = read(fd, &buf, 1);

	/* EOF */
	if (t == 0)
		close(fd);
		fprintf(stdin, "socket closed. fd = %i\n", fd);
		return false;

	/* No hay más nada por ahora */
	if (t < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		fprintf(stdin, "nothing to read. fd = %i\n", fd);
		return true;

	/* Algún error */
	if (t < 0) {
		fprintf(stderr, "error en fd %i? %i\n", fd, errno);
		return false;
	}

	if (buf == PUT) {
		int length = get_length(fd);
		void *key = custom_malloc(table->ht, length);
		t = read(fd, key, length);

		length = get_length(fd);
		void *value = custom_malloc(table->ht, length);
		t = read(fd, value, length);

		int i = memcached_put(table, key, length, value);
		if (i == 0) {
			int code = OK;
			write(fd, &code, 1);
		}
	} 
	else if (buf == DEL)  {
		int length = get_length(fd);
		void *key = custom_malloc(table->ht, length);
		t = read(fd, key, length);

		int i = memcached_delete(table, key, length);
		if (i == 0) {
			int code = OK;
			write(fd, &code, 1);
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
	} 
	else if (buf == GET) {
		int length = get_length(fd);
		void *key = custom_malloc(table->ht, length);
		t = read(fd, key, length);

		void *value = memcached_get(table, key, length);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			write(fd, value, strlen(value));
		} 
		else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
	} 
	else if (buf == TAKE) {
		int length = get_length(fd);
		void *key = custom_malloc(table->ht, length);
		t = read(fd, key, length);

		void *value = memcached_take(table, key, length);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			write(fd, value, strlen(value));
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
	} 
	else if (buf == STATS) {
		char* line = memcached_stats(table);
		write(fd, line, strlen(line));
		free(line);
	} 
	else {
		int code = EINVAL;
		write(fd, &code, 1);
		/*Limpiar buffer?*/
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
	if (strcmp(comm[0], "STATS") == 0 && wordsCount == 1) {
		char* stats = memcached_stats(table);
		write(fd, stats, strlen(stats));
	} else if(wordsCount > 1){
		
		if (strcmp(comm[0], "DEL") == 0 && wordsCount == 2) {

			int i = memcached_delete(table, comm[1], strlen(comm[1]));
			if (i == 0) {
				write(fd, "OK\n", 2);
			} else {
				write(fd, "ENOTFOUND\n", 9);
			}
		} 
		else if (strcmp(comm[0], "GET") == 0 && wordsCount == 2) {

			char *value = memcached_get(table, comm[1], strlen(comm[1]));
			if (value) {
				if (1/*representable*/) {
					write(fd, "OK\n", 3);
					write(fd, value, strlen(value));
				}
				else {
					write(fd, "EBINARY\n", 7);
				}
			} 
			else {
				write(fd, "ENOTFOUND\n", 9);
			}
		} 
		else if (strcmp(comm[0], "TAKE") == 0 && wordsCount == 2) {

			char *value = memcached_take(table, comm[1], strlen(comm[1]));
			if (value) {
				if (1/*representable*/) {
					write(fd, "OK\n", 3);
					write(fd, value, strlen(value));
				}
				else {
					write(fd, "EBINARY\n", 7);
				}
				free(value);
			} else {
				write(fd, "ENOTFOUND\n", 9);
			}
		} 
		else if (strcmp(comm[0], "PUT") == 0 && wordsCount == 3) {
			char *key = custom_malloc(table->ht, strlen(comm[1]) + 1);
			if(key == NULL) return true; // error
			strcpy(key, comm[1]);

			char *value = custom_malloc(table->ht, strlen(comm[2]) + 1);
			if(value == NULL) return true; // error
			strcpy(value, comm[2]);

			int i = memcached_put(table, key, strlen(key), value);
			if (i == 0)
				write(fd, "OK\n", 2);
		} 
		else {
			write(fd, "EINVAL\n", 6);
		}
	}
	return true;
}