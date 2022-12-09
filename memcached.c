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
#include "memcached.h"

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
void binary(int fd, Memcached table) {
	int t;
	int buf;
	t = read(fd, &buf, 1);

	/* EOF */
	if (t == 0)
		return;

	/* No hay más nada por ahora */
	if (t < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return;

	/* Algún error */
	if (t < 0) {
		fprintf(stderr, "error en fd %i? %i\n", fd, errno);
		return;
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

		int i = memcached_delete(table, key);
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

		void *value = memcached_get(table, key);
		if (value != NULL) {
			int code = OK;
			write(fd, &code, 1);
			write(fd, value, len(value));
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
			write(fd, value, len(value));
		} else {
			int code = ENOTFOUND;
			write(fd, &code, 1);
		}
	} 
	else if (buf == STATS) {
		char* line = memcached_stats(table);
		write(fd, line, len(line));
		free(line);
	} 
	else {
		int code = EINVAL;
		write(fd, &code, 1);
		/*Limpiar buffer?*/
	}
}

// Handler de una conexion a cliente en modo texto
void text(int fd, Memcached table) {
	int t;
	char /*buf1[2048],*/ input[2048];

	// inet_ntop(AF_INET, &fdinfo->sin.sin_addr, buf1, sizeof fdinfo->sin);
	input[0] = 0;
	t = read(fd, input, 2047);

	/* EOF */
	if (t == 0)
		return;

	/* No hay más nada por ahora */
	if (t < 0 && (errno == EAGAIN || errno == EWOULDBLOCK))
		return;

	/* Algún error */
	if (t < 0) {
		fprintf(stderr, "error en fd %i? %i\n", fd, errno);
		return;
	}

	char *comm = strtok(input, " ");

	if (strcmp(comm, "PUT") == 0) {
		char *key = strtok(NULL, " ");
		char *value = strtok(NULL, " ");

		int i = memcached_put(table, key, strlen(key), value);
		if (i == 0)
			write(fd, "OK", 2);
	} 
	else if (strcmp(comm, "DEL") == 0) {
		char *key = strtok(NULL, " ");

		int i = memcached_delete(table, key, strlen(key));
		if (i == 0) {
			write(fd, "OK", 2);
		} else {
			write(fd, "ENOTFOUND", 9);
		}
	} 
	else if (strcmp(comm, "GET") == 0) {
		char *key = strtok(NULL, " ");

		char *value = memcached_get(table, key, strlen(key));
		if (value) {
			if (1/*representable*/) {
				write(fd, "OK ", 3);
				write(fd, value, strlen(value));
			}
			else {
				write(fd, "EBINARY", 7);
			}
		} 
		else {
			write(fd, "ENOTFOUND", 9);
		}
	} 
	else if (strcmp(comm, "TAKE") == 0) {
		char *key = strtok(NULL, " ");

		char *value = memcached_take(table, key, strlen(key));
		if (value) {
			if (1/*representable*/) {
				write(fd, "OK ", 3);
				write(fd, value, strlen(value));
			}
			else {
				write(fd, "EBINARY", 7);
			}
			free(value);
		} else {
			write(fd, "ENOTFOUND", 9);
		}
	} 
	else if (strcmp(comm, "STATS") == 0) {
		char* stats = memcached_stats(table);
		write(fd, stats, strlen(stats));
	} 
	else {
		write(fd, "EINVAL", 6);
	}
}

int main() {

}