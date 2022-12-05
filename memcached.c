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

enum code {
	PUT = 11,
	DEL = 12,
	GET = 13,
	TAKE = 14,

	STATS = 21,

	OK = 101,
	EINVALID = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};

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
void binary(struct fdinfo *fdinfo) {
	int fd = fdinfo->fd;
	int t;

	// inet_ntop(AF_INET, &fdinfo->sin.sin_addr, buf1, sizeof fdinfo->sin);
	do {
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
			void *key = malloc(length);
			t = read(fd, key, length);

			length = get_length(fd);
			void *value = malloc(length);
			t = read(fd, value, length);

			int i = memcached_put(key, value);
			if (!i) {
				int code = OK;
				write(fd, &code, 1);
			}
		} 
		else if (buf == DEL)  {
			int length = get_length(fd);
			void *key = malloc(length);
			t = read(fd, key, length);

			int i = memcached_delete(key);
			if (!i) {
				int code = OK;
				write(fd, &code, 1);
			} else {
				int code = ENOTFOUND;
				write(fd, &code, 1);
			}
		} 
		else if (buf == GET) {
			int length = get_length(fd);
			void *key = malloc(length);
			t = read(fd, key, length);

			void *value;
			int i = memcached_get(key, value);
			if (!i) {
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
			void *key = malloc(length);
			t = read(fd, key, length);

			void *value;
			int i = memcached_take(key, value);
			if (!i) {
				int code = OK;
				write(fd, &code, 1);
				write(fd, value, len(value));
			} else {
				int code = ENOTFOUND;
				write(fd, &code, 1);
			}
		} 
		else if (buf == STATS) {
			memcached_stats(fd);
		} 
		else {
			int code = EINVAL;
			write(fd, &code, 1);
			/*Limpiar buffer?*/
		}
	} while (1);
}

// Handler de una conexion a cliente en modo texto
void text(struct fdinfo *fdinfo) {
	int fd = fdinfo->fd;
	int t;
	char /*buf1[2048],*/ input[2048];

	// inet_ntop(AF_INET, &fdinfo->sin.sin_addr, buf1, sizeof fdinfo->sin);
	do {
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

			int i = put(key, value);
			if (!i)
				write(fd, "OK", 2);
		} 
		else if (strcmp(comm, "DEL") == 0) {
			char *key = strtok(NULL, " ");

			int i = delete(key);
			if (!i) {
				write(fd, "OK", 2);
			} else {
				write(fd, "ENOTFOUND", 9);
			}
		} 
		else if (strcmp(comm, "GET") == 0) {
			char *key = strtok(NULL, " ");

			void *value;
			int i = get(key, value);
			if (!i) {
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

			void *value;
			int i = take(key, value);
			if (!i) {
				if (1/*representable*/) {
					write(fd, "OK ", 3);
					write(fd, value, strlen(value));
				}
				else {
					write(fd, "EBINARY", 7);
				}
			} else {
				write(fd, "ENOTFOUND", 9);
			}
		} 
		else if (strcmp(comm, "STATS") == 0) {
			stats(fd);
		} 
		else {
			write(fd, "EINVAL", 6);
		}
	} while (1);
}

int main() {

}