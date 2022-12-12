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

// Lee el tamaño del dato
int get_length(int fd);

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, Memcached table);

// Handler de una conexion a cliente en modo texto
bool text_handler(int fd, Memcached table);