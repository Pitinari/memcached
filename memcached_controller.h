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

enum commands {
	PUT = 11,
	DEL = 12,
	GET = 13,
	TAKE = 14,
	STATS = 21,
};

enum responses {
	OK = 101,
	EINVALID = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
};

enum bin_state_current_reading {
	OPERATOR,
	KEY_SIZE,
	KEY,
	VALUE_SIZE,
	VALUE
};

struct bin_state {
	enum bin_state_current_reading reading;
	uint8_t operator;
	unsigned cursor;
	uint8_t sizeBuf[4];

	unsigned keyLen;
	void* key;

	unsigned valueLen;
	void* value;
};

struct text_state {
	size_t cursor;
	char buf[2048];
};

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, struct bin_state *bin, Memcached table);

// Handler de una conexion a cliente en modo texto
bool text_handler(int fd, struct text_state *text, Memcached table);