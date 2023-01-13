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
	EMPTY = 0,
};

enum responses {
	OK = 101,
	EINVALID = 111,
	ENOTFOUND = 112,
	EBINARY = 113,
	EBIG = 114,
	EUNK = 115,
	EOOM = 116,
};

enum bin_state_current_reading {
	OPERATOR = 0,
	KEY_SIZE = 1,
	KEY = 2,
	VALUE_SIZE = 3,
	VALUE = 4,
	COMPLETED = 5
};

struct bin_state {
	enum bin_state_current_reading reading;
	uint8_t command;
	unsigned cursor;
	uint8_t sizeBuf[4];

	unsigned keyLen;
	void* key;

	unsigned valueLen;
	void* value;
};

struct text_state {
	size_t cursor;
	int wordsCount;
	char buf[2048];
	char *comm[3];
	char *lastReference;
};

// Handler de una conexion a cliente en modo binario
bool binary_handler(int fd, struct bin_state *bin, Memcached table);

// Handler de una conexion a cliente en modo texto
bool text_handler(int fd, struct text_state *text, Memcached table);