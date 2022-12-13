#include <netdb.h>
#include <assert.h>
#include <netinet/ip.h>
#include <stdio.h>
#include <string.h>
#include <sys/epoll.h>
#include <sys/socket.h>
#include <unistd.h>
#include <stdarg.h>
#include <errno.h>
#include <stdlib.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <sys/select.h>
#include "memcached_controller.h"
#include "memcached_service.h"
#include <pthread.h>
#include <sys/resource.h>
#include <fcntl.h>

#define DATA_LIMIT 1000000000
#define NUM_OF_NODES 10000
#define N_THREADS 10

typedef struct sockaddr_in sin;
typedef struct sockaddr    sad;
struct _args {
	int epfd;
	int binSock;
	int textSock;
	Memcached mc;
};

typedef struct _args *args;

struct _dataEvent {
	bool bin;
	int fd;
	union input_state {
		struct text_state *text;
		struct bin_state *bin;
	} input_state;
};

typedef struct _dataEvent *dataEvent;

int register_fd(int epfd, int fd, bool bin, Memcached mc) {
	
	int flags = fcntl(fd, F_GETFL);
	if (flags == -1) return -1;
	if (fcntl(fd, F_SETFL, flags | O_NONBLOCK) == -1) return -1;

	struct epoll_event ev;
	dataEvent data = custom_malloc(mc->ht, sizeof(struct _dataEvent));
	if(data == NULL) return -1;
	data->bin = bin;
	data->fd = fd;
	if(bin) {
		data->input_state.bin = custom_malloc(mc->ht, sizeof(struct bin_state));
		if(data->input_state.bin == NULL) goto error;
		data->input_state.bin->cursor = 0;
		data->input_state.bin->reading = OPERATOR;
		data->input_state.bin->command = EMPTY;
		data->input_state.bin->keyLen = 0;
		data->input_state.bin->valueLen = 0;
	} else {
		data->input_state.text = custom_malloc(mc->ht, sizeof(struct text_state));
		if(data->input_state.text == NULL) goto error;
		data->input_state.text->cursor = 0;
	}
	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.ptr = (void *)data;
	return epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &ev);

	error:
	free(data);
	return -1;
}

int modify_fd(int epfd, int fd, dataEvent ptr, Memcached mc) {
	struct epoll_event ev;
	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.ptr = (void *)ptr;
	return epoll_ctl(epfd, EPOLL_CTL_MOD, fd, &ev);
}

static void die(char *s, ...)
{
	va_list v;

	va_start(v, s);
	vfprintf(stderr, s, v);
	fprintf(stderr, "\n");
	va_end(v);
	fprintf(stderr, " -- errno = %i (%m)\n", errno);

	fflush(stderr);
	abort();
}

void *loop(void* arg) {
	int nev, fd, newSock;
	struct epoll_event ev[16];
	struct _args loopArgs = *(args)arg;

again:
	nev = epoll_wait(loopArgs.epfd, ev, 16, -1);
	if (nev < 0) {
		if (errno == EINTR) {
			fprintf(stderr, "eintr!!\n");
			goto again;
		} else {
			die("epoll_wait");
		}
	}

	bool state;
	for (int i = 0; i < nev; i++) {
		dataEvent data = (dataEvent)ev[i].data.ptr;
		fd = data->fd;
		if (fd == loopArgs.textSock || fd == loopArgs.binSock) {
			
			newSock = accept(fd, NULL, NULL);
			if(newSock < 0)
				continue;

			fprintf(stderr, "Nuevo Cliente\n");
			if(register_fd(loopArgs.epfd, newSock, data->bin, loopArgs.mc) < 0)
				close(newSock);
			if(modify_fd(loopArgs.epfd, fd, data, loopArgs.mc) < 0)
				die("No se puedo volver a guardar el listener");
		} else {
			if(((dataEvent)ev[i].data.ptr)->bin) {
				state = binary_handler(fd, data->input_state.bin, loopArgs.mc);
			} else {
				state = text_handler(fd, data->input_state.text, loopArgs.mc);
			}
			if(state){
				modify_fd(loopArgs.epfd, fd, data, loopArgs.mc);
			} else {
				if(data->bin) free(data->input_state.bin);
				else free(data->input_state.text);
				free(data);
			}
		}
	}

	goto again;
}


int create_sock(int port) {
	struct sockaddr_in sa;
	int lsock;
	int rc;
	int yes = 1;

	/* Crear socket */
	lsock = socket(AF_INET, SOCK_STREAM, 0);
	if (lsock < 0) die("socket");

	/* Setear opción reuseaddr... normalmente no es necesario */
	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		die("setsockopt");

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	/* Bindear al puerto 'port' TCP, en todas las direcciones disponibles */
	rc = bind(lsock, (struct sockaddr *)&sa, sizeof sa);
	if (rc < 0) die("bind");

	/* Setear en modo escucha */
	rc = listen(lsock, 10);
	if (rc < 0) die("listen");

	return lsock;
}

int main() {

	struct rlimit r = {.rlim_cur = DATA_LIMIT, .rlim_max = RLIM_INFINITY};
	if (setrlimit(RLIMIT_DATA, &r) < 0) {
		die("error limiting data");
	}

	gid_t gid = getgid();
	uid_t uid = getuid();

	if (uid != 0) {
		die("administrator permissions required");
	}
	int sock1, sock2;

	sock1 = create_sock(888);
	sock2 = create_sock(889);

	int epfd = epoll_create(1);
	if (epfd < 0)
		die("epoll");

	Memcached mc;
	if((mc = memcached_create(NUM_OF_NODES)) == NULL)
		die("error allocating the memcached structure");

	/* Registrar los sockets */
	if(register_fd(epfd, sock1, false, mc) < 0) 
		die("error on register text socket");
	
	if(register_fd(epfd, sock2, true, mc) < 0) 
		die("error on register bin socket");

	pthread_t hand[N_THREADS];
	struct _args args;
	args.epfd = epfd;
	args.textSock = sock1;
	args.binSock = sock2;
	args.mc = mc;
	
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_create(&hand[i], NULL, loop, (void *)&args);
	}
	
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_join(hand[i], NULL);
	}

	return 0;
}