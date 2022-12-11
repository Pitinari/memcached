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
#include <pthread.h>

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
};

typedef struct _dataEvent *dataEvent;

#define N_THREADS 10

int register_fd(int epfd, int fd, bool bin) {
	struct epoll_event ev;
	struct _dataEvent type = {bin};
	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.fd = fd;
	ev.data.ptr = (void *)&type;
	return epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &ev);
}

int modify_fd(int epfd, int fd, bool bin) {
	struct epoll_event ev;
	struct _dataEvent type = {bin};
	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.fd = fd;
	ev.data.ptr = (void *)&type;
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

void *loop(void* arg) {
	int nev;
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

	fprintf(stderr, "nev = %i\n", nev);

	for (int i = 0; i < nev; i++) {
		int fd = ev[i].data.fd;

		if (fd == loopArgs.textSock || fd == loopArgs.binSock) {
			int newSock = accept(fd, NULL, NULL);
			register_fd(newSock, loopArgs.epfd, ((dataEvent)ev[i].data.ptr)->bin);
			modify_fd(fd, loopArgs.epfd, ((dataEvent)ev[i].data.ptr)->bin);
		} else {
			if(((dataEvent)ev[i].data.ptr)->bin) {
				char buf[200];
				int n = read(0, buf, 200);
				assert(n > 0);
				/* De vuelta: asumo que no bloquea */
				write(((args)arg)->binSock, buf, n);
				binary(fd, loopArgs.mc);
			} else {
				char buf[200];
				int n = read(0, buf, 200);
				assert(n > 0);
				/* De vuelta: asumo que no bloquea */
				write(((args)arg)->textSock, buf, n);
				text(fd, loopArgs.mc);
			}
		}
	}

	loop((void*)arg);
}

int main() {


	int sock1, sock2;

	sock1 = create_sock(4040);
	sock2 = create_sock(4041);

	int epfd = epoll_create(1);
	if (epfd < 0)
		die("epoll");

	/* Registrar los sockets */
	if(register_fd(epfd, sock1, false) < 0){
		die("error on register text socket");
	}
	if(register_fd(epfd, sock2, true) < 0) {
		die("error on register bin socket");
	}

	pthread_t hand[N_THREADS];
	struct _args args;
	args.epfd = epfd;
	args.textSock = sock1;
	args.binSock = sock2;
	
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_create(&hand[i], NULL, loop, (void *)&args);
	}
	
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_join(hand[i], NULL);
	}

	return 0;
}