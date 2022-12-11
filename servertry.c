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
	int sock;
	int mode;
};
typedef struct _args *args;

#define N_THREADS 10

/*
 * Para probar, usar netcat. Ej:
 *
 *      $ nc localhost 4040
 *      NUEVO
 *      0
 *      NUEVO
 *      1
 *      CHAU
*/

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

again:
	nev = epoll_wait(((args)arg)->epfd, ev, 16, -1);
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

		if (fd == 0) {
			char buf[200];
			int n = read(0, buf, 200);
			assert(n > 0);
			/* De vuelta: asumo que no bloquea */
			write(((args)arg)->sock, buf, n);
		} else if (fd == ((args)arg)->sock) {
			if (((args)arg)->mode == 0) {
				/*text(fd, table)*/
			}
			else {
				/*binary(fd, table)*/
			}
		} else {
			die("otro fd??");
		}
	}

	loop((void*)arg);
}

int main() {
	int sock1, sock2;

	sock1 = create_sock(888);
	sock2 = create_sock(889);

	int epfd1 = epoll_create(1);
	if (epfd1 < 0)
		die("epoll");

	int epfd2 = epoll_create(1);
	if (epfd2 < 0)
		die("epoll");

	/* Registrar los sockets */
	{
		struct epoll_event ev;
		ev.events = EPOLLIN;
		ev.data.fd = sock1;
		int rc = epoll_ctl(epfd1, EPOLL_CTL_ADD, sock1, &ev);
		if (rc < 0)
			die("epoll ctl");
	}
	{
		struct epoll_event ev;
		ev.events = EPOLLIN;
		ev.data.fd = sock2;
		int rc = epoll_ctl(epfd2, EPOLL_CTL_ADD, sock2, &ev);
		if (rc < 0)
			die("epoll ctl");
	}

	/* Registrar stdin */
	{
		struct epoll_event ev;
		ev.events = EPOLLIN;
		ev.data.fd = 0;
		int rc = epoll_ctl(epfd1, EPOLL_CTL_ADD, 0, &ev);
		if (rc < 0)
			die("epoll ctl");
	}
	{
		struct epoll_event ev;
		ev.events = EPOLLIN;
		ev.data.fd = 0;
		int rc = epoll_ctl(epfd2, EPOLL_CTL_ADD, 0, &ev);
		if (rc < 0)
			die("epoll ctl");
	}

	pthread_t hand[N_THREADS*2];
	args args1, args2;
	args1->epfd = epfd1;
	args2->epfd = epfd2;
	args1->sock = sock1;
	args2->sock = sock2;
	args1->mode = 0;
	args2->mode = 1;
	
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_create(&hand[i], NULL, loop, (void *)args1);
	}
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_create(&hand[N_THREADS+i], NULL, loop, (void *)args2);
	}
	
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_join(hand[i], NULL);
	}
	for (size_t i = 0; i < N_THREADS; i++) {
		pthread_join(hand[N_THREADS +i], NULL);
	}

	return 0;
}