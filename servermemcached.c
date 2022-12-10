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
#include "memcached_controller.h"

typedef struct sockaddr_in sin;
typedef struct sockaddr    sad;

static void die(char *s, ...) {
	va_list v;

	va_start(v, s);
	vfprintf(stderr, s, v);
	fprintf(stderr, "\n");
	va_end(v);
	fprintf(stderr, " -- errno = %i (%m)\n", errno);

	fflush(stderr);
	abort();
}

int create_sock(char *port) {
	struct sockaddr_in sa;
	int lsock;
	int rc;
	int yes = 1;

	/* Crear socket */
	lsock = socket(AF_INET, SOCK_STREAM, 0);
	if (lsock < 0) quit("socket");

	/* Setear opción reuseaddr... normalmente no es necesario */
	if (setsockopt(lsock, SOL_SOCKET, SO_REUSEADDR, &yes, sizeof yes) == 1)
		quit("setsockopt");

	sa.sin_family = AF_INET;
	sa.sin_port = htons(port);
	sa.sin_addr.s_addr = htonl(INADDR_ANY);

	/* Bindear al puerto 4040 TCP, en todas las direcciones disponibles */
	rc = bind(lsock, (struct sockaddr *)&sa, sizeof sa);
	if (rc < 0) quit("bind");

	/* Setear en modo escucha */
	rc = listen(lsock, 10);
	if (rc < 0) quit("listen");

	return lsock;
}

void loop(int epfd, int sock1, int sock2, Memcached mc) {
	int nev;
	struct epoll_event ev[16];

again:
	nev = epoll_wait(epfd, ev, 16, -1);
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

		if (fd == sock1) {
			text(sock1, mc);
		} else if (fd == sock2) {
			binary(sock2, mc);
		} else {
			die("otro fd??");
		}
	}

	loop(epfd, sock1, sock2, mc);
}

int main() {
	int sock1, sock2;

	sock1 = create_sock("888");
	sock2 = create_sock("889");

	int epfd = epoll_create(1);
	if (epfd < 0) die("epoll");

	/* Registrar los sockets */
	{
		struct epoll_event ev1;
		ev1.events = EPOLLIN;
		ev1.data.fd = sock1;
		int rc = epoll_ctl(epfd, EPOLL_CTL_ADD, sock1, &ev1);
		if (rc < 0)
			die("epoll ctl");
	}
  	{
		struct epoll_event ev2;
		ev2.events = EPOLLIN;
		ev2.data.fd = sock2;
		int rc = epoll_ctl(epfd, EPOLL_CTL_ADD, sock2, &ev2);
		if (rc < 0)
			die("epoll ctl");
	}

	/* Registrar stdin */
	{
		struct epoll_event ev1;
		ev1.events = EPOLLIN;
		ev1.data.fd = 0;
		int rc = epoll_ctl(epfd, EPOLL_CTL_ADD, 0, &ev1);
		if (rc < 0)
			die("epoll ctl");
	}
	{
		struct epoll_event ev2;
		ev2.events = EPOLLIN;
		ev2.data.fd = 0;
		int rc = epoll_ctl(epfd, EPOLL_CTL_ADD, 0, &ev2);
		if (rc < 0)
			die("epoll ctl");
	}

  	Memcached mc = memcached_create(10000);
	loop(epfd, sock1, sock2, mc);

	return 0;
}