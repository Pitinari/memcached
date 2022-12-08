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
#include "memcached.h"

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

int create_sock(char *host, char *port) {
	int sock, ret;
	struct sockaddr_in addr;
	struct addrinfo *gai, hints;

	sock = socket(AF_INET, SOCK_STREAM, 0);
	if (sock < 0)
		die("socket");

	memset(&hints, 0, sizeof hints);
	hints.ai_family = AF_INET;
	hints.ai_socktype = SOCK_STREAM;

	/*
	 * Consultamos la información sobre la dirección que nos
	 * dieron. Podemos pasar una IP, o un nombre que será
	 * consultado a /etc/hosts o al nameserver configurado
	 */
	ret = getaddrinfo(host, port, &hints, &gai);
	if (ret)
		die("getaddrinfo (%s)", gai_strerror(ret));

	/*
	 * getaddrinfo devuelve una lista enlazada con
	 * información, tomamos el primer nodo
	 */

	addr = *(sin*)gai->ai_addr;

	freeaddrinfo(gai);

	/* Conectamos a esa dirección */
	ret = connect(sock, (sad*)&addr, sizeof addr);
	if (ret < 0)
		die("connect");

	return sock;
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

	sock1 = create_sock("1", "888");
	sock2 = create_sock("1", "889");

	int epfd = epoll_create(1);
	if (epfd < 0)
		die("epoll");

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

  Memcached mc = memcached_create();
	loop(epfd, sock1, sock2, mc);

	return 0;
}