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

typedef struct sockaddr_in sin;
typedef struct sockaddr    sad;

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

int create_sock(char *host, char *port)
{
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

#if QUIERO_PELEARME_CON_LA_GENTE_Y_SACARLE_MALA_FAMA_A_GOTO
int f()
{
	a = malloc();
	if (!a)
		return -1;

	b = malloc();
	if (!b)
		goto fail2;

	c = malloc();
	if (!c)
		goto fail1;

	return algo(a,b,c);

fail1:
	free(b);
fail2:
	free(a);
	return -1;
}
#endif

void loop(int epfd, int sock)
{
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

		if (fd == 0) {
			char buf[200];
			int n = read(0, buf, 200);
			assert(n > 0);
			/* De vuelta: asumo que no bloquea */
			write(sock, buf, n);
		} else if (fd == sock) {
			char buf[200];
			int n = read(sock, buf, 200);
			assert(n > 0);
			write(1, buf, n);
		} else {
			die("otro fd??");
		}
	}

	loop(epfd, sock);
}

int main(int argc, char **argv)
{
	int sock;

	if (argc != 3)
		abort();

	sock = create_sock(argv[1], argv[2]);

	int epfd = epoll_create(1);
	if (epfd < 0)
		die("epoll");

	/* Registrar el socket */
	{
		struct epoll_event ev;
		ev.events = EPOLLIN;
		ev.data.fd = sock;
		int rc = epoll_ctl(epfd, EPOLL_CTL_ADD, sock, &ev);
		if (rc < 0)
			die("epoll ctl");
	}

	/* Registrar stdin */
	{
		struct epoll_event ev;
		ev.events = EPOLLIN;
		ev.data.fd = 0;
		int rc = epoll_ctl(epfd, EPOLL_CTL_ADD, 0, &ev);
		if (rc < 0)
			die("epoll ctl");
	}

	loop(epfd, sock);

	return 0;
}
