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
};

typedef struct _dataEvent *dataEvent;

int register_fd(int epfd, int fd, bool bin, Memcached mc) {
	struct epoll_event ev;
	dataEvent data = custom_malloc(mc->ht, sizeof(struct _dataEvent));
	if(data == NULL) return -1;
	data->bin = bin;
	data->fd = fd;
	ev.events = EPOLLIN | EPOLLONESHOT;
	ev.data.ptr = (void *)data;
	return epoll_ctl(epfd, EPOLL_CTL_ADD, fd, &ev);
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

	// fprintf(stderr, "nev = %i\n", nev);

	bool connection;
	for (int i = 0; i < nev; i++) {
		fd = ((dataEvent)ev[i].data.ptr)->fd;
		// fprintf(stderr, "socket registered. fd %i %i %i\n", loopArgs.textSock, loopArgs.binSock, fd);
		if (fd == loopArgs.textSock || fd == loopArgs.binSock) {
			
			newSock = accept(fd, NULL, NULL);
			if(newSock < 0){
				// fprintf(stderr, "Error on create the socket. FD = %i\n", newSock);
				continue;
			}
			register_fd(loopArgs.epfd, newSock, ((dataEvent)ev[i].data.ptr)->bin, loopArgs.mc);
			modify_fd(loopArgs.epfd, fd, (dataEvent)ev[i].data.ptr, loopArgs.mc);
		} else {
			if(((dataEvent)ev[i].data.ptr)->bin) {
				connection = binary_handler(fd, loopArgs.mc);
			} else {
				connection = text_handler(fd, loopArgs.mc);
			}
			if(connection){
				modify_fd(loopArgs.epfd, fd, (dataEvent)ev[i].data.ptr, loopArgs.mc);
			}
		}
	}

	goto again;
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