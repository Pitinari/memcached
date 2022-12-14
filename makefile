FLAGS = -std=c99 -g -pthread

# -Wall -Wextra -Werror

S = structures/
U = utils/

start: compile
	sudo ./server

debug: compile
	sudo valgrind ./server

compile: server.o memcached_controller.o memcached_service.o hash_table_with_lru.o hash.o list_with_lru.o
	gcc $(FLAGS) server.o memcached_controller.o memcached_service.o hash_table_with_lru.o hash.o list_with_lru.o -o server

server.o: server.c memcached_controller.h memcached_service.h memcached_controller.o
	gcc $(FLAGS) -c server.c

memcached_controller.o: memcached_controller.c memcached_controller.h memcached_service.h memcached_service.o
	gcc $(FLAGS) -c memcached_controller.c

memcached_service.o: memcached_service.c memcached_service.h $(S)hash_table_with_lru.h hash_table_with_lru.o
	gcc $(FLAGS) -c memcached_service.c

hash_table_with_lru.o: $(S)hash_table_with_lru.c $(S)hash_table_with_lru.h $(S)list_with_lru.h $(U)hash.h list_with_lru.o hash.o
	gcc $(FLAGS) -c $(S)hash_table_with_lru.c -lm

hash.o: $(U)hash.c $(U)hash.h
	gcc $(FLAGS) -c $(U)hash.c

list_with_lru.o: $(S)list_with_lru.c $(S)list_with_lru.h
	gcc $(FLAGS) -c $(S)list_with_lru.c -lm

clean:
	rm *.o
	rm *.out
