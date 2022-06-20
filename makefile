FLAGS = -Wall -Wextra -Werror -std=c99 -g -pthread

S = structures/
SL = structure_lock/
U = utils/

memcached_service.o: $(S)memcached_service.c $(S)memcached_service.h $(S)hashtable.h $(SL)rwlock.h $(S)double_linked_list.h $(U)hash.h hashtable.o rwlock.o double_linked_list.o hash.o
	gcc $(FLAGS) -c $(S)memcached_service.c

hash.o: $(U)hash.c $(U)hash.h
	gcc $(FLAGS) -c $(U)hash.c

rwlock.o: $(SL)rwlock.c $(SL)rwlock.h $(S)queue.h queue.o
	gcc $(FLAGS) -c $(SL)rwlock.c

hashtable.o: $(S)hashtable.c $(S)hashtable.h $(S)double_linked_list.h double_linked_list.o
	gcc $(FLAGS) -c $(S)hashtable.c -lm

double_linked_list.o: $(S)double_linked_list.c $(S)double_linked_list.h
	gcc $(FLAGS) -c $(S)double_linked_list.c -lm

queue.o: $(S)queue.c $(S)queue.h
	gcc $(FLAGS) -c $(S)queue.c

clean:
	rm *.o
	rm *.out
