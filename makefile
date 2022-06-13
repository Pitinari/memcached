FLAGS = -Wall -Wextra -Werror -std=c99 -g -pthread

S = structures/
SL = structure_lock/

concurrent_hashtable.o: $(S)concurrent_hashtable.c $(S)concurrent_hashtable.h $(S)hashtable.h $(SL)rwlock.h hashtable.o rwlock.o
	gcc $(FLAGS) -c $(S)concurrent_hashtable.c

rwlock.o: $(SL)rwlock.c $(SL)rwlock.h $(S)queue.h queue.o
	gcc $(FLAGS) -c $(SL)rwlock.c

hashtable.o: $(S)hashtable.c $(S)hashtable.h
	gcc $(FLAGS) -c $(S)hashtable.c -lm

queue.o: $(S)queue.c $(S)queue.h
	gcc $(FLAGS) -c $(S)queue.c

clean:
	rm *.o
	rm *.out