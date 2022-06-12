FLAGS = -Wall -Wextra -Werror -std=c99 -g -pthread

S = structures/
SL = structure_lock/

rwlock.o: $(SL)rwlock.c $(SL)rwlock.h $(S)queue.h queue.o
	gcc $(FLAGS) -c $(SL)rwlock.c

hashtable.o: $(S)hashtable.c $(S)hashtable.h
	gcc $(FLAGS) -c $(S)hashtable.c -lm

queue.o: $(S)queue.c $(S)queue.h
	gcc $(FLAGS) -c $(S)queue.c

clean:
	rm *.o
	rm *.out