CC = mpicc

CFLAGS = -std=c11 -ggdb -Wall -Wextra

circle: circle.c Makefile
	$(CC) $(CFLAGS) circle.c -o circle

clean:
	rm -f circle
