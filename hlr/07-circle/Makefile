CC = mpicc

CFLAGS = -std=c11 -ggdb -Wall -Wextra
LIBS = -lm

circle: circle.c Makefile
	$(CC) $(CFLAGS) circle.c -o circle $(LIBS)

clean:
	rm -f circle
