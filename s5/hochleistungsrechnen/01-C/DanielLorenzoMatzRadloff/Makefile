.PHONY: pointer pointer.o map map.o all clean

all: pointer map

pointer: pointer.o
	gcc -o pointer pointer.o

pointer.o: pointer.c
	gcc -c pointer.c

map: map.o
	gcc -o map map.o

map.o: map.c
	gcc -c map.c

clean:
	rm -f pointer
	rm -f map
	rm -f *.o
	rm -f *~
