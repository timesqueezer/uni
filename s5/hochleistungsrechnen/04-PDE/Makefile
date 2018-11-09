# Common definitions
CC = gcc

# Compiler flags, paths and libraries
CFLAGS = -std=c11 -pedantic -Wall -Wextra -O3 -ggdb
LFLAGS = $(CFLAGS)
LIBS   = -lm

TGTS = partdiff partdiff-openmp
OBJS = partdiff.o askparams.o

# Targets ...
all: partdiff

partdiff-openmp: CFLAGS += -fopenmp
partdiff-openmp: partdiff

partdiff: $(OBJS) Makefile
	$(CC) $(LFLAGS) -o $@ $(OBJS) $(LIBS)

partdiff.o: partdiff.c Makefile

askparams.o: askparams.c Makefile

# Rule to create *.o from *.c
%.o: %.c
	$(CC) -c $(CFLAGS) $*.c

clean:
	$(RM) $(OBJS)
	$(RM) $(TGTS)
