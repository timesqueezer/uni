# Common definitions
CC = mpicc

# Compiler flags, paths and libraries
CFLAGS = -std=c11 -pedantic -Wall -Wextra -O3 -ggdb
LFLAGS = $(CFLAGS)
LIBS   = -lm

TGTS = partdiff partdiff-par
OBJS = partdiff.o askparams.o
OBJS_PAR = askparams-par.o displaymatrix-mpi.o partdiff-par.o

# Targets ...
all: partdiff-par

partdiff-par: $(OBJS_PAR) Makefile
	$(CC) $(LFLAGS) -o $@ $(OBJS_PAR) $(LIBS)

partdiff: $(OBJS) Makefile
	$(CC) $(LFLAGS) -o $@ $(OBJS) $(LIBS)

partdiff-par.o: partdiff-par.c Makefile

partdiff.o: partdiff.c Makefile

askparams.o: askparams.c Makefile

# Rule to create *.o from *.c
%.o: %.c
	$(CC) -c $(CFLAGS) $*.c

clean:
	$(RM) $(OBJS)
	$(RM) $(OBJS_PAR)
	$(RM) $(TGTS)
