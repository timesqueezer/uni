# Common definitions
CC = mpicc

# Compiler flags, paths and libraries
CFLAGS = -std=c99 -pedantic -Wall -Wextra -O2 -ggdb -fopenmp
LFLAGS = $(CFLAGS)
LIBS   = -lm

TGTS = partdiff partdiff-par partdiff-par-hybrid
OBJS = partdiff.o askparams.o
OBJSMP = displaymatrix-mpi.o askparams-par.o partdiff-par-hybrid.o 
OBJS_PAR = askparams-par.o displaymatrix-mpi.o partdiff-par.o

# Targets ...
all: partdiff-par partdiff partdiff-par-hybrid

partdiff-par: $(OBJS_PAR) Makefile
	$(CC) $(LFLAGS) -o $@ $(OBJS_PAR) $(LIBS)

partdiff: $(OBJS) Makefile
	$(CC) $(LFLAGS) -o $@ $(OBJS) $(LIBS)
	
partdiff-par-hybrid: $(OBJSMP) Makefile
	$(CC) $(LFLAGS) -o $@ $(OBJSMP) $(LIBS)

partdiff-par.o: partdiff-par.c Makefile

partdiff-par-hybrid.o: partdiff-par-hybrid.c Makefile

partdiff.o: partdiff.c Makefile

askparams.o: askparams.c Makefile

# Rule to create *.o from *.c
%.o: %.c
	$(CC) -c $(CFLAGS) $*.c

clean:
	$(RM) $(OBJS)
	$(RM) $(OBJS_PAR)
	$(RM) $(TGTS)
