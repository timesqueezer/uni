/****************************************************************************/
/****************************************************************************/
/**                                                                        **/
/**                 TU München - Institut für Informatik                   **/
/**                                                                        **/
/** Copyright: Prof. Dr. Thomas Ludwig                                     **/
/**            Andreas C. Schmidt                                          **/
/**                                                                        **/
/** File:      partdiff.c                                                  **/
/**                                                                        **/
/** Purpose:   Partial differential equation solver for Gauß-Seidel and    **/
/**            Jacobi method.                                              **/
/**                                                                        **/
/****************************************************************************/
/****************************************************************************/

/* ************************************************************************ */
/* Include standard header file.                                            */
/* ************************************************************************ */
#define _POSIX_C_SOURCE 200809L

#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <inttypes.h>
#include <math.h>
#include <malloc.h>
#include <sys/time.h>
#include <mpi.h>
#include <unistd.h>

#include "partdiff-par.h"

/* ************************************************************************ */
/* Global variables                                                         */
/* ************************************************************************ */

/* time measurement variables */
struct timeval start_time;       /* time when program started                      */
struct timeval comp_time;        /* time when calculation completed                */


/* ************************************************************************ */
/* initVariables: Initializes some global variables                         */
/* ************************************************************************ */
static
void
initVariables (struct calculation_arguments* arguments, struct calculation_results* results, struct options const* options)
{
	MPI_Comm_size(MPI_COMM_WORLD, &arguments->nproc);
	MPI_Comm_rank(MPI_COMM_WORLD, &arguments->rank);
	int const nproc = arguments->nproc;
	int const rank = arguments->rank;
	int rest;

	// sync options
	if (rank == 0) {
		for (int i = 1; i < nproc; i++)
			MPI_Send(options, sizeof(struct options), MPI_CHAR, i, 0, MPI_COMM_WORLD);
	} else {
		MPI_Status status;
		MPI_Recv(options, sizeof(struct options), MPI_CHAR, 0, 0, MPI_COMM_WORLD, &status);
		printf("rank %d received options\n", rank);
	}

	sleep(0.1);
	MPI_Barrier(MPI_COMM_WORLD);

	arguments->N_global = (options->interlines * 8) + 9 - 1;
	rest = (arguments->N_global + 1 - 2) % nproc;

	if(rank < rest)
	{
		arguments->offset = (((arguments->N_global + 1 - 2) / nproc) * rank) + rank;
		arguments->N = ((arguments->N_global + 1 - 2) / nproc) + 2 + 1 - 1;
	}
	else
	{
		arguments->offset = (((arguments->N_global + 1 - 2) / nproc) * rank) + rest;
		arguments->N = ((arguments->N_global + 1 - 2) / nproc) + 2 - 1;
	}

	arguments->num_matrices = (options->method == METH_JACOBI) ? 2 : 1;
	arguments->h = 1.0 / arguments->N_global;

	results->m = 0;
	results->stat_iteration = 0;
	results->stat_precision = 0;
}

/* ************************************************************************ */
/* freeMatrices: frees memory for matrices                                  */
/* ************************************************************************ */
static
void
freeMatrices (struct calculation_arguments* arguments)
{
	uint64_t i;

	for (i = 0; i < arguments->num_matrices; i++)
	{
		free(arguments->Matrix[i]);
	}

	free(arguments->Matrix);
	free(arguments->M);
}

/* ************************************************************************ */
/* allocateMemory ()                                                        */
/* allocates memory and quits if there was a memory allocation problem      */
/* ************************************************************************ */
static
void*
allocateMemory (size_t size)
{
	void *p;

	if ((p = malloc(size)) == NULL)
	{
		printf("Speicherprobleme! (%" PRIu64 " Bytes angefordert)\n", size);
		exit(1);
	}

	return p;
}

/* ************************************************************************ */
/* allocateMatrices: allocates memory for matrices                          */
/* ************************************************************************ */
static
void
allocateMatrices (struct calculation_arguments* arguments)
{
	uint64_t i, j;

	uint64_t const N = arguments->N;
	uint64_t const N_global = arguments->N_global;

	printf("rank: %d, N: %d, N_global: %d, offset: %d\n", arguments->rank, arguments->N, arguments->N_global, arguments->offset);

	arguments->M = allocateMemory(arguments->num_matrices * (N + 1) * (N_global + 1) * sizeof(double));
	arguments->Matrix = allocateMemory(arguments->num_matrices * sizeof(double**));

	for (i = 0; i < arguments->num_matrices; i++)
	{
		arguments->Matrix[i] = allocateMemory((N + 1) * sizeof(double*));

		for (j = 0; j <= N; j++)
		{
			arguments->Matrix[i][j] = arguments->M + (i * (N + 1) * (N_global + 1)) + (j * (N_global + 1));
		}
	}
}

/* ************************************************************************ */
/* initMatrices: Initialize matrix/matrices and some global variables       */
/* ************************************************************************ */
static
void
initMatrices (struct calculation_arguments* arguments, struct options const* options)
{
	uint64_t g, i, j;                                /*  local variables for loops   */

	uint64_t const N = arguments->N;
	uint64_t const N_global = arguments->N_global;
	uint64_t const offset = arguments->offset;
	uint64_t const rank = arguments->rank;
	uint64_t const nproc = arguments->nproc;

	double const h = arguments->h;
	double*** Matrix = arguments->Matrix;

	/* initialize matrix/matrices with zeros */
	for (g = 0; g < arguments->num_matrices; g++)
	{
		for (i = 0; i <= N; i++)
		{
			for (j = 0; j <= N_global; j++)
			{
				Matrix[g][i][j] = 0.0;
			}
		}
	}

	/* initialize borders, depending on function (function 2: nothing to do) */
	if (options->inf_func == FUNC_F0)
	{
		for (g = 0; g < arguments->num_matrices; g++)
		{
			for (i = 0; i < N + 1; i++)
			{
				Matrix[g][i][0] = 1.0 - (h * (i + offset));
				Matrix[g][i][N_global] = h * (i + offset);
			}
		}

		if(rank == 0)
			for (g = 0; g < arguments->num_matrices; g++)
				for (i = 0; i < N_global + 1; i++)
					Matrix[g][0][i] = 1.0 - (h * i);

		if(rank == nproc - 1)
			for (g = 0; g < arguments->num_matrices; g++)
				for (i = 0; i < N_global + 1; i++)
					Matrix[g][N][i] = h * i;

		for (g = 0; g < arguments->num_matrices; g++)
		{
			Matrix[g][N][0] = 0.0;
			Matrix[g][0][N_global] = 0.0;
		}
	}
}

/* ************************************************************************ */
/* calculate: solves the equation                                           */
/* ************************************************************************ */
static
void
calculate_jacobi (struct calculation_arguments const* arguments, struct calculation_results* results, struct options const* options)
{
	int i, j;                                   /* local variables for loops */
	int m1, m2;                                 /* used as indices for old and new matrices */
	double star;                                /* four times center value minus 4 neigh.b values */
	double residuum;                            /* residuum of current iteration */
	double maxresiduum;                         /* maximum residuum value of a slave in iteration */

	int const N = arguments->N;
	int const N_global = arguments->N_global;
	double const h = arguments->h;
	int const nproc = arguments->nproc;
	int const rank = arguments->rank;
	int global_i = 0;

	MPI_Status status;

	double pih = 0.0;
	double fpisin = 0.0;

	int term_iteration = options->term_iteration;

	/* initialize m1 and m2 depending on algorithm */
	if (options->method == METH_JACOBI)
	{
		m1 = 0;
		m2 = 1;
	}
	else
	{
		m1 = 0;
		m2 = 0;
	}

	if (options->inf_func == FUNC_FPISIN)
	{
		pih = PI * h;
		fpisin = 0.25 * TWO_PI_SQUARE * h * h;
	}


	while (term_iteration > 0)
	{
		double** Matrix_Out = arguments->Matrix[m1];
		double** Matrix_In  = arguments->Matrix[m2];

		maxresiduum = 0;

		/* over all rows */
		for (i = 1; i < N; i++)
		{
			double fpisin_i = 0.0;

			if (options->inf_func == FUNC_FPISIN)
			{
				global_i = rank == 0 ? i : arguments->offset + i;
				fpisin_i = fpisin * sin(pih * (double)global_i);
			}

			/* over all columns */
			for (j = 1; j < N_global; j++)
			{
				star = 0.25 * (Matrix_In[i-1][j] + Matrix_In[i][j-1] + Matrix_In[i][j+1] + Matrix_In[i+1][j]);

				if (options->inf_func == FUNC_FPISIN)
				{
					star += fpisin_i * sin(pih * (double)j);
				}

				if (options->termination == TERM_PREC || term_iteration == 1)
				{
					residuum = Matrix_In[i][j] - star;
					residuum = (residuum < 0) ? -residuum : residuum;
					maxresiduum = (residuum < maxresiduum) ? maxresiduum : residuum;
				}

				Matrix_Out[i][j] = star;
			}
		}

		if(rank > 0)
		{
			MPI_Sendrecv(Matrix_Out[1], N_global + 1, MPI_DOUBLE, rank - 1, rank,
						 Matrix_Out[0], N_global + 1, MPI_DOUBLE, rank - 1, rank - 1,
						 MPI_COMM_WORLD, &status);
		}

		if(rank != nproc - 1)
		{
			MPI_Sendrecv(Matrix_Out[N - 1], N_global + 1, MPI_DOUBLE, rank + 1, rank,
						 Matrix_Out[N],     N_global + 1, MPI_DOUBLE, rank + 1, rank + 1,
						 MPI_COMM_WORLD, &status);
		}

		results->stat_iteration++;
		results->stat_precision = maxresiduum;

		/* exchange m1 and m2 */
		i = m1;
		m1 = m2;
		m2 = i;

		MPI_Allreduce(MPI_IN_PLACE, &maxresiduum, 1, MPI_DOUBLE, MPI_MAX, MPI_COMM_WORLD);

		/* check for stopping calculation depending on termination method */
		if (options->termination == TERM_PREC)
		{
			if (maxresiduum < options->term_precision)
			{
				term_iteration = 0;
			}
		}
		else if (options->termination == TERM_ITER)
		{
			term_iteration--;
		}
	}

	results->m = m2;
}

/* ************************************************************************ */
/* calculate: solves the equation using Gauss-Seidel                        */
/* ************************************************************************ */
static
void
calculate_gaussseidel (struct calculation_arguments const* arguments, struct calculation_results *results, struct options* options)
{
    int i, j;                                   /* local variables for loops  */
    int m1, m2;                                 /* used as indices for old and new matrices       */
    double star;                                /* four times center value minus 4 neigh.b values */
    double residuum;                            /* residuum of current iteration                  */
    double maxresiduum;                         /* maximum residuum value of a slave in iteration */
    double maxresiduumbuf;                      /* holds the preceeding maxresiduum               */
    int termflag;                               /* once the last rank is accurate enough to
                                                   terminate by precision, it communicates
                                                   this information using this flag to the
                                                   next process above it.                           */
    int termflag2;                              /* this flag is set and sent down starting from
                                                   rank 0 once rank 0 receives termflag = 1 from a
                                                   process beneath it                               */

    int const N = arguments->N;
    int const N_global = arguments->N_global;
    double const h = arguments->h;
    int const nproc = arguments->nproc;
    int const rank = arguments->rank;

    int term_iteration = options->term_iteration;
    termflag = 0;
    termflag2 = 0;

    /* initialize m1 and m2 depending on algorithm */
    if (options->method == METH_JACOBI)
    {
        m1 = 0;
        m2 = 1;
    }
    else
    {
        m1 = 0;
        m2 = 0;
    }

    while (term_iteration > 0)
    {
        double** Matrix_Out = arguments->Matrix[m1];
        double** Matrix_In  = arguments->Matrix[m2];

        maxresiduum = 0;
        maxresiduumbuf = 0;

        if(rank > 0)
        {
            // receive communication line from above
            MPI_Recv(Matrix_Out[0], N_global + 1, MPI_DOUBLE, rank - 1,
                    rank - 1 + results->stat_iteration, MPI_COMM_WORLD, NULL);

            // receive preceeding maxresiduum
            MPI_Recv(&maxresiduumbuf, 1, MPI_DOUBLE, rank - 1, rank - 1, MPI_COMM_WORLD, NULL);

            // receive final termination flag from above
            MPI_Recv(&termflag2, 1, MPI_DOUBLE, rank - 1, rank - 1, MPI_COMM_WORLD, NULL);
        }

        // in the initial run the first process must not receive values
        if(results->stat_iteration > 0)
        {
            if(rank != nproc - 1)
            {
                // reveive communication line from below
                MPI_Recv(Matrix_Out[N], N_global + 1, MPI_DOUBLE, rank + 1, 
                        rank + 1 + results->stat_iteration - 1, MPI_COMM_WORLD, NULL);

                // receive preliminary termflag from preceeding rank
                MPI_Recv(&termflag, 1, MPI_INT, rank + 1, rank + 1, MPI_COMM_WORLD, NULL);
            }
        }

        /* over all rows */
        for (i = 1; i < N; i++)
        {
            /* over all columns */
            for (j = 1; j < N_global; j++)
            {
                star = 0.25 * (Matrix_In[i-1][j] +
                        Matrix_In[i][j-1] +
                        Matrix_In[i][j+1] +
                        Matrix_In[i+1][j]);

                if (options->inf_func == FUNC_FPISIN)
                {
                    star += (0.25 * TWO_PI_SQUARE * h * h) *
                        sin((PI * h) * ((double)i + arguments->offset)) *
                        sin((PI * h) * (double)j);
                }

                if (options->termination == TERM_PREC || term_iteration == 1)
                {
                    residuum = Matrix_In[i][j] - star;
                    residuum = (residuum < 0) ? -residuum : residuum;
                    maxresiduum = (residuum < maxresiduum) ? maxresiduum : residuum;
                    maxresiduum = (maxresiduumbuf < maxresiduum) ? maxresiduum : maxresiduumbuf;
                }

                Matrix_Out[i][j] = star;
            }
        }

        // in the last iteration the values must not get sent upwards, this lets the pipeline run out
        if(term_iteration > 1 && termflag2 != 1)
        {
            if(rank > 0)
            {
                // send communication line upwards
                MPI_Send(Matrix_Out[1], N_global + 1, MPI_DOUBLE, rank - 1,
                        rank + results->stat_iteration, MPI_COMM_WORLD);

                // send prelimary termination flag upwards
                MPI_Send(&termflag, 1, MPI_INT, rank - 1, rank, MPI_COMM_WORLD);
            }
        }

        if(rank != nproc - 1)
        {
            // send communication line downwards
            MPI_Send(Matrix_Out[N - 1], N_global + 1, MPI_DOUBLE, rank + 1, 
                    rank + results->stat_iteration, MPI_COMM_WORLD);

            // send maxresiduum down the procs
            MPI_Send(&maxresiduum, 1, MPI_DOUBLE, rank + 1, rank, MPI_COMM_WORLD);

            // send final termflag downwards
            MPI_Send(&termflag2, 1, MPI_DOUBLE, rank + 1, rank, MPI_COMM_WORLD);
        }

        /* exchange m1 and m2 */
        i = m1;
        m1 = m2;
        m2 = i;

        results->stat_iteration++;
        results->stat_precision = maxresiduum;

        if(termflag2 == 1)
            term_iteration = 0;

        // if we receive the termflag at the top of our process stack (rank 0), set termflag2 to 1
        // so the stack terminates
        if(rank == 0)
            if(termflag == 1)
            {
                termflag2 = 1;
            }

        /* check for stopping calculation, depending on termination method */
        if (options->termination == TERM_PREC)
        {
            if (rank == nproc - 1) {
                if (maxresiduum < options->term_precision && termflag != 1)
                {
                    termflag = 1;
                }
            }
        }
        else if (options->termination == TERM_ITER)
        {
            term_iteration--;
        }

    }

    results->m = m2;
}

/* ************************************************************************ */
/*  displayStatistics: displays some statistics about the calculation       */
/* ************************************************************************ */
static
void
displayStatistics (struct calculation_arguments const* arguments, struct calculation_results const* results, struct options const* options)
{
	int N = arguments->N;
	double time = (comp_time.tv_sec - start_time.tv_sec) + (comp_time.tv_usec - start_time.tv_usec) * 1e-6;

	printf("Berechnungszeit:    %f s \n", time);
	printf("Speicherbedarf:     %f MiB\n", (N + 1) * (N + 1) * sizeof(double) * arguments->num_matrices / 1024.0 / 1024.0);
	printf("Berechnungsmethode: ");

	if (options->method == METH_GAUSS_SEIDEL)
	{
		printf("Gauß-Seidel");
	}
	else if (options->method == METH_JACOBI)
	{
		printf("Jacobi");
	}

	printf("\n");
	printf("Interlines:         %" PRIu64 "\n",options->interlines);
	printf("Stoerfunktion:      ");

	if (options->inf_func == FUNC_F0)
	{
		printf("f(x,y) = 0");
	}
	else if (options->inf_func == FUNC_FPISIN)
	{
		printf("f(x,y) = 2pi^2*sin(pi*x)sin(pi*y)");
	}

	printf("\n");
	printf("Terminierung:       ");

	if (options->termination == TERM_PREC)
	{
		printf("Hinreichende Genaugkeit");
	}
	else if (options->termination == TERM_ITER)
	{
		printf("Anzahl der Iterationen");
	}

	printf("\n");
	printf("Anzahl Iterationen: %" PRIu64 "\n", results->stat_iteration);
	printf("Norm des Fehlers:   %e\n", results->stat_precision);
	printf("\n");
}

/* ************************************************************************ */
/*  main                                                                    */
/* ************************************************************************ */
int
main (int argc, char** argv)
{
	MPI_Init(&argc, &argv);

	struct options options;
	struct calculation_arguments arguments;
	struct calculation_results results;

	AskParams(&options, argc, argv);

	initVariables(&arguments, &results, &options);

	allocateMatrices(&arguments);
	initMatrices(&arguments, &options);

	gettimeofday(&start_time, NULL);
	calculate_gaussseidel(&arguments, &results, &options);
	MPI_Barrier(MPI_COMM_WORLD);
	gettimeofday(&comp_time, NULL);

	if (arguments.rank == 0)
		displayStatistics(&arguments, &results, &options);

	// printDebug(&arguments, &results);

	DisplayMatrix(&arguments, &results, &options,
	              arguments.rank, arguments.nproc,
	              arguments.offset + ((arguments.rank > 0) ? 1 : 0),
	              (arguments.offset + arguments.N - 1));

	freeMatrices(&arguments);

	MPI_Finalize();

	return 0;
}
