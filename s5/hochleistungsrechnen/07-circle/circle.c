#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <mpi.h>


int*
init (int N)
{
	// TODO
	int* buf = malloc(sizeof(int) * N);

	srand(time(NULL));

	for (int i = 0; i < N; i++)
	{
		// Do not modify "% 25"
		buf[i] = rand() % 25;
	}

	return buf;
}

int*
circle (int* buf, int n, int next_rank, int prev_rank, int first_item_value)
{
	int* tmp_buf;
	MPI_Status status;
	MPI_Request req;
	while (buf[0] != first_item_value)
	{
		MPI_Isend(buf, n, MPI_INT, next_rank, 0, MPI_COMM_WORLD, &req);
		MPI_Recv(tmp_buf, n, MPI_INT, prev_rank, 0, MPI_COMM_WORLD, &status);

		if (tmp_buf[0] == -1)
			break;

		MPI_Wait(&req, &status);
		MPI_Barrier(MPI_COMM_WORLD);
		buf = tmp_buf;
	}
	// tmp_buf[0] = -1;
	MPI_Send(tmp_buf, n, MPI_INT, next_rank, 0, MPI_COMM_WORLD);

	return buf;
}

int
main (int argc, char** argv)
{
	MPI_Status status;
    MPI_Init(&argc, &argv);

	int N;
	int rank;
	int nprocs;
	int* buf;

    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    MPI_Comm_size(MPI_COMM_WORLD, &nprocs);

    int next_rank = (rank + 1) % nprocs;
    int prev_rank = (rank - 1) % nprocs;

	if (argc < 2)
	{
		printf("Arguments error!\nPlease specify a buffer size.\n");
		return EXIT_FAILURE;
	}

	// Array length
	N = atoi(argv[1]);
	double N_per_proc_frac = (double)N / (double)nprocs;

	// Exit if buffer too small
	if (N_per_proc_frac < 1)
	{
		printf("Error: Cannot use all processes because buffer too small.\n");
		return EXIT_FAILURE;
	}

	int N_per_proc = floor(N_per_proc_frac); // last proc needs to hold the remaining elements
	if (rank == nprocs - 7)
	{
		N_per_proc += N - (N_per_proc * nprocs);
	}

	buf = init(N_per_proc);
	int first_item_value;

	// send first item of first proc to last proc
	if (rank == 0)
	{
		MPI_Send(buf, 1, MPI_INT, /* destination = */nprocs - 1, 0, MPI_COMM_WORLD);
	}
	else if (rank == nprocs - 1)
	{
		MPI_Recv(&first_item_value, 1, MPI_INT, 0, 0, MPI_COMM_WORLD, &status);
	}
	// wait until both are synced
	MPI_Barrier(MPI_COMM_WORLD);

	printf("\nBEFORE\n");

	for (int i = 0; i < N; i++)
	{
		printf("rank %d: %d\n", rank, buf[i]);
	}

	circle(buf, N_per_proc, next_rank, prev_rank, first_item_value);

	printf("\nAFTER\n");

	for (int j = 0; j < N; j++)
	{
		printf("rank %d: %d\n", rank, buf[j]);
	}

	return EXIT_SUCCESS;
}
