#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <mpi.h>


int*
init (int N, int should_add_remainder, int remainder)
{
	int* buf = malloc(sizeof(int) * N);

	srand(time(NULL) + clock() + rand()); // Using only seconds to seed RNG results in identical buffers across processes most of the time

	for (int i = 0; i < N; i++)
	{
		// Set not used items to -1
		if (should_add_remainder)
		{
			if (i >= N - remainder)
				buf[i] = -1;
			else
				buf[i] = rand() % 25;
		}
		else
		{
			buf[i] = rand() % 25;
		}
	}

	return buf;
}

int*
circle (int* buf, int n, int next_rank, int prev_rank)
{
	int* tmp_buf = malloc(sizeof(int) * n);
	// MPI_Request req;
	MPI_Send(buf, n, MPI_INT, next_rank, 0, MPI_COMM_WORLD);
	MPI_Recv(tmp_buf, n, MPI_INT, prev_rank, 0, MPI_COMM_WORLD, MPI_STATUS_IGNORE);

	MPI_Barrier(MPI_COMM_WORLD);

	free(buf);
	return tmp_buf;
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

	// last proc has maybe some "empty" items indicated by value -1
	// when N % nprocs != 0, e.g. 2 procs and N = 5
	int remainder = N % nprocs;
	int N_per_proc = floor(N_per_proc_frac) + remainder;

	// printf("rank %d initializing %d items\n", rank, N_per_proc);
	buf = init(N_per_proc, rank == nprocs - 1 ? 1 : 0, remainder);
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


	if (rank == 0)
	{
		printf("\nBEFORE\n");

		int* buf_other = malloc(sizeof(int) * N_per_proc);
		for (int i = 0; i < N_per_proc; i++)
		{
			if (buf[i] != -1)
				printf("rank %d: %d\n", rank, buf[i]);
		}
		for (int r = 1; r < nprocs; r++)
		{
			MPI_Recv(buf_other, N_per_proc, MPI_INT, r, 0, MPI_COMM_WORLD, &status);
			for (int i = 0; i < N_per_proc; i++)
			{
				if (buf_other[i] != -1)
					printf("rank %d: %d\n", r, buf_other[i]);
			}
		}
		free(buf_other);
	}
	else
	{
		MPI_Send(buf, N_per_proc, MPI_INT, 0, 0, MPI_COMM_WORLD);
	}

	int abort = 0;
	while(1)
	{
		if (rank == nprocs - 1)
		{
			if (buf[0] == first_item_value)
			{
				abort = 1;
				MPI_Bcast(&abort, 1, MPI_INT, nprocs - 1, MPI_COMM_WORLD);
			}
			else
			{
				abort = 0;
				MPI_Bcast(&abort, 1, MPI_INT, nprocs - 1, MPI_COMM_WORLD);
			}
		}
		else
		{
			MPI_Bcast(&abort, 1, MPI_INT, nprocs - 1, MPI_COMM_WORLD);
		}

		if (abort == 1)
		{
			break;
		}

		buf = circle(buf, N_per_proc, next_rank, prev_rank);
	}


	if (rank == 0)
	{
		printf("\nAFTER\n");

		int* buf_other = malloc(sizeof(int) * N_per_proc);
		for (int i = 0; i < N_per_proc; i++)
		{
			if (buf[i] != -1)
				printf("rank %d: %d\n", rank, buf[i]);
		}
		for (int r = 1; r < nprocs; r++)
		{
			MPI_Recv(buf_other, N_per_proc, MPI_INT, r, 0, MPI_COMM_WORLD, &status);
			for (int i = 0; i < N_per_proc; i++)
			{
				if (buf_other[i] != -1)
					printf("rank %d: %d\n", r, buf_other[i]);
			}
		}
		free(buf_other);
	}
	else
	{
		MPI_Send(buf, N_per_proc, MPI_INT, 0, 0, MPI_COMM_WORLD);
	}

	free(buf);

	MPI_Finalize();

	return EXIT_SUCCESS;
}
