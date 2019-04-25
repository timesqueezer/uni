#include <stdio.h>
#include <mpi.h>
#include <time.h>
#include <unistd.h>

int main(int argc, char **argv)
{
	int my_id , num_processes;
	MPI_Status status;
	MPI_Init(&argc, &argv);
	MPI_Comm_rank(MPI_COMM_WORLD, &my_id);
	MPI_Comm_size(MPI_COMM_WORLD, &num_processes);

	if (my_id == 0)
	{
		char buff[100];
		for (int i = 1; i < num_processes; i++)
		{
		MPI_Recv(buff, 100, MPI_CHAR, i, 0, MPI_COMM_WORLD, &status);
			printf("%s\n", buff);
		}
	}
	else
	{
		char hostname[50];
		char time[50];
		char buff[100];
		struct timespec ts;

		gethostname(hostname, 50);
		timespec_get(&ts, TIME_UTC);

		strftime(time, sizeof(time), "%F %T", localtime(&ts.tv_sec));
		sprintf(buff, "%s: %s.%09ld", hostname, time, ts.tv_nsec);

		MPI_Send(buff, 100, MPI_CHAR, 0, 0, MPI_COMM_WORLD);
	}

	MPI_Barrier(MPI_COMM_WORLD);

	printf("Rang %d beendet jetzt!\n", my_id);
	MPI_Finalize();
}