#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <time.h>
#include <mpi.h

int*
init (int k,int rest,int rank)
{
  int* buf = malloc(sizeof(int) * (k+1));

  srand(time(NULL));

 if(rest > rank){
    for (int i = 0; i < k+1; i++){
      //DO not modify % 25
      buf[i] = abs((rand()*rank+rand()) % 25);
    }
  }
  else{*/
    for (int i = 0; i < k+1; i++){
      if(i < k)
        // Do not modify % 25
        buf[i] = abs((rand()*rank+rand()) % 25);
      else
        buf[i] = -1;
    }
  }

  return buf;
}

int*
circle (int* buf,int* bufNew,int size,int rank,int k,int rest)
{
  //send last rank to first
  if(rank == size-1)
    MPI_Send(buf,k+rest,MPI_INT,0,0,MPI_COMM_WORLD);
  //other ranks send to next
  else
    MPI_Send(buf,k+rest,MPI_INT,rank+1,0,MPI_COMM_WORLD);
  
  //first rank recieves from last
  if(rank != 0)
    MPI_Recv(bufNew,k+rest,MPI_INT,rank-1,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
  //other ranks recieves from their -1
  else
    MPI_Recv(bufNew,k+rest,MPI_INT,size-1,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE);

  MPI_Barrier(MPI_COMM_WORLD);

  return bufNew;
}

int
main (int argc, char** argv)
{
  MPI_Init(&argc,&argv);
  
  char arg[256];
  int N;
  int rank;
  int size;
  int* buf;
  int* bufNew;
  //last rank needs to hold the first element
  int firstElement;
  //other ranks recieve an abort
  int abort = 0;

  if (argc < 2)
  {
    printf("Arguments error!\n");
    return EXIT_FAILURE;
  }

  sscanf(argv[1], "%s", arg);

  MPI_Comm_size(MPI_COMM_WORLD,&size);
  if(size == 1){
    printf("Nothing to be done for a single Thread!\n");
    return EXIT_SUCCESS;
  }

  MPI_Comm_rank(MPI_COMM_WORLD,&rank);
  // Array length
  N = atoi(arg);

  int k = N/size;
  int rest = N%size;

  buf = init(k,rest,rank);
  bufNew = malloc(sizeof(int) * (k+1));
  rest = 1;

  if(rank == 0)
    MPI_Send(buf,1,MPI_INT,size-1,0,MPI_COMM_WORLD);
  if(rank == size-1)
    MPI_Recv(&firstElement,1,MPI_INT,0,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
 
  //first rank prints out all buffers
  if(rank == 0){
    printf("\nBEFORE\n");
    for (int i = 0; i < k+rest; i++){
      if(buf[i] != -1)
        printf ("rank %d: %d\n", rank, buf[i]);
    }
    for (int j = 1; j < size; j++){
      MPI_Recv(bufNew,k+rest,MPI_INT,j,0,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
      for (int i = 0; i < k+rest; i++){
        if(bufNew[i] != -1)
          printf ("rank %d: %d\n", j, bufNew[i]);
      }
    }
  }
  else
    MPI_Send(buf,k+rest,MPI_INT,0,0,MPI_COMM_WORLD);

  //iterate untill the last rank holds the first element
  while(1){
    if(rank == size-1){
      if(buf[0] == firstElement){
        abort = 1;
        MPI_Bcast(&abort,1,MPI_INT,size-1,MPI_COMM_WORLD);
        break;
      }
      else{
        abort = 0;
        MPI_Bcast(&abort,1,MPI_INT,size-1,MPI_COMM_WORLD);
      }
    }
    else
      MPI_Bcast(&abort,1,MPI_INT,size-1,MPI_COMM_WORLD);
    if(abort == 1)
      break;
    buf = circle(buf,bufNew,size,rank,k,rest);
  }
  
  if(rank == 0){
    printf("\nAFTER\n");
    for (int i = 0; i < k+rest; i++)
    {
      if(buf[i] != -1)
        printf ("rank %d: %d\n", rank, buf[i]);
    }
    
    for (int j = 1; j < size; j++){
      MPI_Recv(bufNew,k+rest,MPI_INT,j,1,MPI_COMM_WORLD,MPI_STATUS_IGNORE);
      for (int i = 0; i < k+rest; i++)
      {
        if(bufNew[i] != -1)
          printf ("rank %d: %d\n", j, bufNew[i]);
      }
    }
  }
  else{
    MPI_Send(buf,k+rest,MPI_INT,0,1,MPI_COMM_WORLD);
  }

  MPI_Finalize();
  return EXIT_SUCCESS;
}
