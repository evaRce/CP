#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>

 int MPI_FlattreeColectiva(void * buf, int count, MPI_Datatype datatype, int root, MPI_Comm comm){
     int size , rank;

     MPI_Comm_size(MPI_COMM_WORLD, &size);
     MPI_Comm_rank(MPI_COMM_WORLD, &rank);

     if(rank == root){
         for (int i = 0; i < size; i++) {
             if(i != root)
                MPI_Send(buf,count,datatype,i,0,comm);
         }
     }else{
         MPI_Recv(buf,count,datatype,MPI_ANY_SOURCE,0,comm,MPI_STATUS_IGNORE);
     }
     return MPI_SUCCESS;
}

int MPI_BinomialColectiva(void * buf, int count, MPI_Datatype datatype, int root, MPI_Comm comm){
    int size , myrank;

    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &myrank);

    for(int i=1; pow(2,i-1) <= size; i++){
        //procesos que pueden enviar // procesos que pueden recibir
        if(myrank < pow(2,i-1) && myrank+pow(2,i-1) < size){
            MPI_Send(buf, count, datatype, myrank+pow(2,i-1), 0, comm);
            printf("Iteration %d: From %d to %d\n", i, myrank, (int)(myrank+pow(2,i-1)));
        }

        //if(pow(2,i-1) <=  myrank && myrank < pow(2,i))
        if(myrank >= pow(2,i-1) && myrank < pow(2,i))
            MPI_Recv(buf, count, datatype, myrank-pow(2,i-1), 0, comm, MPI_STATUS_IGNORE);
    }

    return MPI_SUCCESS;




}

int main(int argc, char *argv[])
{
    int i, done = 0, n, count;
    int rank, size;
    double PI25DT = 3.141592653589793238462643;
    double pi, x, y, z,pi_aux;

    MPI_Init(&argc,&argv);
    MPI_Comm_size(MPI_COMM_WORLD,&size);
    MPI_Comm_rank(MPI_COMM_WORLD,&rank);


    while (!done)
    {
        if(rank == 0){
            printf("Enter the number of points: (0 quits) \n");
            scanf("%d",&n);
       }

        //MPI_Bcast(&n,1,MPI_INT,0,MPI_COMM_WORLD);
        MPI_FlattreeColectiva(&n,1,MPI_INT,0,MPI_COMM_WORLD);
        MPI_BinomialColectiva(&n,1,MPI_INT,0,MPI_COMM_WORLD);


        if (n == 0) break;
        count = 0;

        for (i = 1+rank; i <= n; i+=size) {
            // Get the random numbers between 0 and 1
            x = ((double) rand()) / ((double) RAND_MAX);
            y = ((double) rand()) / ((double) RAND_MAX);

            // Calculate the square root of the squares
            z = sqrt((x*x)+(y*y));

            // Check whether z is within the circle
            if(z <= 1.0)
                count++;
        }
        pi = ((double) count/(double) n)*4.0;

        MPI_Reduce(&pi,&pi_aux,1,MPI_DOUBLE,MPI_SUM,0,MPI_COMM_WORLD);

        if(rank == 0)
            printf("pi is approx. %.16f, Error is %.16f\n", pi_aux, fabs(pi_aux - PI25DT));

    }
    MPI_Finalize();

}
