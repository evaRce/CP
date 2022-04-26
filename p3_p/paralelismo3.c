#include <stdio.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>

#define DEBUG 1

#define N 1024

int main(int argc, char *argv[] ) {

    int i, j;
    int rank, size,rows,display;
    //float matrix[N][N];
    //float vector[N];
    //float result[N];
    struct timeval  tv1, tv2;


    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   float *matrix,vector,result,*matrixAux,resultAux;

    if (rank == 0){
        matrix =(float *) malloc(N*N*sizeof (float));//Matriz
        matrixAux =(float *) malloc(N*(N/size)*sizeof (float)); //Trozos de matriz
        vector = malloc(N*sizeof (float));
        result = malloc(N*sizeof (float));//Trozos del vector resultado
        resultAux = malloc(N*sizeof (float));//Vector resultado completo

        /* Initialize Matrix and Vector */
        for(i=0;i<N;i++) {
            vector[i] = i;
            for(j=0;j<N;j++) {
                matrix[i][j] = i+j;
            }
        }
        MPI_Bcast(&vector,N,MPI_FLOAT,0,MPI_COMM_WORLD);
    }

    if(rank < (N % size)){ //rank < N mod NP
        rows = (N/size)+1;
    }else{
        rows = N/size;
    }

    if(rank == 0){
        display = 0;
        for (int i = 0; i < size; i++) {
            MPI_Scatterv(matrix,rows*N,display,MPI_FLOAT,matrixAux,rows*N,MPI_FLOAT,0,MPI_COMM_WORLD);
                         display+= rows*N;
        }
    }

    gettimeofday(&tv1, NULL);

    //Modelizar
    for(i=0;i<N;i++) {
        result[i]=0;
        for(j=0;j<N;j++) {
            result[i] += matrixAux[i][j]*vector[j];
        }
    }

    gettimeofday(&tv2, NULL);

    int microseconds = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);

    /*Display result */
    if (DEBUG){
        for(i=0;i<N;i++) {
            printf(" %f \t ",resultAux[i]);
        }
    } else {
        printf ("Time (seconds) = %lf\n", (double) microseconds/1E6);
    }

    MPI_Finalize();
    return 0;

}
