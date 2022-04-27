#include <stdio.h>
#include <sys/time.h>
#include <stdlib.h>
#include <math.h>
#include <mpi.h>

#define DEBUG 1

#define N 1024

int main(int argc, char *argv[] ) {

    int i, j,cnt=0;
    int rank, size,rows;
    //float matrix[N][N];
    //float vector[N];
    //float result[N];
    struct timeval  tv1, tv2,tv3,tv4;


    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   float *matrix,*vector,*result,*matrixAux,*resultAux;
   int  *sendCounts, *displs;


    if (rank == 0){
        matrix =  malloc(N*N*sizeof (float));//Matriz
        matrixAux =malloc(N*(N/size)*sizeof (float)); //Trozos de matriz
        vector = malloc(N*sizeof (float));
        result = malloc((N/size)*sizeof (float));//Trozos del vector resultado
        resultAux = malloc(N*sizeof (float));//Vector resultado completo
        sendCounts = malloc(size*sizeof (int ));
        displs = malloc(size*sizeof (int ));

        /* Initialize Matrix and Vector */
        for(i=0;i<N;i++) {
            vector[i] = i;
            for(j=0;j<N;j++) {
                matrix[i][j] = i+j;
            }
        }

        for (int i = 0; i < size; i++) {
            if(i < (N % size)){ //rank < N mod NP
                rows = (N/size)+1;
            }else{
                rows = N/size;
            }
            sendCounts[i]=rows*N;
            if(i =! 0){
                cnt += sendCounts[i-1];
            }
            displs[i]= cnt * N;
        }

        for (int i = 0; i < size; i++) {
            MPI_Scatterv(matrix,sendCounts,displs,MPI_FLOAT,matrixAux,rows*N,MPI_FLOAT,0,MPI_COMM_WORLD);
        }

        MPI_Bcast(&vector,N,MPI_FLOAT,0,MPI_COMM_WORLD);
    }

    gettimeofday(&tv1, NULL);
    //Modelizar
    for(i=0;i<N;i++) {//i hasta N/size
        result[i]=0;
        for(j=0;j<N;j++) {
            result[i] += matrixAux[i][j]*vector[j];
        }
    }
    gettimeofday(&tv2, NULL);

    if(rank==0){
        for (int i = 0; i < size; i++) {
            if(i < (N % size)){ //rank < N mod NP
                rows = (N/size)+1;
            }else{
                rows = N/size;
            }
            sendCounts[i]=rows*N;
            if(i =! 0){
                cnt += sendCounts[i-1];
            }
            displs[i]= cnt * N;
            MPI_Gatherv(result,rows,MPI_FLOAT,resultAux,sendCounts,displs,MPI_FLOAT,0,MPI_COMM_WORLD);
        }

    }

    int microsecondsA = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);
    //int microsecondsB = (tv4.tv_usec - tv3.tv_usec)+ 1000000 * (tv4.tv_sec - tv3.tv_sec);

    /*Display result */
    if (DEBUG){
        for(i=0;i<N;i++) {
            printf(" %f \t ",resultAux[i]);
        }
    } else {
        printf ("Time (seconds) = %lf\n", (double) microsecondsA/1E6);
        //printf ("Time (seconds) = %lf\n", (double) microsecondsB/1E6);
    }

    MPI_Finalize();
    return 0;

}
