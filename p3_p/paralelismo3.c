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
    struct timeval  tv1, tv2,tv3,tv4,tv5,tv6;


    MPI_Init(&argc, &argv);
    MPI_Comm_size(MPI_COMM_WORLD, &size);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

   float *matrix,*vector,*result,*matrixAux,*resultAux;
   int  *sendCounts, *displs;

    
    matrixAux = malloc(N * ((N/size)+(N%size)) * sizeof (float)); //Trozos de matriz
    vector = malloc(N*sizeof (float));
    resultAux = malloc(N*sizeof (float));//Vector resultado completo

    matrix = malloc(N*N*sizeof (float));//Matriz
    result = malloc(((N/size)+(N%size)) * sizeof (float));//Trozos del vector resultado
    sendCounts = malloc(size*sizeof (int ));
    displs = malloc(size*sizeof (int ));

    if (rank == 0){
        /* Initialize Matrix and Vector */
        for(i=0;i<N;i++) {
            vector[i] = i;
            for(j=0;j<N;j++) {
                matrix[i*N+j] = i+j;
            }
        }
    }

    

    for (int i = 0; i < size; i++) {
        if(i < (N % size)){ //rank < N mod NP   i=0 10%3=1  0<1 si   i=1 10%3=1 1<1  no   i=2  10%3=1  2<1  no
            rows = (N/size)+1;                   //rows= 4           //rows=3              //rows=3
        }else{
            rows = N/size;
        }
        sendCounts[i]=rows*N;
        if(i =! 0){
            cnt += sendCounts[i-1];
        }
        displs[i]= cnt * N;
    }

    //1er tiempo de comunicación
    gettimeofday(&tv1, NULL);
    MPI_Scatterv(matrix, sendCounts, displs, MPI_FLOAT, matrixAux, sendCounts[rank], MPI_FLOAT, 0, MPI_COMM_WORLD);
    MPI_Bcast(&vector, N, MPI_FLOAT, 0, MPI_COMM_WORLD);
    gettimeofday(&tv2, NULL);


    gettimeofday(&tv3, NULL);
    //Modelizar
    for(i=0;i<((N/size)+(N%size));i++) {//i hasta N/size
        result[i]=0;
        for(j=0;j<N;j++) {
            result[i] += matrixAux[i*N+j]*vector[j];
        }
    }
    gettimeofday(&tv4, NULL);

    for (int i = 0; i < size; i++) {
        if(i < (N % size)){ //rank < N mod NP   i=0 10%3=1  0<1 si   i=1 10%3=1 1<1  no   i=2  10%3=1  2<1  no
            rows = (N/size)+1;                   //rows= 4           //rows=3              //rows=3
        }else{
            rows = N/size;
        }
        sendCounts[i]=rows;
        if(i =! 0){
            cnt += sendCounts[i-1];
        }
        displs[i]= cnt;
    }
    

    gettimeofday(&tv5, NULL);
    MPI_Gatherv(result,rows,MPI_FLOAT,resultAux,sendCounts[rank],displs[rank],MPI_FLOAT,0,MPI_COMM_WORLD);
    gettimeofday(&tv6, NULL);
        

    int microsecondsA = (tv2.tv_usec - tv1.tv_usec)+ 1000000 * (tv2.tv_sec - tv1.tv_sec);
    int microsecondsB = (tv4.tv_usec - tv3.tv_usec)+ 1000000 * (tv4.tv_sec - tv3.tv_sec);
    int microsecondsC = (tv6.tv_usec - tv5.tv_usec)+ 1000000 * (tv6.tv_sec - tv5.tv_sec);

    double tiemposComp[size];
    double tiemposComm[size];
    double tiempoComp = (double) (microsecondsA + microsecondsC)/1E6;
    double tiempoComm = (double) microsecondsB/1E6;

    //recolecta en un array todos los tiempos de computacion y en otro array todos los tiempos de comunicacion
    MPI_Gather(&tiempoComp, 1, MPI_DOUBLE, &tiemposComp[rank], 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);
    MPI_Gather(&tiempoComm , 1, MPI_DOUBLE, &tiemposComm[rank], 1, MPI_DOUBLE, 0, MPI_COMM_WORLD);


    /*Display result */
    if (DEBUG){
        for(i=0;i<N;i++) {
            printf(" %f \t ",resultAux[i]);
        }
    } else {
        for(i = 0; i<size; i++){
            fprintf (stderr, "(Rank: %d) Time Computing (seconds) = %lf  Time Comunicating (seconds) = %lf\n", i, tiemposComp[i], tiemposComm[i]);
        }
    }

    free(matrix);
    free(matrixAux);
    free(vector);
    free(result);
    free(resultAux);
    free(sendCounts);
    free(displs);
    MPI_Finalize();
    return 0;

}
