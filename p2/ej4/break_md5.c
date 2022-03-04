#include <sys/types.h>
#include <openssl/md5.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <pthread.h>
#include <unistd.h>
#include <sys/time.h>
#include <math.h>

#define PASS_LEN 6
#define NUM_THREADS 6

struct shared{
    int *resuelto;
    long cont_compartido;
    char ** original; //passw q se le pasa
    long bound;
    long bound_inf; //limite inferior
    long bound_sup; //limite superior
    long revisado;
    int cont_passw; //contador de contraseñas
    int num_passw;
    pthread_mutex_t mutex_revisado;
    pthread_mutex_t mutex_resuelto;
    pthread_mutex_t mutex_cont_compartido;
    pthread_cond_t cond;
};

//Argumento del thread
struct args {
    int thread_num;
    struct shared * shared;
};

struct thread_info{
    pthread_t id;
    struct args * args;
};


double microsegundos() {
    struct timeval t;
    if (gettimeofday(&t, NULL) < 0)
        return 0.0;
    return (t.tv_usec + t.tv_sec * 1000000.0);
}

long ipow(long base, int exp){
    long res = 1;
    for(;;){
        if (exp & 1)
            res *= base;
        exp >>= 1;
        if (!exp)
            break;
        base *= base;
    }

    return res;
}


long pass_to_long(char *str) {
    long res = 0;

    for(int i=0; i < PASS_LEN; i++)
        res = res * 26 + str[i]-'a';

    return res;
};


void long_to_pass(long n, unsigned char *str) {  // str should have size PASS_SIZE+1
    for(int i=PASS_LEN-1; i >= 0; i--) {
        str[i] = n % 26 + 'a';
        n /= 26;
    }
    str[PASS_LEN] = '\0';
}


int hex_value(char c) {
    if (c>='0' && c <='9')
        return c - '0';
    else if (c>= 'A' && c <='F')
        return c-'A'+10;
    else if (c>= 'a' && c <='f')
        return c-'a'+10;
    else return 0;
}


void hex_to_num(char *str, unsigned char *hex) {
    for(int i=0; i < MD5_DIGEST_LENGTH; i++)
        hex[i] = (hex_value(str[i*2]) << 4) + hex_value(str[i*2 + 1]);
}


int actualizar(int progreso, int total, int parcial){
    int veces = 100;
    int trigger = total / veces;
    if(progreso <= (parcial/trigger)){
        return 1;
    } else {
        return 0;
    }
} 

void * print_progress(void * ptr){
    struct args * args = ptr;
    long total = args->shared->bound;
    char barra [101] = "";
    barra[0] = '\0';
    int parcial = 0;
    long porcentaje = 0;
    pthread_mutex_lock(&args->shared->mutex_revisado);
    
    while(args->shared->cont_passw != args->shared->num_passw){ 
        pthread_cond_wait(&args->shared->cond, &args->shared->mutex_revisado);
        if(actualizar(parcial, total,args->shared->cont_compartido)==1){
            barra[parcial] = '#';
            barra[parcial+1] = '\0';
            porcentaje = ((float) args->shared->cont_compartido/total) * 100;
            printf("\r%72s [%ld%%] %s", "Progress",porcentaje ,barra);
            fflush(stdout);
            parcial++;
        }
    }
    
    pthread_mutex_unlock(&args->shared->mutex_revisado);
    return NULL;
}


long min(long a, long b){
    if(a < b)
        return a;
    return b;
}


void *break_pass(void *ptr) {
    struct args * args = ptr;
    char **argv1 = args->shared->original;
    long i, parcial = 0, cont = 0;
    long casos_a_probar = 1000,
        casos_a_probar_local = 0, 
        casos_restantes = args->shared->bound;
    double t1 = 0, t2 = 0, total = 0;
    int salida = 0, j;
    unsigned char md5_num[MD5_DIGEST_LENGTH];
    unsigned char res[MD5_DIGEST_LENGTH];
    unsigned char *pass = malloc((PASS_LEN + 1) * sizeof(char)); //shared->solucion


    while(args->shared->cont_passw != args->shared->num_passw){
        while(salida == 0){
            pthread_mutex_lock(&args->shared->mutex_cont_compartido);
            //cuando no hay mas posibilidades de pruebas para los threads q intenten coger un rango
            if(casos_restantes == 0){  
                printf("No puedes reservar un rango %ld\n",args->shared->revisado);
                pthread_mutex_unlock(&args->shared->mutex_cont_compartido);
                break;
            }

            casos_a_probar_local = args->shared->cont_compartido;
            //minimo entre casos_a_probar y lo que queda
            args->shared->cont_compartido += min(casos_a_probar, args->shared->bound - args->shared->cont_compartido);

            //casos q van quedando
            casos_restantes = args->shared->bound - args->shared->cont_compartido;

            //rangos del thread
            args->shared->bound_inf = casos_a_probar_local;
            args->shared->bound_sup = args->shared->cont_compartido;
            //printf("(%ld, %ld)\n",args->shared->bound_inf, args->shared->bound_sup);
            salida = 1;
            
            pthread_mutex_unlock(&args->shared->mutex_cont_compartido);
        }

        for(i = args->shared->bound_inf; i < args->shared->bound_sup; i++) {
            if((i == 0 || total >= pow(10,6)) && args->shared->cont_passw != args->shared->num_passw){
                printf("\rCasos : %ld", cont);
                t1 = microsegundos();
                fflush(stdout);
                cont = 0;
            }
            //si los 2 hash q se le pasa por terminal son iguales, descifras solo un passw
            if(strcmp(argv1[0],argv1[1]) == 0){
                args->shared->num_passw-=1;
            }

            long_to_pass(i, pass);
            MD5(pass, PASS_LEN, res);

            for(j = 0; j < args->shared->num_passw; j++){
                hex_to_num(argv1[j], md5_num);
                if(args->shared->resuelto[j] == 0){ //meterlo en el while del rango
                    if(0 == memcmp(res, md5_num, MD5_DIGEST_LENGTH)){
                        pthread_mutex_lock(&args->shared->mutex_resuelto);
                        args->shared->resuelto[j] = 1;
                        args->shared->cont_passw++;
                        printf("\t  %s: %s\n", argv1[j], pass); // Imprimimos la contraseña descodificada
                        pthread_cond_signal(&args->shared->cond);
                        pthread_mutex_unlock(&args->shared->mutex_resuelto);
                        //free(pass);
                        break; // Found it!
                    } 
                }
            }

            t2 = microsegundos();
            total = t2 -t1;

            parcial++; //contador de casos probados
            cont++;

            if(parcial % 2600 == 0){
                pthread_mutex_lock(&args->shared->mutex_revisado);
                args->shared->revisado = args->shared->revisado + parcial; //revisado: contador de casos probados q se reiniciara
                pthread_cond_signal(&args->shared->cond);
                pthread_mutex_unlock(&args->shared->mutex_revisado);
                parcial = 0;
            }

            if(i == (args->shared->bound_sup-1)){
                salida = 0;
                break;
            }
        }

    }
    if(args->shared->cont_passw == args->shared->num_passw)
    free(pass);
    return NULL;
}


struct thread_info * start_threads(struct shared * shared, int num_threads){
    struct thread_info *threads;
    int i = 0;

    threads = malloc(sizeof(struct thread_info) *num_threads);
    if (threads == NULL) {					//Compruebo que se haya reservado correctamente la memoria
		printf("Not enough memory\n");		//Mensaje de error
		exit(1);							//Salida
	}
    
    for(i = 0; i < num_threads; i++){
        if( i == 0){     
            threads[i].args = malloc(sizeof(struct args));
            threads[i].args->thread_num = i;
            threads[i].args->shared = shared;
            if(0 != pthread_create(&threads[i].id, NULL, print_progress, threads[i].args)){
                printf("Could not create thread #%d", i);	//Mensaje de error
                exit(1);									//Salida
            }   
        } else {
            //poner limites: inferior y superior
            threads[i].args = malloc(sizeof(struct args));
            threads[i].args->thread_num = i;
            threads[i].args->shared = shared;
            if(0 != pthread_create(&threads[i].id, NULL, break_pass, threads[i].args)){
                printf("Could not create thread #%d", i);	//Mensaje de error
                exit(1);									//Salida
            }
        }
    }
    
    return threads;
}

void init_shared(struct shared * shared, char * arr[], int num_passw){//passw, char *passw2){
    shared->bound = ipow(26, PASS_LEN); // we have passwords of PASS_LEN
    shared->bound_inf = 0;
    shared->bound_sup = 0;
    shared->cont_compartido = 0;
    shared->resuelto = malloc(sizeof(int) * num_passw);
    shared->revisado = 0;
    shared->original = malloc(sizeof(char *) * 33 * (num_passw)); // hash/0 (33 caracteres)
    //shared->solucion = malloc(sizeof(char *) * (PASS_LEN+1)); //passw/0 (7caracteres)
    shared->cont_passw = 0;
    shared->num_passw = num_passw;
    int i;

    for(i = 0; i < num_passw; i++){
        shared->original[i] = arr[i+1];
        shared->resuelto[i] = 0;
    }

    pthread_mutex_init(&shared->mutex_revisado, NULL);
    pthread_mutex_init(&shared->mutex_resuelto, NULL);
    pthread_mutex_init(&shared->mutex_cont_compartido, NULL);
    pthread_cond_init(&shared->cond, NULL);
}


void wait(struct shared * shared, struct thread_info *threads, int num_threads) {
    int i;
    for(i = 0; i < num_threads; i++)
        pthread_join(threads[i].id, NULL); //finalizara el thread

    for(i = 0; i < num_threads; i++){
        free(threads[i].args);
    }

    pthread_mutex_destroy(&shared->mutex_revisado);
    pthread_mutex_destroy(&shared->mutex_resuelto);
    pthread_mutex_destroy(&shared->mutex_cont_compartido);
    pthread_cond_destroy(&shared->cond);

    free(shared->resuelto);
    free(shared->original);
    free(threads);  //libera memoria
}

int main(int argc, char *argv[]) {
    if(argc < 2) {
        printf("Use: %s string\n", argv[0]); //error si usas menos de 2 arg
        exit(0);
    }
    struct shared shared;
    struct thread_info *thrs;
    int num_threads = NUM_THREADS;

    //Init shared
    init_shared(&shared, argv, argc-1); 

    //Start threads
    thrs = start_threads(&shared, num_threads);

    //wait
    wait(&shared, thrs, num_threads);
    return 0;
}
