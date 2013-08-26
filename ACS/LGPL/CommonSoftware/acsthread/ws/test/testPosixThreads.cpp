#include <stdio.h>
#include <sys/types.h>
#include <pthread.h>
#include <stdlib.h>

#define MAX_THREAD 2000

typedef struct {
	int id;
} parm;

void *hello(void *arg)
{
	parm *p=(parm *)arg;
	printf("Hello from node %d\n", p->id);
	return (NULL);
}

void createThreads(int n, pthread_attr_t *pthread_custom_attr)
{
    int i;
    pthread_t *threads;
    parm *p;

    threads=(pthread_t *)malloc(n*sizeof(*threads));
  
    
    p=(parm *)malloc(sizeof(parm)*n);


    for (i=0; i<n; i++)
	{
	p[i].id=i;
	int ret = pthread_create(&threads[i], pthread_custom_attr, hello, (void *)(p+i));
	printf ("Ret: %d for thread #: %d\n", ret, i);
	}

    for (i=0; i<n; i++)
	{
	pthread_join(threads[i],NULL);
	}
    free(p);

}//creatThreads

int main(int argc, char* argv[]) {
    int n;
    pthread_attr_t pthread_custom_attr;
    size_t stacksize;
    int STACKSIZE =  1024*1024;

    if (argc < 2)
	{
	printf ("Usage: %s threads# [stacksize in kB]\n",argv[0]);
	return 1;
	}

    n = atoi(argv[1]);
    
    if ((n < 1) || (n > MAX_THREAD))
	{
	printf ("The # of thread should between 1 and %d.\n",MAX_THREAD);
	exit(1);
	}
    
    if (n>2)
	STACKSIZE = atoi(argv[2]) * 1024;
	

    pthread_attr_init(&pthread_custom_attr);

    pthread_attr_getstacksize(&pthread_custom_attr, &stacksize);
    printf("Going to create %d threads with default stack size: %d bytes\n", n, stacksize);
    createThreads(n, &pthread_custom_attr);
    printf("----------------------------------------------\n");
    
    pthread_attr_setstacksize(&pthread_custom_attr, STACKSIZE);
    pthread_attr_getstacksize(&pthread_custom_attr, &stacksize);
    printf("Going to create %d threads with stack size: %d bytes\n", n, stacksize);
    createThreads(n, &pthread_custom_attr);
    
    return 0;
}

