/*
#include <stdio.h>
#include <pthread.h>
  
  
class TestThread
{
public:
    TestThread() {if (_mutex == NULL )
                    {
                        _mutex = new pthread_mutex_t;
                        pthread_mutex_init(_mutex,0);
                    }
                }
    ~TestThread() {}
    void start(int input_parameter);
    static void *entryPoint(void *pthis);
    void *run();
private:
    int _input_parameter;
    pthread_t _thread;
    static pthread_mutex_t* _mutex;
};

 

#include <unistd.h>
#include <stdlib.h>
#include <string.h>
  
int STACKSIZE;
  
pthread_mutex_t* TestThread::_mutex = NULL;
void get_thread_stack_info();
  
int main(int argc, char **argv)
{
    if(argc > 2)
        STACKSIZE = 65536*atoi(argv[2]);
    else
        STACKSIZE = 65536*32;
//    STACKSIZE = 10485760;
    get_thread_stack_info();
    for ( int i = 0; i < atoi(argv[1]); i++)
    {
        TestThread tt;
  
        printf("%i\n",i);
        tt.start(i);
    }
    sleep(10);
}
void get_thread_stack_info()
{
    size_t stacksize;
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_getstacksize(&attr, &stacksize);
    printf("default stacksize: %i\n",stacksize);
    pthread_attr_setstacksize(&attr,STACKSIZE);
    pthread_attr_getstacksize(&attr, &stacksize);
    printf("new stacksize: %i\n",stacksize);
    //printf("PTHREAD_STACK_SIZE: %i\n",PTHREAD_STACK_SIZE);
}
  
void *TestThread::run()
{
  printf("GOT TO thread: %i\n", _input_parameter);
  pthread_mutex_unlock(_mutex);
  sleep(10);
    
  return (void *)0;
    
}
  
void *TestThread::entryPoint(void *pthis)
{
  //fprintf(stdout,"GOT TO entryPoint()\n");
  TestThread *pt = (TestThread*)pthis;
  pt->run();
    
  return (void *)0;
}
  
void TestThread::start(int input_parameter)
{
    pthread_attr_t attr;
    pthread_attr_init(&attr);
    pthread_attr_setstacksize(&attr,STACKSIZE);
    pthread_mutex_lock(_mutex);
    _input_parameter = input_parameter;
    int ret = pthread_create( &_thread, &attr, TestThread::entryPoint, this);
    printf("ret: %i : [%s]\n",ret, strerror(ret));
                      
} 

*/


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

    if (argc != 2)
	{
	printf ("Usage: %s n\n  where n is no. of threads\n",argv[0]);
	return 1;
	}

    n=atoi(argv[1]);
    
    if ((n < 1) || (n > MAX_THREAD))
	{
	printf ("The # of thread should between 1 and %d.\n",MAX_THREAD);
	exit(1);
	}

    pthread_attr_init(&pthread_custom_attr);

    pthread_attr_getstacksize(&pthread_custom_attr, &stacksize);
    printf("Going to create %d threads with default stack size: %d bytes\n", 300, stacksize);
    createThreads(300, &pthread_custom_attr);
    printf("----------------------------------------------\n");
    
    pthread_attr_setstacksize(&pthread_custom_attr, STACKSIZE);
    pthread_attr_getstacksize(&pthread_custom_attr, &stacksize);
    printf("Going to create %d threads with stack size: %d bytes\n", n, stacksize);
    createThreads(n, &pthread_custom_attr);
    
    return 0;
}

