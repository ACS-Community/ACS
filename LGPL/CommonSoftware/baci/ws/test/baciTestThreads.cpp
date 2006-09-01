#include <acsutil.h>
#include <logging.h>
#include <baciThread.h>

#ifdef MAKE_VXWORKS
#	include <acsutilArgUnpack.h>
#endif


 using namespace baci;

LoggingProxy *g_logger = 0;
int iter = 0;

static void initThread(const char * threadName)
{
    // create logging proxy
    if (g_logger)
	LoggingProxy::init(g_logger);
    LoggingProxy::ThreadName(threadName);

    ACS_SHORT_LOG((LM_INFO, "Starting thread '%s' [%llu].", threadName, ACE_Thread_Manager::instance()->thr_self()));
}

static void doneThread()
{
    ACS_SHORT_LOG((LM_INFO, "Stopping thread [%llu].", ACE_Thread_Manager::instance()->thr_self()));
}

static void worker (void* param)
{
    BACIThreadParameter* baciParameter = (BACIThreadParameter*)param;

    BACIThread* myself = baciParameter->getBACIThread();

    if (BACIThread::InitThread) BACIThread::InitThread(myself->getName().c_str());
    
    while (myself->check())
      {
	if (!myself->isSuspended())	// if suspend is not supported by the system
	  {
	    // do something
	  if (iter < 30 ) 
	      {
	    ACS_SHORT_LOG((LM_INFO, "."));
	    ACE_OS::fflush(0);
	    ACE_Thread::yield(); 
	    iter ++;
	      }
	  }
	myself->sleep();
      }

    if (BACIThread::DoneThread) BACIThread::DoneThread();

    delete baciParameter;
    myself->setStopped();
}

int main(int argc, char* argv[])
{

    // create logging proxy
    LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
    LoggingProxy::init(m_logger);
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");

    g_logger = m_logger;

//    printf("b4: setInitializers\n");
    BACIThread::setInitializers(initThread, doneThread);

// printf("b4: threadManager\n");
    BACIThreadManager * threadManager_p = new BACIThreadManager();

    ACS_SHORT_LOG((LM_INFO, "Spawning threads..."));

// printf("b4: threadManager create\n");
    BACIThread * thread = threadManager_p->create("Test thread", 
						  (void*)worker, (void*)0);
    thread->resume();

    ACS_SHORT_LOG((LM_INFO, "Spawned."));
 
    ACE_OS::sleep(5);

    /******* suspend *******/

    ACS_SHORT_LOG((LM_INFO, "Suspending."));

    thread->suspend();

    ACS_SHORT_LOG((LM_INFO, "Suspended."));

    ACE_OS::sleep(5);

    /******* resume *******/

    ACS_SHORT_LOG((LM_INFO, "Resuming."));

    thread->resume();

    ACS_SHORT_LOG((LM_INFO, "Resumed."));

    ACE_OS::sleep(5);

    /******* sleep *******/
/*
    // this will sleep this thread

    ACS_SHORT_LOG((LM_INFO, "Sleeping for default time."));

    thread->sleep();

    ACS_SHORT_LOG((LM_INFO, "Waken up."));

    ACS_SHORT_LOG((LM_INFO, "Sleeping for 3s time."));

    thread->sleep(30000000);

    ACS_SHORT_LOG((LM_INFO, "Waken up."));
*/

    /******* stop *******/

    ACS_SHORT_LOG((LM_INFO, "Stopping."));

    // another way
    threadManager_p->stop("Test thread");

    ACS_SHORT_LOG((LM_INFO, "Stopped."));

    ACE_OS::sleep(5);

    /********************/

    ACS_SHORT_LOG((LM_INFO, "Deleting manager."));

    delete threadManager_p;

    ACS_SHORT_LOG((LM_INFO, "Done."));

    g_logger = 0;
    LoggingProxy::done();
    delete m_logger;

    return 0;
}


#ifdef MAKE_VXWORKS
int startBaciTestThreads(char *szCmdLn)
{
  int  argc;
  char *argv[100];

//  ACE_MAIN_OBJECT_MANAGER
  ACE_OS_Object_Manager ace_os_object_manager;
  ACE_Object_Manager ace_object_manager;


  argc = argUnpack(szCmdLn, argv);
  argv[0] = "baciTestServer";

  int retval = ace_main_i(argc, argv);

   return retval;
}

#endif

/*

#include <pthread.h>
#include <stdio.h>

#define pthread_attr_default  0

typedef void* (*THR_FUNC)(void *);

static void worker (void* ptr)
  {
     char *message;
     message = (char *) ptr;
     printf("%s ", message);
  }


int main()
  {
     pthread_t thread1, thread2;
     const char * message1 = "Hello";
     const char * message2 = "World";

     pthread_create( &thread1, 0,
		    (THR_FUNC)worker, (void*) message1);
     pthread_create(&thread2, 0, 
                    (THR_FUNC)worker, (void*) message2);
  
     return 0;
  }

*/
