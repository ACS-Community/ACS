#include <acsThreadManager.h>


class testJoinableThread : public ACS::Thread {
public:
    testJoinableThread(const ACE_CString& name, const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime,
		       const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime, bool del=false) :
	ACS::Thread(name, responseTime*10, sleepTime*50, del, THR_NEW_LWP| THR_JOINABLE)
	{
	    ACS_TRACE("testJoinableThread::testJoinableThread");
	}

    ~testJoinableThread()
	{
	    ACS_TRACE("testJoinableThread::~testJoinableThread");
	}

    virtual void run()
	{
    	ACS_SHORT_LOG(( LM_INFO, " run()"));
	    sleep();
	}
};

int main(int argc, char *argv[])
{

    int numThread = atoi(argv[1]);
    ACS::ThreadManager tm;
    testJoinableThread **threads = new testJoinableThread*[numThread];
    ACE_thread_t *threadId = new ACE_thread_t[numThread];

    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);


    for (int i=0; i<numThread; i++)
	{
	std::stringstream str_thread;
	str_thread << "thread " << i;
	threads[i] = tm.create<testJoinableThread>(str_thread.str().c_str());
	threads[i]->resume();
	threadId[i] = threads[i]->getThreadID();
	}//for
    ACS_SHORT_LOG((LM_INFO, "# of threads in the thread manager: %d\n", tm.getThreadCount()));
    for (int i=0; i<numThread; i++)
	{
	ACS_SHORT_LOG((LM_INFO, "joining thread # %d\n", i));
	int st = tm.join(threadId[i]);

	if (st==-1)cout<<"thread "<<i<<" join fails"<<endl;
	ACS_SHORT_LOG((LM_INFO, "=> thread # %d joint with ret: %d\n", i , st));
	delete threads[i];
	}//for

    ACS_SHORT_LOG((LM_INFO, "# of threads in the thread manager after joining: %d\n", tm.getThreadCount()));


    delete[] threads;
    delete[] threadId;

    LoggingProxy::done();

    sleep(1);
    return 0;
}

/*___oOo___*/
