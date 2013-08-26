/*
 * "@(#) $Id: acstestcompTimingExplorerImpl.cpp,v 1.4 2008/10/09 08:07:23 cparedes Exp $"
 *
 * $Log: acstestcompTimingExplorerImpl.cpp,v $
 * Revision 1.4  2008/10/09 08:07:23  cparedes
 * Remove cpp exception declarations
 *
 * Revision 1.3  2008/10/01 05:33:43  cparedes
 * Removing exception declaration from cpp implementation
 *
 * Revision 1.2  2007/01/08 16:00:06  gchiozzi
 * Replaced obsolete (and incomplete) ACS Completion initiallization with
 * initialization from construction of completion generated type.
 *
 * Revision 1.1  2006/09/28 16:02:57  gchiozzi
 * Added second test component.
 * Some cleanup and refactoring.
 *
 * Revision 1.1  2006/09/14 14:54:34  gchiozzi
 * First checkin of new module with CPP test components.
 *
 *
 */

#include <acsutilTimeStamp.h>
#include <acstestcompTimingExplorerImpl.h>
#include <memory>
#include <ACSErrTypeCommon.h>
#include <ACSErrTypeOK.h>

/**
 * This structure is used to pass data to the thread.
 * A class with private data and eccessors would be cleaner and
 * and safer, but we use a structure
 * to make it smaller for the test. 
 * The important thing here is to have a constructor
 * that duplicates the callback object
 * and a destructor that does the release at the end.
 * This is essential for proper memory management.
 */
struct ThreadData
{
    ThreadData(const int _iterations, const ACS::CBvoid_ptr _cb, const ACS::CBDescIn _desc):
	iterations(_iterations), 
	cb(ACS::CBvoid::_duplicate(_cb)),
	desc(_desc) {};

    ~ThreadData() {
	CORBA::release(cb);
    }
    const int iterations;
    const ACS::CBvoid_ptr cb;
    const ACS::CBDescIn desc;
}; 

/* -------------------------- [ MultipleReplyThread ] -------------------------- */

/**
 * This is the thread class to handle multiple replies
 * The runLoop method is called automatically at the desired frequency
 * util we detect that this is the last iteration.
 */

class MultipleReplyThread :public ACS::Thread
{
  public:
    /**
     * ThreadData must be allocated by the caller, but then
     * its responsibility is transferred to the Thread object
     * that will take care of deallocating it (using auto pointers).
     */
    MultipleReplyThread(const ACE_CString& name, 
			ThreadData *threadData, 
			const ACS::TimeInterval& responseTime=ACS::ThreadBase::defaultResponseTime, 
			const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
			bool del=true) :
	ACS::Thread(name, responseTime, sleepTime, del),
	threadData_m(threadData),
	iterationsDone_m(0)
	{
	    ACS_LOG(LM_FULL_INFO,"MultipleReplyThread::MultipleReplyThread",
		    (LM_INFO,"Creating thread to run %d iterations", 
		     threadData_m->iterations)); 

	}

    virtual void runLoop()
	{
	    ACS_TRACE("MultipleReplyThread::runLoop");

	    ACSErr::Completion completion;
	    ACS::CBDescOut descOut;
	    
	    try
		{
		completion = ACSErrTypeOK::ACSErrOKCompletion();
		
		if(iterationsDone_m < threadData_m->iterations)
		    {
		    iterationsDone_m++;
		    ACS_LOG(LM_FULL_INFO,"MultipleReplyThread::runLoop",
			    (LM_INFO,"Running iteration %d/%d of the loop", 
			     iterationsDone_m, threadData_m->iterations)); 
		    threadData_m->cb->working(completion, descOut);
		    }
		else
		    {
		    ACS_LOG(LM_FULL_INFO,"MultipleReplyThread::runLoop",
			    (LM_INFO,"Running iteration %d/%d of the loop (the last one)", 
			     iterationsDone_m, threadData_m->iterations)) ;
		    threadData_m->cb->done(completion, descOut);

                    // Here I tell the thread that it has to exit.
		    exit();
		    }
		}
	    /*
	     * I catch here exceptions as ACS exceptions or as
	     * unsexpected exceptions, to be sure
	     * I can catch any possibility.
	     * Here in the thread function I can reasonably 
	     * only log the exception and exit from the thread
	     */
	    catch(ACSErrTypeCommon::GenericErrorExImpl &ex)
		{
		ACSErrTypeCommon::GenericErrorExImpl ex2(ex, 
							 __FILE__, __LINE__, 
							 "MultipleReplyThread::runLoop");
		ex2.setErrorDesc("Error calling callback");
		ex2.log();
		exit();
		}
	    catch(CORBA::SystemException &ex)
		{
		ACSErrTypeCommon::GenericErrorExImpl ex2(
		    __FILE__, __LINE__,
		    "MultipleReplyThread::runLoop");
		ex2.setErrorDesc("Trying to call the callback has thrown a CORBA exception");
		ex2.log();
		exit();
		}
	    catch(...)
		{
		ACSErrTypeCommon::UnexpectedExceptionExImpl ex2(__FILE__, __LINE__, 
								"MultipleReplyThread::runLoop");
		ex2.log();
		exit();
		}
	};  /* End runLoop() */

  protected:
    auto_ptr<ThreadData> threadData_m;
    int iterationsDone_m;
};

/* -------------------------- [ TimingExplorerImpl ] -------------------------- */

TimingExplorerImpl::TimingExplorerImpl(const ACE_CString& name, 
				       maci::ContainerServices* containerServices)
    : ACSComponentImpl(name, containerServices),
      multipleRepliesCounter_m(0)
{
}


void TimingExplorerImpl::initialize(void)
{
    if(strcmp(name(), "HangOnInit") == 0)
	{
	while(1)
	    {
	    sleep(1);
	    }
	}
}


/* --------------------- [ CORBA interface ] ----------------------*/

void TimingExplorerImpl::waitToReply(CORBA::Long waitTimeSec) 
{
    ACS_LOG(LM_FULL_INFO,"TimingExplorerImpl::waitToReply",
	    (LM_INFO,"Method waiting %d seconds to reply", waitTimeSec)) 
    sleep(waitTimeSec);
    ACS_LOG(LM_FULL_INFO,"TimingExplorerImpl::waitToReply",
	    (LM_INFO,"Done")) 
}


void TimingExplorerImpl::multipleReplies(CORBA::Long repetitions, 
					 CORBA::Long waitTimeSec, 
					 ACS::CBvoid_ptr cb, 
					 const ACS::CBDescIn& desc)
{
    printf("a\n");
    ACS_LOG(LM_FULL_INFO,"TimingExplorerImpl::multipleReplies",
	    (LM_INFO,"Method sending %d replies with a wait of %d seconds", 
	     repetitions, waitTimeSec));
    /*
     * Increment the counter for replies
     * and build this counter to build a unique thread name
     */
    multipleRepliesCounter_m++;
    char threadName[256];

    sprintf(threadName, "MultipleReplyThread_%d", multipleRepliesCounter_m);
    
    /*
     * Allocates and initialises the ThreadData
     * This is allocated on the heap and responsibility to delete
     * is is left to the thread object, as by contract.
     */
    ThreadData *threadData = new ThreadData(repetitions, 
					    cb,
					    desc);

    /*
     * In the method we create a new thread.
     * In this way each invocation of the method will 
     * run in parallel in its own independent thread.
     * The thread is created as autodelete, so the object
     * will be deleted when the thread exists.
     */
    MultipleReplyThread *methodThread = getContainerServices()->
	getThreadManager()->
	create<MultipleReplyThread, ThreadData*>(threadName, 
						 threadData, 
						 ACS::ThreadBase::defaultResponseTime, 
						 waitTimeSec*10000000,
						 true);
    /*
     * If creation of the thread is OK,
     * we resume the thread, that is always started suspended.
     */
    if(methodThread == NULL)
	{
	ACS_LOG(LM_FULL_INFO,"TimingExplorerImpl::multipleReplies",
		(LM_INFO,"Cannot create thread!"));
	}
    else
	{
	ACS_LOG(LM_FULL_INFO,"TimingExplorerImpl::multipleReplies",
		(LM_INFO,"Created thread!"));
	methodThread->resume();
	}
}




/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TimingExplorerImpl)
