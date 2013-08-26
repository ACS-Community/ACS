/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: testACSThreadCorrExmpl.cpp,v 1.6 2006/10/03 22:18:32 gchiozzi Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include <ACSErrTypeCommon.h>

// Commented from original example to remove a forward dependency in the test.
// #include <acsErrTypeLifeCycle.h>


static char *rcsId="@(#) $Id: testACSThreadCorrExmpl.cpp,v 1.6 2006/10/03 22:18:32 gchiozzi Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

/************************************************************** 
 * GCH
 * This thread implementation comes from a Correlator example
 * that was causing problems.
 * It has been modified after having fixed the problems in the
 * ACS thread libraries to provide the same functionality with
 * minimal changes.
 * See below for a more compact version.
 */
#define CORR_SIM_THREAD_NAME	"CorrSimThread"

class CorrSimThread : public ACS::Thread
{
public:
    /* GCH
     * In the original code here the constructor for the base class
     * ACS:Thread is receiving only name and suspend mode.
     * This means that responseTime and sleep time are not
     * passed down. The parameters are therefore useless here and
     * they get the default values.
     * This constructor implementation is therefore wrong!
     * Also, the del parameter is not passed.
     * The thread can be created only as NOT autodelete.
     * But then in the main somebody has to take care ot deleting it.
     * See the CorrSimThreadLoop class and notes in the main()
     */
    CorrSimThread(const ACE_CString &name,
		  const ACS::TimeInterval &responseTime = ThreadBase::defaultResponseTime,
		  const ACS::TimeInterval &sleepTime = ThreadBase::defaultSleepTime) :
	ACS::Thread(name)
    {
	ACS_TRACE("CorrSimThread::CorrSimThread");
    }

    ~CorrSimThread()
    {
	ACS_TRACE("CorrSimThread::~CorrSimThread");
    }
    
    void run();
    
private:

};  

void CorrSimThread::run()
{
    /* 
     * GCH
     * This code is not necessary any more,
     * since logging initialisation is done by the thread service
     * function before executing run()
     */
    // LoggingProxy logger(0, 0, 31);
    // LoggingProxy::init (&logger); 
    // LoggingProxy::ProcessName(getName().c_str());
    // LoggingProxy::ThreadName(getName().c_str());

    ACS_SHORT_LOG((LM_INFO, "%s thread started.", getName().c_str()));

    int  heartBeatMsgCount = 0;
    const int HEART_BEAT_PERIOD = 30;  // defines period of heart beat msg in seconds

    while ( check() && isSuspended() == false )
    {
        /*
         * GCH
	 * I did not touch this code, but I am not sure this is what you want.
	 * With this code, the sleep is executed only every HEART_BEAT_PERIOD 
	 * iterations.
	 * Therefore the iterations themself are "at full speed"
	 * but for the one every HEART_BEAT_PERIOD that sleeps one second
	 * May be this is just because of the way you have cut the real code.
	 * Otherwise the sleep should go AFTER the 'if' and not INSIDE it
	 * 
	 */

	// display a heart beat every 10 iterations (which is 10 seconds as
	// the getCanMessage() waits 1 second before timing out
	heartBeatMsgCount++;
 	if( !(heartBeatMsgCount % HEART_BEAT_PERIOD) )
 	{
	    /* 
	     * GCH
	     * ACS_OS::sleep() should not be used here.
	     * Instead you have to call the sleep(100ns)
	     * method inherited from the ThreadBase base class..
	     * The difference is that now the thread sleep()
	     * can be interrupted and handle suspend of the thread.
	     * 
	     * Notice also that the log is directly AFTER the sleep()
	     * This means that if the thread will be stopped
	     * while in the sleep(), it will return from there
	     * and an extra message will be written.
	     *
	     * Normally the sleep should be the last thing
	     * in the loop or there should be a check() just after it.
	     *
	     */
	    // ACE_OS::sleep(1);
  	    sleep(10000000);   
 	    ACS_SHORT_LOG((LM_INFO,"Just passing the time..."));
 	}
    }
    setStopped();
    ACS_SHORT_LOG((LM_INFO,"Exiting CorrSimThread::run()"));
}

/**************************************************************  
 * GCH
 * This alternative thread implementation
 * provides the same functionality as the one above,
 * but in a more compact way using runLoop() instead of run().
 */

class CorrSimThreadLoop : public ACS::Thread
{
public:
    /*
     * GCH
     * Here we handle the complete Thread constructor
     * signature, to be able to set all Thread parameters.
     * This thread has by specs response and sleep time of one
     * second, so we do not provide these as parameters
     * in the outer constructor class.
     * But the ThreadManager knows only of a specific
     * set of Thread signatures, therefore we have to
     * put the parameters in the interface and ignore them
     * when calling the parent class.
     */
    CorrSimThreadLoop(const ACE_CString &name,
		      const ACS::TimeInterval &ignoreRT = ThreadBase::defaultResponseTime,
		      const ACS::TimeInterval &ignoreST = ThreadBase::defaultSleepTime,
		      bool del=false) :
	ACS::Thread(name, 
		    10000000 /* 1 sec, hardcoded according to spec */, 
		    10000000 /* 1 sec, hardcoded according to spec */,
		    del)
    {
	ACS_TRACE("CorrSimThread::CorrSimThread");
    }

    ~CorrSimThreadLoop()
    {
	ACS_TRACE("CorrSimThread::~CorrSimThread");
    }
    
    void runLoop();
    
private:

};  

void CorrSimThreadLoop::runLoop()
{
    ACS_SHORT_LOG((LM_INFO,"This is an iteration for doing something."));
}


/************************************************************** 
 *
 * GCH
 *
 * Main program
 *
 * Sleeps have been introduced in the code to make its
 * execution more deterministic in terms of producing the same
 * messages in as much as possible the same order.
 * Without them, the code would work but with sometime different
 * results due to concurrency.
 *
 * About error handling.
 * I hake kept the exception/error handling as similar as 
 * possible to the original code, since the structure was already
 * good.
 * I have therefore just added an outer layer try/catch block
 * a fixed a few details and one important error, probably due
 * to a misunderstanding in the interface of the exception 
 * classes.
 **************************************************************/

int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    try
	{
	/* 
	 * GCH
	 * We use a local thread manager instead of a components' one
	 * since we are in a main and not in a Component
	 */
	ACS::ThreadManager tm;
	ACS::Thread *pCorrSimThread = NULL;

	/*************************************************************
	 * GCH
	 * Creates the thread with the same code originally used
	 * in the execute() of the example component.
	 * (just removed passing the component as a thread parameter).
	 */
	/*
	 * GCH
	 * Rather than ACS_SHORT_LOG it is better to use ACS_LOG
	 * In this case we get in the log detailed information
	 * about the file, line and routine where the log was generated.
	 * I have left in the code most of the ACS_SHORT_LOG
	 * in order not to touch the original code provided as an example
	 *
	 * ACS_SHORT_LOG is fine in test programs, but for application
	 * code, ACS_SHORT_LOG provides additional information
	 * that is worth the little additional effort
	 */
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "=============== Creating thread as was done in example component."));
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Spawning %s thread....", CORR_SIM_THREAD_NAME));
	    pCorrSimThread = tm.create<CorrSimThread>(CORR_SIM_THREAD_NAME);	
	    if( pCorrSimThread != NULL )
		{
                pCorrSimThread->resume();
		ACS_SHORT_LOG((LM_INFO,"%s thread created.", CORR_SIM_THREAD_NAME));
		ACE_OS::sleep(2);
		}
	    else
		{
		/*
		 * GCH
		 *
		 * ATTENTION: This code if WRONG!!!!
		 *
		 * char errorBuf[100];
		 * sprintf(errorBuf,"Failed to create %s thread!", CORR_SIM_THREAD_NAME);
		 * throw acsErrTypeLifeCycle::LifeCycleExImpl( __FILE__, __LINE__, errorBuf);
		 *
		 * The 3rd parameter is the name of the ROUTINE.
		 * Not the error message.
		 * This would put the wrong thing in the XML.
		 * If an exception has a text message, this is part 
		 * of the signature.
		 * Otherwise no error string is expected more than what
		 * is automatically done by the exception classes.
		 *
		 * I have fixed this problem here and in all other places in the code.
		 * I have also replaced the original LifeCycleExImpl exception with
		 * with another one, more suited to describe the problem.
		 */
		throw ACSErrTypeCommon::CouldntCreateThreadExImpl( __FILE__, __LINE__, "main");
		}
	    }
	catch(ACSErrTypeCommon::CouldntCreateThreadExImpl &ex)
	    {
	    throw ACSErrTypeCommon::CouldntCreateThreadExImpl( ex, __FILE__, __LINE__, "main");
	    }
	catch(...)
	    {
	    throw ACSErrTypeCommon::CouldntCreateThreadExImpl( __FILE__,__LINE__,"main");
	    }

    
	/*
	 * GCH
	 * Wait a while to simulate components lifetime
	 */
	ACS_SHORT_LOG((LM_INFO,"Waiting"));
	ACE_OS::sleep(2);

	/*
	 * GCH
	 * Cleanup the thread with the same code originally used
	 * in the cleanup() of the example component.
	 * (just removed passing the component as a thread parameter).
	 */
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "=============== Cleaning up thread as was done in example component"));

	int threadState = tm.isAlive(CORR_SIM_THREAD_NAME);
	// Stop the thread & wait for it to complete
	ACS_SHORT_LOG((LM_INFO,"%s state: %d",CORR_SIM_THREAD_NAME,threadState));
		   
	/*
	 * GCH
	 * Calling stop() now returns after the thread has been
	 * stopped (soft) or after a timoeut.
	 * so there is no need to have the while loop.
	 * If a "soft" stop is not sufficiently safe,
	 * you could call terminate(), that is "harder"
	 * by calling cancel() is stop() fails (see the documentation.
	 *
	 * But I left here the while because it is interestingand 
	 * could be useful in other situations.
	 * 
	 * Notice that this code can block forever, since there is no
	 * limitation to the iterations.
	 * It would be better to add a counter and exit anyway
	 * if the counter is exceeded.
	 * I will do this in the second example below, calling 
	 * terminate() instead of stop().
	 *
	 * Another important aspect is that here tm.isAlive() is called
	 * to check if the thread is stopped.
	 * This is the right thing to do, because it is asking the ThreadManager
	 * and not the Thread object, that could have been
	 * already destroyed; remember: never use the pointer 
	 * to an autodelete thread
	 */
	tm.stop(CORR_SIM_THREAD_NAME);
	while( threadState )
	    {
	    ACS_SHORT_LOG((LM_INFO,"Trying to shut down %s",CORR_SIM_THREAD_NAME));
	    ACE_OS::sleep(1);
	    threadState = tm.isAlive(CORR_SIM_THREAD_NAME);
	    }
	ACS_SHORT_LOG((LM_INFO,"End of cleanup"));
	ACE_OS::sleep(5);

	/*
	 * GCH
	 * The thread is not autodelete, but has been allocated 
	 * on the heap.
	 * It is therefore necessary to delete the object
	 * to avoid memory leaks.
	 * There are two ways: 
	 * - explicitly ask the ThreadManager o take care.
	 *      tm.destroy(pCorrSimThread)
	 * - call delete:
	 *      delete pCorrSimThread;
	 * They are equivalent
	 * Comment the call to destroy() to see how it fails
	 * afterwards and hoe exceptions are handled.
	 */
	ACS_SHORT_LOG((LM_INFO,"Destroy thread"));
	tm.destroy(pCorrSimThread);
	ACE_OS::sleep(5);

	/*************************************************************
	 * GCH
	 * The same as above with the compact thread class
	 */
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "=============== Creating thread as was done in example component. Compact class"));
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Spawning %s thread....", CORR_SIM_THREAD_NAME));

	    /*
	     * GCH
	     * Here we create a Thread with the same name of the previous one.
	     * This would fail if we would not have detroyed the other thread,
	     * because the ThreadManager would be handling already a thread
	     * with this name.
	     * Try to comment out tm.destroy() above to see this behavior.
	     */
	    /*
	     * GCH
	     * This time we take advantage of the autodelete flag,
	     * so that we do not need to handle object deletion afterwards.
	     * The response and sleep times are ignored in the constructor, as
	     * described in the above documentation, so I pass 0.
	     */
	    pCorrSimThread = tm.create<CorrSimThreadLoop>(CORR_SIM_THREAD_NAME,
							  0 , 0,
							  true);
	    ACS_SHORT_LOG((LM_INFO,"Spawning %s thread....", CORR_SIM_THREAD_NAME));
	    if( pCorrSimThread != NULL )
		{
                pCorrSimThread->resume();
		ACS_SHORT_LOG((LM_INFO,"%s thread created.", CORR_SIM_THREAD_NAME));
		ACE_OS::sleep(2);
		}
	    else
		{
		throw ACSErrTypeCommon::CouldntCreateThreadExImpl( __FILE__, __LINE__, "main");
		}
	    }
	catch(ACSErrTypeCommon::CouldntCreateThreadExImpl &ex)
	    {
	    throw ACSErrTypeCommon::CouldntCreateThreadExImpl( ex, __FILE__, __LINE__,"main"); 
	    }
	catch(...)
	    {
	    throw ACSErrTypeCommon::CouldntCreateThreadExImpl( __FILE__,__LINE__,"main");
	    }

    
	/*
	 * GCH
	 * Wait a while to simulate components lifetime
	 */
	ACS_SHORT_LOG((LM_INFO,"Waiting"));
	ACE_OS::sleep(5);

	/*
	 * GCH
	 * Cleanup the thread with the same code originally used
	 * in the cleanup() of the example component.
	 * (just removed passing the component as a thread parameter).
	 */
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "=============== Cleaning up thread as was done in example component"));

	/*
	 * GCH
	 * Calling terminate() is sufficient, as described above.
	 * No wait loop would be necessary.
	 * But I leave it here to show how to handle a limited
	 * number of iterations.
	 */
	int maxIterations = 10;
	tm.terminate(CORR_SIM_THREAD_NAME);
	while( maxIterations-- && tm.isAlive(CORR_SIM_THREAD_NAME))
	    {
	    ACS_SHORT_LOG((LM_INFO,"Trying to shut down %s",CORR_SIM_THREAD_NAME));
	    ACE_OS::sleep(1);
	    }
	if(!maxIterations)
	    {
	    ACS_LOG(LM_SOURCE_INFO,"main", 
		    (LM_ERROR, "Error stopping thread"));
	    }
	ACS_SHORT_LOG((LM_INFO,"End of cleanup"));

	/***********************************************
	 * GCH
	 * Wait for everything to cleanup and go home
	 */
	ACE_OS::sleep(5);
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "=============== The end"));

	}
    catch(ACSErr::ACSbaseExImpl &ex)
	{
	/*
	 * GCH
	 * I just log the exception.
	 * Nothing else is reasonable to do.
	 * I could may be add another error to the stack trace.
	 */
	ex.log();
	}
    catch(...)
	{
	/*
	 * GCH
	 * I should never get here.
	 * But it is very good to have a catch(...) to capture
	 * unexpected exception, i.e. exceptions that we should have
	 * handled but we forgot.
	 */
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_ERROR, "=============== Unexpected exception"));
	}
    return 0;

}

/* __oOo__ */
