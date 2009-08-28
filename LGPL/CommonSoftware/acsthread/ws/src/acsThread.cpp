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
* "@(#) $Id: acsThread.cpp,v 1.42 2009/08/28 09:53:54 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

#include "vltPort.h"

static char *rcsId="@(#) $Id: acsThread.cpp,v 1.42 2009/08/28 09:53:54 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acsThread.h"
#include "acsThreadManager.h"


using namespace ACS;
using namespace acsthreadErrType;
// static
ACE_TSS<ThreadManager::ThreadManagerTSS> ThreadManager::threadManagerTSS;

Thread::Thread(const ACE_CString & name,
	       const TimeInterval& responseTime,
	       const TimeInterval& sleepTime,
	       const bool del,
	       const long _thrFlags,
	       const size_t _stackSize
		     ):
    ThreadBase(name,
	       ACE_Thread_Manager::instance(),
	       (void*)Thread::threadSvc,
//	       (void*)this,
	       static_cast<void*>(this),
	       responseTime,
	       sleepTime,
	       false /* super class shall not create a thread */,
	       THR_NEW_LWP | THR_DETACHED,
	       _stackSize),
    logger_mp(0), thrMgr_mp(0), delete_m(del)
{
    ACS_TRACE("ACS::Thread::Thread");
    thrMgr_mp = ACS::ThreadManager::threadManagerTSS->getThreadManager(true);
    if (thrMgr_mp != NULL)
    {
        if(!thrMgr_mp->add(name, static_cast<ACS::Thread*>(this)))
        {
            acsthreadErrType::ThreadAlreadyExistExImpl ex1(__FILE__, __LINE__, "ACS::Thread::Thread");
            ex1.setThreadName(getName());
            acsthreadErrType::CanNotSpawnThreadExImpl ex2(ex1, __FILE__, __LINE__, "ACS::Thread::Thread");
            ex2.setThreadName(getName());
            throw ex2;
        }

        // set logger
        Logging::Logger::LoggerSmartPtr thrMgrLog = getThreadManager()->getLogger();
        //if thread manager's logger is not global logger
        if (thrMgrLog->getName() != Logging::Logger::getGlobalLogger()->getName() )
        {
        	setLogger(thrMgrLog);
        }//if
    }//if
    /*
     * We create a Kernel Thread and the thread is:
     * But we have to be careful: if we have a JOINABLE thread,
     * we HAVE TO JOIN to it to release resources!!!!
     */
    if (!create(_thrFlags))
    {
        if (thrMgr_mp != NULL)  thrMgr_mp->removeFromMap(name);
        acsthreadErrType::CanNotSpawnThreadExImpl ex(__FILE__, __LINE__, "ACS::Thread::Thread");
        ex.setThreadName(getName());
        throw ex;
    }//if
}//Thread::Thread

Thread::~Thread()
{
    ACS_TRACE("ACS::Thread::~Thread");

   /*
    * Here we call terminte to make sure the thread
    * function is completed BEFORE the class
    * members and virtual tables are destroyed.
    * It could be that the thread function is using
    * some object resources and if this happens after
    * the object has been destroyed we can easily get
    * memory access faults.
    */
    terminate();
    if (thrMgr_mp != 0)
	{
	thrMgr_mp->removeFromMap(this->getName());
	}
}//Thread::~Thread

ACS::ThreadManager*  Thread::getThreadManager() const
{
    return thrMgr_mp;
}//Thread::getThreadManager


void Thread::commonStart()
{
    logger_mp = new LoggingProxy(0, 0, 31);
    LoggingProxy::init(logger_mp);
    LoggingProxy::ThreadName(getName().c_str());
    ACS_TRACE("ACS::Thread::commonStart"); // we can not put before initalization of logging system
    try
    {
    	onStart();
    }
    catch(ACSErr::ACSbaseExImpl &_ex)
    {
    	ExceptionInOnStartExImpl ex(_ex, __FILE__, __LINE__, "ACS::Thread::commonStart");
    	ex.setThreadName(getName());
    	throw ex;
    }
    catch(CORBA::SystemException &_ex)
    {
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
    								    "ACS::Thread::commonStart");
    	corbaProblemEx.setMinor(_ex.minor());
    	corbaProblemEx.setCompletionStatus(_ex.completed());
    	corbaProblemEx.setInfo(_ex._info().c_str());
    	ExceptionInOnStartExImpl ex(corbaProblemEx, __FILE__, __LINE__, "ACS::Thread::commonStart");
    	ex.setThreadName(getName());
    	throw ex;
    }
    catch(...)
    {
    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "ACS::Thread::commonStart");
    	ExceptionInOnStartExImpl ex(uex, __FILE__, __LINE__, "ACS::Thread::commonStart");
    	ex.setThreadName(getName());
    	throw ex;
    }//try-catch
}//commonStart

void Thread::commonStop()
{
    ACS_TRACE("ACS::Thread::commonStop");
    try
    {
    	onStop();
    }
    catch(ACSErr::ACSbaseExImpl &_ex)
    {
    	ExceptionInOnStopExImpl ex(_ex, __FILE__, __LINE__, "ACS::Thread::commonStop");
    	ex.setThreadName(getName());
    	throw ex;
    }
    catch(CORBA::SystemException &_ex)
    {
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
    								    "ACS::Thread::commonStop");
    	corbaProblemEx.setMinor(_ex.minor());
    	corbaProblemEx.setCompletionStatus(_ex.completed());
    	corbaProblemEx.setInfo(_ex._info().c_str());
    	ExceptionInOnStopExImpl ex(corbaProblemEx, __FILE__, __LINE__, "ACS::Thread::commonStop");
    	ex.setThreadName(getName());
    	throw ex;
    }
    catch(...)
    {
    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "ACS::Thread::commonStop");
    	ExceptionInOnStopExImpl ex(uex, __FILE__, __LINE__, "ACS::Thread::commonStop");
    	ex.setThreadName(getName());
    	throw ex;
    }//try-catch

    // logger can be deleted only if delete is not called from SVC function
    if (!delete_m) delete logger_mp; // we can delete the logger w/o  problem because of fix in the logging system
}//commonStop

/*
 * Debugging logs are commented for performance reasons
 * Uncomment them if needed for debugging.
 */
void Thread::run()
{
	ACS_TRACE("ACS::Thread::run");
	try
	{
		while (check())
		{
			/*
			 * This check if isSuspended should not be necessary and is just
			 * a precaution.
			 * Now sleep() takes care of hanlding suspension.
			 * At the first iteration the sleep is performed in the
			 * threadSvc, so that we do not even come here.
			 * At the following iterations is done here below, followed
			 * by the check() here above in the while condition.
			 */
			if (!isSuspended())
			{
				runLoop();
			}
			// else
			//   {
			//   ACS_TRACE("ACS::Thread::run susp");
			//   }

			/*
			 * GCH 2006-01-26
			 * Here we can now call directly sleep.
			 * We do not need any more any waitForResume(), since
			 * everything is now implemented inside the sleep itself.
			 */
			/*
			 * With the current implementation of sleep()
			 * the run will stop here also in case of suspend().
			 * until released or interrupted.
			 * Immediatly after it will go back to check()
			 * ensuring that we verify if we have to perform another iteration
			 * or exit the thread.
			 * After a sleep() always do a check()!
			 */
			sleep();
		}//while
	}
	catch(ACSErr::ACSbaseExImpl &_ex)
	{
		ExceptionInRunLoopExImpl ex(_ex, __FILE__, __LINE__, "ACS::Thread::runLoop");
		ex.setThreadName(getName());
		throw ex;
	}
	catch(CORBA::SystemException &_ex)
	{
		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, "ACS::Thread::runLoop");
		corbaProblemEx.setMinor(_ex.minor());
		corbaProblemEx.setCompletionStatus(_ex.completed());
		corbaProblemEx.setInfo(_ex._info().c_str());
		ExceptionInRunLoopExImpl ex(corbaProblemEx, __FILE__, __LINE__, "ACS::Thread::runLoop");
		ex.setThreadName(getName());
		throw ex;
	}
	catch(...)
	{
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "ACS::Thread::runLoop");
		ExceptionInRunLoopExImpl ex(uex, __FILE__, __LINE__, "ACS::Thread::runLoop");
		ex.setThreadName(getName());
		throw ex;
	}//try-catch
} //runLoop

/*
 * We do not put logs here to ensure max performance
 */
void Thread::threadSvc(void *param)
{
//    Thread* thrObj_p = (Thread*)(((ThreadBaseParameter*) param)->getParameter());

    /* change from getParameter() to getThreadBase()
     * according to acsThreadBase.h, getThreadBase is for this purpose
     * todo : 1. modify Thread constructor to let getParameter() be useful
     *        2. remove const from getThreadBase() , then the line could be simplier :
     * Thread* thrObj_p = dynamic_cast<const Thread*>((static_cast<ThreadBaseParameter*>(param))->getThreadBase());
     */
    Thread* thrObj_p = const_cast<Thread *>(dynamic_cast<const Thread*>((static_cast<ThreadBaseParameter*>(param))->getThreadBase()));

    /* since we use dynamic_cast, we'd better check if null */
    if( thrObj_p == NULL ) {
      delete static_cast<ThreadBaseParameter*>(param);
      return;
    }

    /*
     * If the thread is born suspended,
     * call the thread sleep() that will wait until released
     * or interrupted.
     * After that always perform a check() so that we can exit cleanly
     * in case this was the reason for interrupting the sleep.
     */
    if( thrObj_p->isSuspended() )
	{
	thrObj_p->sleep();
	}

    try
    {
    	if( thrObj_p->check() )
    	{
    		thrObj_p->commonStart(); // Init logger. Should be this executed before while ?
    		thrObj_p->run();
    		thrObj_p->commonStop(); // This deletes the logger. Mo more logs after this!
    	}//if
    }
    catch(ACSErr::ACSbaseExImpl &_ex)
    {
    	ThreadExecutionProblemExImpl ex(_ex, __FILE__, __LINE__, "ACS::Thread::threadSvc");
    	ex.log();
    }
    catch(CORBA::SystemException &_ex)
    	{
    		ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__, "ACS::Thread::threadSvc");
    		corbaProblemEx.setMinor(_ex.minor());
    		corbaProblemEx.setCompletionStatus(_ex.completed());
    		corbaProblemEx.setInfo(_ex._info().c_str());
    		ThreadExecutionProblemExImpl ex(corbaProblemEx, __FILE__, __LINE__, "ACS::Thread::threadSvc");
    		ex.log();
    	}
    catch(...)
    {
    	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "ACS::Thread::threadSvc");
    	ThreadExecutionProblemExImpl ex(uex, __FILE__, __LINE__, "ACS::Thread::threadSvc");
    	ex.log();
    }//try-catch

    thrObj_p->setStopped();

    delete static_cast<ThreadBaseParameter*>(param);

    /**
     * @todo GCH IMPORTANT
     * Look at the code after this comment.
     * This needs to be changed.
     * If we here delete the object, other threads using it might
     * get into big troubles.
     * In particular we have problems with calls to stop() from another thread.
     * stop() monitors thread variables to know if the object managed
     * to stop(), but if the object has been also deleted, that variables are
     * not valid any more and we get a segfault().
     */
    if (thrObj_p->delete_m)
	{
	delete thrObj_p; // deleting thread object
	}
} // threadSvc

/*___oOo___*/
