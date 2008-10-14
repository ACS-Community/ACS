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
* "@(#) $Id: testACSThreadStackSize.cpp,v 1.1 2008/10/14 21:28:07 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"


static char *rcsId="@(#) $Id: testACSThreadStackSize.cpp,v 1.1 2008/10/14 21:28:07 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


class TestACSThread :public ACS::Thread
{
  public:
    TestACSThread(const ACE_CString& name,
			       const ACS::TimeInterval& responseTime,
			       const ACS::TimeInterval& sleepTime,
			       bool del,
			       const long thrFlags,
			       const size_t stackSize) :
	ACS::Thread(name, responseTime, sleepTime, del, thrFlags, stackSize)
	{
	    ACS_TRACE("TestACSThread::TestACSThread");
	    loopCounter_m = 0;
	}

    ~TestACSThread()
	{
	    ACS_TRACE("TestACSThread::~TestACSThread");

	    terminate();
	}

    virtual void runLoop()
	{
	    if (loopCounter_m==2)
		yield();
	    if (loopCounter_m==10)
		{
		exit();
		}
	    ACS_LOG(LM_SOURCE_INFO, "TestACSThread::runLoop",
		    (LM_INFO, "Thread (%d) parameter: %s", loopCounter_m, msg.c_str()));
	    ++ loopCounter_m;
	}
  protected:
    int loopCounter_m;
    ACE_CString msg;
};

///////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////////

class TestACSThreadWithParameter :public ACS::Thread
{
  public:
    TestACSThreadWithParameter(const ACE_CString& name,
			       char  *parmMsg,
			       const ACS::TimeInterval& responseTime,
			       const ACS::TimeInterval& sleepTime,
			       bool del,
			       const long thrFlags,
			       const size_t stackSize) :
	ACS::Thread(name, responseTime, sleepTime, del, thrFlags, stackSize)
	{
	    ACS_TRACE("TestACSThreadWithParameter::TestACSThreadWithParameter");
	    loopCounter_m = 0;
	    msg = parmMsg;
	    ACS_LOG(LM_SOURCE_INFO, "TestACSThreadWithParameter::TestACSThreadWithParameter",
		    (LM_INFO, "Thread parameter: %s", parmMsg));
	}

    ~TestACSThreadWithParameter()
	{
	    ACS_TRACE("TestACSThreadWithParameter::~TestACSThreadWithParameter");

	    terminate();
	}

    virtual void runLoop()
	{
	    if (loopCounter_m==2)
		yield();
	    if (loopCounter_m==10)
		{
		exit();
		}
	    ACS_LOG(LM_SOURCE_INFO, "TestACSThreadWithParameter::runLoop",
		    (LM_INFO, "Thread (%d) parameter: %s", loopCounter_m, msg.c_str()));
	    ++ loopCounter_m;
	}
  protected:
    int loopCounter_m;
    ACE_CString msg;
};

int main(int argc, char *argv[])
{
	LoggingProxy logger_m(0, 0, 31);
	LoggingProxy::init(&logger_m);

	ACS::ThreadManager tm;

	ACS_LOG(LM_SOURCE_INFO,"main",
			(LM_INFO, "=============== 1 - Creating thread with a parameter and stack size"));
	char *msg="Thread msg";
	TestACSThreadWithParameter *a =
		tm.create<TestACSThreadWithParameter, char*>("TestThreadA",
				msg,
				ACS::ThreadBase::defaultResponseTime,
				ACS::ThreadBase::defaultSleepTime,
				false,
				THR_NEW_LWP | THR_DETACHED,
				5000);
		a->resume();
		sleep(20);
		ACS_LOG(LM_SOURCE_INFO,"main",
				(LM_INFO, "Requesting ThreadManager to destroy thread"));
		tm.destroy(a);


		ACS_LOG(LM_SOURCE_INFO,"main",
				(LM_INFO, "=============== 2 - Creating thread with stack size"));
		TestACSThread *b =
			tm.create<TestACSThread>("TestThreadB",

					ACS::ThreadBase::defaultResponseTime,
					ACS::ThreadBase::defaultSleepTime,
					false,
					THR_NEW_LWP | THR_DETACHED,
					5000);
			b->resume();
			sleep(20);
			ACS_LOG(LM_SOURCE_INFO,"main",
					(LM_INFO, "Requesting ThreadManager to destroy thread"));
			tm.destroy(b);


			/**
			 * Wait for everything to cleanup and go home
			 */
			sleep(20);
			ACS_LOG(LM_SOURCE_INFO,"main",
					(LM_INFO, "=============== The end"));

			return 0;
}








