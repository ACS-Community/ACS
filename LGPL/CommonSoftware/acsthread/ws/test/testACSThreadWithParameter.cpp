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
* "@(#) $Id: testACSThreadWithParameter.cpp,v 1.11 2008/03/28 12:34:25 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"


static char *rcsId="@(#) $Id: testACSThreadWithParameter.cpp,v 1.11 2008/03/28 12:34:25 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);



class TestACSThreadWithParameter :public ACS::Thread
{
  public:
    TestACSThreadWithParameter(const ACE_CString& name, 
			       char  *parmMsg, 
			       const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			       const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime,
			       bool del=false) :
	ACS::Thread(name, responseTime, sleepTime, del)
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

    /*
     * Creates a thread passing just thread name
     * Given the constructor default parameters, this will have
     * default iteration time and will NOT be auto-delete.
     * We let it run for a while, then we ask the ThreadManager to destroy  it.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 1 - Creating thread passing just thread name"));
    char *msg="Thread msg";
    TestACSThreadWithParameter *a = tm.create<TestACSThreadWithParameter, char*>("TestThreadA", msg);
    a->resume();
    sleep(20);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Requesting ThreadManager to destroy thread"));
    tm.destroy(a);

    /*
     * Creates a thread as before, but initially suspended.
     * We wait a while to see if it is really suspended.
     * Then we resume its execution and we let it sun for a while.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 2 - Creating thread initially suspended"));
    TestACSThreadWithParameter *b = tm.create<TestACSThreadWithParameter, char*>("TestThreadB", msg);
    sleep(20);
    b->resume();
    sleep(20);

    /*
     * Here we forcefully delete the thread without stopping it first
     * The thread should automatically stop gracefully and then be deleted.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Deleting thread"));
    delete b;

    /*
     * Creates a thread as before not initially suspended
     * and with specific responce and sleep times.
     * Then we suspend and resume its execution a few times to
     * see if suspend works properly.
     * We can reuse the pointer to the deleted thread
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 3 - Creating thread"));
    b = tm.create<TestACSThreadWithParameter>("TestThreadC", 
					      msg, 
					      100*1000*10/*=100ms*/, 142*100*1000*10 /*14.2 sec*/);
    b->resume();
    sleep(20);

    /*
     * Creates a thread that is never woken-up
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 4 - Creating thread that is never woken-up"));
    TestACSThreadWithParameter *forEverSuspend = tm.create<TestACSThreadWithParameter>("SuspendForEver", msg);
    sleep(20);

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Deleting thread"));
    delete forEverSuspend;

    /**
     * Wait for everything to cleanup and go home
     */
    sleep(20);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== The end"));

    return 0;

}








