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
* "@(#) $Id: testACSThread.cpp,v 1.24 2009/08/28 09:53:54 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include "acsThreadTest.h"

static char *rcsId="@(#) $Id: testACSThread.cpp,v 1.24 2009/08/28 09:53:54 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"

int testACSThread (char *szCmdLn){
  int  argc;
  char *argv[100];

  argc = argUnpack(szCmdLn, argv);
  argv[0] = "testACSThread";
#else
int main(int argc, char *argv[])
{
#endif
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS_CHECK_LOGGER;
    ACS::ThreadManager tm(getNamedLogger("ThrMgrLogger"));

    /*
     * Creates a thread passing just thread name
     * Given the constructor default parameters, this will have
     * default iteration time and will NOT be auto-delete.
     * We let it run for a while, then we ask the ThreadManager to destroy  it.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 1 - Creating thread passing just thread name"));
    TestACSThread *a = tm.create<TestACSThread>("TestThreadA");
    a->resume();
    sleep(35);
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
    TestACSThread *b = tm.create<TestACSThread>("TestThreadB");
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
	    (LM_INFO, "=============== 3 - Creating thread, suspend and resume a few times"));
    b = tm.create<TestACSThread>("TestThreadC", 
				 100*1000*10/*=100ms*/, 142*100*1000*10 /*14.2 sec*/);
    b->resume();
    sleep(10);
    b->suspend();
    sleep(10);
    b->resume();
    sleep(50);
    b->suspend();
    sleep(20);
    b->resume();
    sleep(50);

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Deleting thread"));
    delete b;

    /*
     * Creates a thread that is never woken-up
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 4 - Creating thread that is never woken-up"));
    TestACSThread *forEverSleep = tm.create<TestACSThread>("SuspendForEver");
    sleep(20);

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Deleting thread"));
    delete forEverSleep;

    /*
     * Creates a thread with a long period
     * and then stop it.
     * Stop will take as long as the period but fast responce time.
     * Stop should fail.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 5 - Creating thread with long period and stop() it"));
    b = tm.create<TestACSThread>("TestThreadC", 
				 20*100*1000*10 /*=2sec*/, 100*100*1000*10 /*10 sec*/);
    b->resume();
    sleep(5);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Stopping thread"));
    if(b->stop() == false)
	{
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "Stop failed, as expected"));
	}
    else
	{
	ACS_LOG(LM_SOURCE_INFO,"main", 
		(LM_INFO, "Stop succeded but should have failed"));
	}
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Deleting thread"));
    delete b;

    /*
     * Creates a thread with a long period
     * and then cancel it.
     * Cancel should be quick.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 6 - Creating thread with long period and cancel() it"));
    b = tm.create<TestACSThread>("TestThreadC", 
				 20*100*1000*10 /*=2 sec*/, 100*100*1000*10 /*10 sec*/);
    b->resume();
    sleep(5);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Cancelling thread"));
    b->cancel();

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Deleting thread"));
    delete b;

    /**
     * Wait for everything to cleanup and go home
     */
    sleep(5);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== The end"));

    return 0;

}








