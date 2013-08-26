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
* "@(#) $Id: testACSThreadDoubleLoop.cpp,v 1.2 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include "acsThreadTest.h"

static char *rcsId="@(#) $Id: testACSThreadDoubleLoop.cpp,v 1.2 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;
    TestACSThread     *b;

    /*
     * Creates a thread with a long period
     * and then stop it.
     * Stop will take as long as the period but fast responce time.
     * Stop should fail.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 1 - Creating thread with long period and stop() it"));
    b = tm.create<TestACSThread>("TestThreadC", 
				 20*100*1000*10 /*=2sec*/, 20*100*1000*10 /*2 sec*/);
    b->resume();
    sleep(10);
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
	    (LM_INFO, "=============== 2 - Creating thread with long period and cancel() it"));
    b = tm.create<TestACSThread>("TestThreadC", 
				 20*100*1000*10 /*=2 sec*/, 20*100*1000*10 /*2 sec*/);
    b->resume();
    sleep(10);
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








