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
* "@(#) $Id: testACSThreadSleep.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include <acsErrTypeLifeCycle.h>
#include <ACSErrTypeCommon.h>


static char *rcsId="@(#) $Id: testACSThreadSleep.cpp,v 1.3 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

class SleepThreadLoop : public ACS::Thread
{
public:
    SleepThreadLoop(const ACE_CString &name,
		      const ACS::TimeInterval &responseTime = ThreadBase::defaultResponseTime,
		      const ACS::TimeInterval &sleepTime = ThreadBase::defaultSleepTime,
		      bool del=false) :
	ACS::Thread(name, 
		    responseTime,
		    sleepTime,
		    del)
    {
	ACS_TRACE("SleepThreadLoop::SleepThreadLoop");
    }

    ~SleepThreadLoop()
    {
	ACS_TRACE("SleepThreadLoop::~SleepThreadLoop");
    }
    
    void runLoop();
    
private:

};  

/*
 * After every sleep() I check if I have to go back
 * This is important to handle properly stopping the thread
 */
void SleepThreadLoop::runLoop()
{
    ACS_LOG(LM_SOURCE_INFO,"SleepThreadLoop::runLoop", 
	    (LM_INFO, "Will sleep 1 sec."));
    sleep(10000000); // 1 s
    if(!check()) return;
    ACS_LOG(LM_SOURCE_INFO,"SleepThreadLoop::runLoop", 
	    (LM_INFO, "Will sleep 2 sec."));
    sleep(20000000); // 2 s
    if(!check()) return;
    ACS_LOG(LM_SOURCE_INFO,"SleepThreadLoop::runLoop", 
	    (LM_INFO, "Will sleep 1 sec."));
    sleep(10000000); // 1 s
    if(!check()) return;
    ACS_LOG(LM_SOURCE_INFO,"SleepThreadLoop::runLoop", 
	    (LM_INFO, "Done iteration."));
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

    ACS::ThreadManager tm;

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== Creating thread"));
    tm.create<SleepThreadLoop>("SleepThread",
			       ACS::ThreadBase::defaultResponseTime,
			       10000000, // 1 sec
			       true);   
    /*
     * GCH
     * Wait a while to simulate components lifetime
     */
    tm.resume("SleepThread");
    ACS_SHORT_LOG((LM_INFO,"Waiting"));
    ACE_OS::sleep(20);


    /*
     * GCH
     * Suspend and wait a while to simulate components lifetime
     */
    ACS_SHORT_LOG((LM_INFO,"Suspend"));
    tm.suspend("SleepThread");
    ACS_SHORT_LOG((LM_INFO,"Waiting"));
    ACE_OS::sleep(5);

    /*
     * GCH
     * Resume and wait a while to simulate components lifetime
     */
    ACS_SHORT_LOG((LM_INFO,"Resume"));
    tm.resume("SleepThread");
    ACS_SHORT_LOG((LM_INFO,"Waiting"));
    ACE_OS::sleep(5);

    /*
     * GCH
     * Suspend and wait a while to simulate components lifetime
     */
    ACS_SHORT_LOG((LM_INFO,"Suspend"));
    tm.suspend("SleepThread");
    ACS_SHORT_LOG((LM_INFO,"Waiting"));
    ACE_OS::sleep(5);

    /*
     * GCH
     * Resume and wait a while to simulate components lifetime
     */
    ACS_SHORT_LOG((LM_INFO,"Resume"));
    tm.resume("SleepThread");
    ACS_SHORT_LOG((LM_INFO,"Waiting"));
    ACE_OS::sleep(20);

    /*
     * GCH
     * Cleanup the thread.
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== Cleaning up thread"));

    tm.stop("SleepThread");
    ACS_SHORT_LOG((LM_INFO,"End of cleanup"));

    /***********************************************
     * GCH
     * Wait for everything to cleanup and go home
     */
    ACE_OS::sleep(5);
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== The end"));

    return 0;

}

/* __oOo__ */
