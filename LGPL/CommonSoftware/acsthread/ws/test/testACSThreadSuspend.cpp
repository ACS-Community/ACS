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
* "@(#) $Id: testACSThreadSuspend.cpp,v 1.2 2006/03/24 12:42:31 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2005-02-15  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"


#include "acsThreadManager.h"
#include "acsThreadTest.h"


static char *rcsId="@(#) $Id: testACSThreadSuspend.cpp,v 1.2 2006/03/24 12:42:31 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
    bool returnCode;

    LoggingProxy logger_m(0, 0, 31);
    LoggingProxy::init(&logger_m);

    ACS::ThreadManager tm;

    /*********************************************************
     * Creates an autodelete thread that runs every 5 seconds
     * with the help of the ThreadManager
     */
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "=============== 1 - Autodelete thread"));
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Creating thread"));
    TestACSThread *a = tm.create<TestACSThread>("TestThreadA", 
						5 * 10 * 1000000 /* 5s */, 
						5 * 10 * 1000000 /* 5s */,
						true);
    a->resume();
    /*
     * After 13 seconds (i.e. 2 loops)
     * It suspends it twice
     */
    sleep(13);
    returnCode = a->isSuspended();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Checking suspended status. Returned: %d", returnCode));

    returnCode = a->suspend();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Called suspend. Returned: %d", returnCode));

    returnCode = a->isSuspended();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Checking suspended status. Returned: %d", returnCode));
    returnCode = a->suspend();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Called suspend. Returned: %d", returnCode));

    /*
     * After 6 seconds, it resumes once
     */
    sleep(6);
    returnCode = a->isSuspended();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Checking suspended status. Returned: %d", returnCode));

    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Resuming thread."));

    returnCode = a->resume();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Called resume. Returned: %d", returnCode));

    /*
     * After 6 seconds, it resumes again.
     */
    sleep(6);
    returnCode = a->isSuspended();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Checking suspended status. Returned: %d", returnCode));

    returnCode = a->resume();
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Called resume again. Returned: %d", returnCode));

    /*
     * Waits a couple of iterations
     */
    sleep(12);

    /*
     * done
     */ 
    ACS_LOG(LM_SOURCE_INFO,"main", 
	    (LM_INFO, "Done"));
    return 0;

}








