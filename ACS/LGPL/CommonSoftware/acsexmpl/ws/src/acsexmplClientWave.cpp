/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsexmplClientWave.cpp,v 1.102 2007/02/01 05:14:26 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-06-17 fixed client.init(argc,argv)
* gchiozzi 2002-02-13 cleane up
* msekoran  2001/07/13  created 
*/

/** @file acsexmplClientWave.cpp
 *  @htmlonly
 *  <br><br>
    @endhtmlonly
 *  @param "component name" Use this required parameter to specify which component
 *  should be activated.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-ORBEndpoint iiop://yyy:xxxx" Use this optional parameter to specify which host/port SimpleClient
 *  should run on.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 *  @param "-m corbaloc::yyy:xxxx/Manager" Use this optional parameter to specify where
 *  manager is.
 *  @htmlonly
    <br><hr>
    @endhtmlonly
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCLIENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLCLIENTWAVEDOC Client Wave
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This example shows a client that:
<ul>
 <li>logs into manager</li>
 <li>gets a MOUNT component</li>
 <li>spawns a thread</li>
 <li>calls the mount->objfix() command from within the thread setting the mount position in a sin() wave</li>
 <li>gets callback replies</li>
 <li>destroys the callback and releases the component</li>
 <li>logs out of manager</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (remote) components.</li>
  <li>Using callback classes for asynchronous methods.</li>
  <li>ACS thread usage.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientWave_8cpp.html">Client Wave File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */


/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include <acsexmplMountC.h>
#include "acsexmplCallbacks.h"
#include <acsThread.h>
#include <math.h>

ACE_RCSID(acsexmpl, acsexmpClientWave, "$Id: acsexmplClientWave.cpp,v 1.102 2007/02/01 05:14:26 cparedes Exp $")
using namespace ACS;
using namespace maci;

//ATTENTION:
//For the time being use global variable. Not clean.
int LENGTH     = 10;
int INTERVAL   = 1;
int ITERATIONS = 0;


/**
 * This is the worker function for the thread
 * that calls the objfix command
 * @param A ThreadBaseParameter which contains a reference to the component we need.
 * @return void
 * @htmlonly
   <br><hr>
   @endhtmlonly
 */
class WorkerThread : public ACS::Thread
{ 
  public:
    WorkerThread(const ACE_CString& name, 
		 MOUNT_ACS::Mount * mount, 
		 const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		 const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime),
	 myCallback("objfix")
	{
	    ACS_TRACE("WorkerThread::WorkerThread");
	    mount_p = mount;
	    cbv = myCallback._this();  
	}
    
    ~WorkerThread() { 
	ACS_TRACE("WorkerThread::~WorkerThread"); 
    }
    
    virtual void onStart() 
	{ 
	    az = 0; el = 0; count = 0; 
	 
	}

    virtual void runLoop()
	{
	    //while there are still values that need to be set...    
	    if (ITERATIONS<=0)
		exit();

	    ACS::CBDescIn desc;
	    
	    try
		{
		
		
		//perform a few miscellaneous operations on the azimuth and 
		//elevation the telescope will move to.
		az = sin(count * 0.157) * 200 - 100;
		el  = sin(count * 0.157) * 90;
		
		//move the telescope asynchronously
		ACS_SHORT_LOG((LM_INFO, "%s: Going to (%f,%f)", getName().c_str(),az,el));		
		mount_p->objfix(az, el, cbv.in(), desc);

		count = count++==10 ? 0 : count;
		//make sure we stop moving the telescope at some point.
		ITERATIONS--;   
		}
	    catch(...) 
		{
		ACS_SHORT_LOG((LM_ERROR,"Error!"));
		}

	    ACE_OS::fflush(stdout); 

	}


    virtual void onStop()
	{
	    ACS_SHORT_LOG((LM_INFO, "%s: OK", getName().c_str()));
	}

  private:
    MOUNT_ACS::Mount * mount_p;
    double az, el;
    int    count;
    MyCBvoid myCallback;
    ACS::CBvoid_var cbv;

}; 

/*******************************************************************************/

/** @cond
*/    
int main(int argc, char *argv[])
{
    
    //Checks command-line arguments.
    if (argc < 4)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <component name> <length of time for process to run> <time interval for monitor> <options>", argv[0]));
	return -1;
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));

	// Total amount of time the telescope will be operated on.
	LENGTH   = atoi(argv[2]);
	
	// Requested inteval for moving the telescope.
	INTERVAL = atoi(argv[3]);
	
	// Number of times we change the position of the telescope
	ITERATIONS = static_cast<int>(static_cast<double>(LENGTH) / static_cast<double>(INTERVAL));
	}

    //Creates and initialyses the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

    ACS_SHORT_LOG((LM_INFO, "Will:"));
    ACS_SHORT_LOG((LM_INFO, "  contact      : %s",  argv[1]));
    ACS_SHORT_LOG((LM_INFO, "  work for     : %ds", LENGTH));
    ACS_SHORT_LOG((LM_INFO, "  with interval: %ds", INTERVAL));
    ACS_SHORT_LOG((LM_INFO, "  # cmds sent  : %d", ITERATIONS));
    
    try
	{
	//Now gets the specific MOUNT we have requested on the command-line
	ACS_SHORT_LOG((LM_INFO, "Getting component: %s", argv[1]));
	MOUNT_ACS::Mount_var mount = client.getComponent<MOUNT_ACS::Mount>(argv[1], 0, true);	    
	
	//Prints the descriptor of the requested component
	ACS_SHORT_LOG((LM_DEBUG, "Requesting descriptor()... "));
	ACS::CharacteristicComponentDesc_var descriptor = mount->descriptor();
	ACS_SHORT_LOG((LM_DEBUG, "Got descriptor()."));
	ACS_SHORT_LOG((LM_INFO,"Descriptor:"));
	ACS_SHORT_LOG((LM_INFO,"\tname: %s", descriptor->name.in()));
	
	//create and spawn the thread for commands 
	MOUNT_ACS::Mount * mount_p = mount.ptr();

	WorkerThread thread_p("actionThread", // name of the thread
		                       mount_p,  // the mount parameter
                             ThreadBase::defaultResponseTime,
                             INTERVAL);
	// by default threads that are not created using a thread manager are creatd suspended so we have to resume them!!
	thread_p.resume();
	
	//Enter main loop and stays there for a fixed amount of time
	ACS_SHORT_LOG((LM_INFO, "Going in main loop for %ds...", LENGTH));
	ACE_Time_Value tv(LENGTH);
	client.run(tv);
	}
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	uex.log();
	return -1;
	}//try-catch
    
    //Another try section where we release our component and logout from manager
    try
	{
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.releaseComponent(argv[1]);	
	client.logout();
	}
    catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch(...)
	{
	ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, 
							"main");
	uex.log();
	return -1;
	}//try-catch
    

    //sleep for 3 sec to allow everytihng to cleanup and stabilyse
    //so that the tests can be determinitstic.
    ACE_OS::sleep(3);   
    return 0;
}
/** @endcond
*/    

/*___oOo___*/




