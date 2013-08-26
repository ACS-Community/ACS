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
*
*
* "@(#) $Id: acsexmplClientAlarmThread.cpp,v 1.110 2009/05/20 17:19:48 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-09-04 added ACE_TRY_CHECK on line 171 to get rid of warnings.
* david 2002-06-17 fixed client.init(argc,argv)
* naoj 2002-03-12 Added implementation of spawning thread in which value changes of current simulated
* gchiozzi 2002-02-13 Cleaned up and renamed
* gchiozzi 2002-01-23 Removed #define _POSIX_SOURCE 1. Gives problems on Solaris.
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* gchiozzi 2001-02-15 Merged: added tests for IFR and history.
* gchiozzi 2001-02-15 created standard header
* msekoran 2001-03-14 Changed namespace ESO to ACS 
* msekoran 2001-03-20 Added Alarmdouble (event) example
*/

/** @file acsexmplClientAlarmThread.cpp
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

/** @defgroup ACSEXMPLCLIENTALARMTHREADDOC Client Alarm Thread
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
This client creates a BACI alarm (basically using the callback pattern) on a read-only double BACI property. The values 
necessary for this alarm to "go off" are set using an ACS thread.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>SimpleClient usage.</li>
  <li>Using manager directly through SimpleClient.</li>
  <li>ACS logging mechanisms.</li>
  <li>Accessing (remote) components.</li>
  <li>Creating alarms on remote objects</li>
  <li>Creating ACS threads outside the context of components</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="acsexmplClientAlarmThread_8cpp.html">Client Alarm Thread File Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/
#include <maciSimpleClient.h>
#include <acsThread.h>
#include "acsexmplCallbacks.h"

#include <acsexmplPowerSupplyC.h>

ACE_RCSID(acsexmpl, acsexmplClientAlarmThread, "$Id: acsexmplClientAlarmThread.cpp,v 1.110 2009/05/20 17:19:48 javarias Exp $")

using namespace ACS;
using namespace maci;

/**
 * This is the worker thread that calls the objfix command.
 */
class WorkerThread : public ACS::Thread
{ 
	public:
		WorkerThread(const ACE_CString& name, 
			ACS::RWdouble_var curr, 
			const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
		    ACS::Thread(name, responseTime, sleepTime)
		{
		    ACS_TRACE("WorkerThread::WorkerThread");
		    current = curr;
		    count = 0;
		    numValues = 7;
		    values = new double[7];
		    values[0] = 50;
		    values[1] = 8;
		    values[2] = 15;
		    values[3] = 25;
		    values[4] = 995;
		    values[5] = 970;
		    values[6] = 50;

		    ACS_SHORT_LOG((LM_INFO, "%s: Created thread", getName().c_str()));
		}

		~WorkerThread() 
		{ 
			ACS_TRACE("WorkerThread::~WorkerThread"); 
			if(NULL != values) 
			{
				delete[] values;
			}
		}

		virtual void runLoop()
		{
			if(0 == count) {
				ACS_SHORT_LOG((LM_INFO, "%s: Started runLoop for thread", getName().c_str()));
			}
    		//while there are still values that need to be set...
			if(count < numValues)
			{
	    		try
				{
					//change the BACI property's value synchronously
					ACS_SHORT_LOG((LM_INFO, "%s: Setting current to %f", getName().c_str(), values[count]));
					current->set_sync(values[count]);
					count++;
				}
	    		catch(...)
				{
					ACS_SHORT_LOG((LM_ERROR,"Error!"));
				}
			}
			else {
    			setStopped();
    			ACS_SHORT_LOG((LM_INFO, "%s: Stopped thread", getName().c_str()));
			}
		}

	private:
		ACS::RWdouble_var current;
		int count;
    	int    numValues;
    	//Ranges in the CDB for readback are:
    	// alarm low  on : 10
    	// alarm low  off: 20
    	// alarm high off: 980
    	// alarm high on : 990
    	double * values; 
};

/*-----------------------------------------------------------------*/
//Now onto the real example...
/** @cond
*/    
int main(int argc, char *argv[]) 
{
    //Checks command-line arguments.
    if (argc < 2)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <component name> <options>", argv[0]));
	return -1;
	}
    else
	{
	ACS_SHORT_LOG((LM_INFO, "Welcome to %s!", argv[0]));
	}
    
    //Creates and initializes the SimpleClient object
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
    
    try
	{
	//Get reference to the component
	ACS_SHORT_LOG((LM_INFO, "Getting component: %s", argv[1]));
	PS::PowerSupply_var ps = client.getComponent<PS::PowerSupply>(argv[1],0,true);
	if (CORBA::is_nil(ps.in()) == true) 
	{
	   ACS_SHORT_LOG((LM_INFO, "Failed to get a reference to the PowerSupply component."));
	   return -1;
	}
	ACS_SHORT_LOG((LM_INFO, "PowerSupply reference retrieved."));

	// first, set the current to a value which won't generate an alarm when
	// we first add the alarm subscription. (The CDB schema for the power supply
	// defines the conditions under which an alarm will occur.)
	ps->current()->set_sync(50);
	
	//Get the BACI property we'll create an alarm for
	ACS_SHORT_LOG((LM_INFO, "Trying to get readback... "));
	ACS::ROdouble_var readback = ps->readback();
	ACS_SHORT_LOG((LM_INFO, "OK"));
	
	//create an instance of our alarm class
	MyAlarmdouble macb("readback");
	//activate it as a CORBA object
	ACS::Alarmdouble_var acb = macb._this(); 
	//create the actual BACI double alarm
	ACS::CBDescIn desc;
	ACS::Subscription_var alarmSub = readback->new_subscription_Alarm (acb.in(), desc);
	ACS_SHORT_LOG((LM_INFO,"Alarm subscription created")); 
	
	ACS::RWdouble_var current = ps->current();
	// create the thread 
	WorkerThread thread_p /*threadManager.create<WorkerThread,ACS::RWdouble_var>*/ ("actionThread", current, ThreadBase::defaultResponseTime, ThreadBase::defaultSleepTime*10 /*=1s*/);
	// by default threads that are not created using a thread manager are creatd suspended so we have to resume them!!
	thread_p.resume();

	//Enter main loop and stays there for a fixed amount of time.  Really we are
	//just allowing the thread we just created to run for awhile before exiting out
	//of this example.
	ACS_SHORT_LOG((LM_INFO, "(BACIClient main thread) Going in main loop sleep..."));
	ACE_Time_Value tv(20);
	client.run(tv);
	
	//Must cleanly destroy the alarm
	ACS_SHORT_LOG((LM_INFO,"Alarm subscription deleted")); 
	alarmSub->destroy();	
	} 
    catch(maciErrType::CannotGetComponentExImpl &_ex)
	{
	_ex.log();
	return -1;
	}
    catch(std::exception& e)
	{
		std::cout << e.what() << std::endl;
	ACS_SHORT_LOG((LM_ERROR,"Error!"));
	return -1;
	}
    
    //Another try section where we release our component and logout from manager
    try
	{
	ACS_SHORT_LOG((LM_INFO,"Releasing..."));
	client.releaseComponent( argv[1]);
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
    
    //sleep for 3 sec to allow everytihng to cleanup and stabilize
    //so that the tests can be determinitstic.
    ACE_OS::sleep(3);
    return 0;
}
/** @endcond
*/
    
