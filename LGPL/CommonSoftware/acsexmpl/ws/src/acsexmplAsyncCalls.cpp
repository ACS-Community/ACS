/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acsexmplAsyncCalls.cpp,v 1.8 2008/07/10 10:00:37 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-08-09  created
*/

/** @file ACSEXMPLCLIENTASYNCCALLSDOC acsexmplAsynCalls
 * @htmlonly
 * <br><br>
 * @endhtmlonly
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCLIENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLCLIENTASYNCCALLSDOC Async Calls
 *  @{
 * @htmlonly
 <h2>Description</h2>
 This is is a client that uses asynchronous calls.
 It access a MOUNT component to move the antenna to a definite position.
 The movement is asynchronous
 <br>

 <h2>What can I gain from this example?</h2>
 <ul>
  	<li>Simple client usage</li>
  	<li>asynchronous calls to read property</li>
   <li>asynchronous execution of methods </li>
   <li>Usage of baci threads</li>
  </ul>
  @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

#include <maciSimpleClient.h>
#include <baciThread.h>
#include "acsexmplMountC.h"
#include "acsexmplAsyncCallbacks.h"
#include "acsexmplAsyncMethodCB.h"
#include <iostream>
#include <string>

static char *rcsId="@(#) $Id: acsexmplAsyncCalls.cpp,v 1.8 2008/07/10 10:00:37 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace maci;
using namespace baci;

typedef struct threadParamStruct {
	MOUNT_ACS::Mount* mount;
	double az;
	double el;
} ThreadParamStruct;

/**
 * The thread to read the actual azimuth and elevation
 * The reads are synchronous
 *
 */
static void
worker(void *threadParam_p) {

	if (threadParam_p==NULL) {
		ACS_SHORT_LOG((LM_ERROR,"Invalid parameter for thread!"));
		return;
	}

	//Get access to the BACI thread this function is being executed from
    BACIThreadParameter *baciParameter_p = static_cast<BACIThreadParameter *>(threadParam_p);
    BACIThread *myself_p = baciParameter_p->getBACIThread();

	// Init the thread
    if (BACIThread::InitThread != 0)  {
		BACIThread::InitThread("Position thread");
	}

    ACS_SHORT_LOG((LM_INFO,"Thread started with name %s",myself_p->getName().c_str()));

    // Get the struct from the parameter
    ThreadParamStruct* param=(ThreadParamStruct*)baciParameter_p->getParameter();

    // Get the reference to the actAz property of the mount
    ACS::ROdouble_var actAz = param->mount->actAz();
    if (CORBA::is_nil(actAz)) {
    	ACS_SHORT_LOG((LM_ERROR,"ERROR getting actAz"));
    	return;
    }

    // Get the reference to the actEl property of the mount
    ACS::ROdouble_var actEl = param->mount->actEl();
    if (CORBA::is_nil(actEl)) {
    	ACS_SHORT_LOG((LM_ERROR,"ERROR getting actEl"));
    	return;
    }

    // The az (azimuth) and el (elevation) are declared here
    // but they are both updated in the callback
	double az,el;
	ACSErr::Completion_var completion;

	// Create the structs for the async calls to read the values of az
	AsyncCBdouble azCallback("actAz",&az);
	ACS::CBdouble_var actAz_CB = azCallback._this();
	ACS::CBDescIn actAzDescIn;
	// Create the structs for the async calls to read the values of az
	AsyncCBdouble elCallback("actEl",&el);
	ACS::CBdouble_var actEl_CB = elCallback._this();
	ACS::CBDescIn actElDescIn;

	// Create the callback for the objfix method
	AsyncMethodCBvoid objfixCB("objfix");
	ACS::CBvoid_var objfix_CB = objfixCB._this();
	ACS::CBDescIn objfixDescIn;

	// Times handshaking is not yet implemented so
	// we do not really need to fill these fields
	//objfixDescIn.normal_timeout=10000000;
	//objfixDescIn.negotiable_timeout=5000000;

	CORBA::Double newAz=(CORBA::Double)param->az;
	CORBA::Double newEl=(CORBA::Double)param->el;

	ACS_SHORT_LOG((LM_INFO,"Calling the asynchronous objfix"))

	param->mount->objfix(newAz,newEl,objfix_CB.in(),objfixDescIn);

	while(myself_p->check() == true) {

		if(myself_p->isSuspended() == false) {
			ACS_SHORT_LOG((LM_INFO,"Calling the async methods to read Az and El"))
			actAz->get_async(actAz_CB.in(),actAzDescIn);
			actEl->get_async(actEl_CB.in(),actElDescIn);
		} else {
			myself_p->sleep();
		}

		sleep(1);
	}

	ACS_SHORT_LOG((LM_INFO,"Exiting thread"))
	sleep(5);
	if (BACIThread::DoneThread != 0) {
		BACIThread::DoneThread();
	}

    delete baciParameter_p;
    myself_p->setStopped();
}

/** @cond
*/
int main(int argc, char* argv[])
{
    MOUNT_ACS::Mount_var mount;
    BACIThreadManager threadManager;   // The thread manager is used to manage the thread
    ThreadParamStruct param; // The parameter for the thread

    // Check the arguments in the command line
	if (argc<2) {
		std::cerr<<"Usage: "<<argv[0]<<" component azimuth elevation <options>\n";
		return -1;
	}

	double destAz, destEl;
	if (sscanf(argv[2],"%lf",&destAz)+sscanf(argv[3],"%lf",&destEl)!=2) {
		std::cerr<<"Format error in azimuth and/or elevation"<<std::endl;
		std::cerr<<"Usage: "<<argv[0]<<" component azimuth elevation <options>\n";
		return -1;
	}

	// Create the SimpleClient object
	SimpleClient mountClient;

	// Init the client
	if (mountClient.init(argc,argv)==0) {
		// Error initing
		ACS_SHORT_LOG((LM_ERROR,"Error initing the client"));
		return -1;
	} else {
		ACS_SHORT_LOG((LM_INFO,"SimpleClient built"));
	}

	// Login the client
	if (mountClient.login()==0) {
		//Error
		ACS_SHORT_LOG((LM_ERROR,"Error logging in the client"));
		return -1;
	} else {
		ACS_SHORT_LOG((LM_INFO,"Client logged in"));
	}

	try
	    {
	    // Get the component
	    ACS_SHORT_LOG((LM_INFO,"Getting component %s",argv[1]));
	    mount = mountClient.getComponent<MOUNT_ACS::Mount>(argv[1], 0, true);
	    }
	catch(maciErrType::CannotGetComponentExImpl &_ex)
	    {
	    _ex.log();
	    return -1;
	    }

	param.mount=mount.ptr();
	param.az=destAz;
	param.el=destEl;

	char logStr[128];
	sprintf(logStr,"Commanded position az=%lf, el=%lf",destAz,destEl);
	ACS_SHORT_LOG((LM_INFO,logStr));

	// Start the thread to read the position of the antenna
	ACS_SHORT_LOG((LM_INFO,"Starting the thread"));
	BACIThread* thread_p = threadManager.create("Position thread",    //Name of the new thread
		(void *)worker,    //Function to run inside the thread
		static_cast<void *>(&param));    //the single parameter

	if (thread_p==NULL)  {
		ACS_SHORT_LOG((LM_ERROR,"Error in spawning thread"));
	}
        thread_p->resume();

	// The thread will run for 30 secs so we wait until it finishes
	// There are better (and safer) solution to wait instead of a simple wait
	// but... this is the faster one!
	sleep(40);

	// End of computation: begin to clean up

	// Stop all the threads
	ACS_SHORT_LOG((LM_INFO,"Terminating the thread"));
	threadManager.terminateAll();

	// Release the component
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Releasing %s",argv[1]));
	    mountClient.releaseComponent(argv[1]);
	    }
	catch(maciErrType::CannotReleaseComponentExImpl &_ex)
	    {
	    _ex.log();
	    return -1;
	    }

	// logout the client
	try
	    {
	    ACS_SHORT_LOG((LM_INFO,"Logging out"));
	    mountClient.logout();
	    }
	catch (...)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"Error logging out the simple client object"));
	    ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,
							    "main");
	    uex.log();
	    return -1;
	    }//try-catch

	ACS_SHORT_LOG((LM_INFO,"Done"));

	return 0;
}
/** @endcond
*/

