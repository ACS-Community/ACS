/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: acsncRTSupplierImpl.cpp,v 1.17 2010/04/29 20:00:45 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created 
*/
//-----------------------------------------------------------------------------
#include "acsncRTSupplier.h"
using namespace baci;
namespace nc {
//-----------------------------------------------------------------------------
RTSupplier::RTSupplier(const char* channelName, 
		       acscomponent::ACSComponentImpl* component) : 
    Supplier(channelName, component),
    threadManager_mp(0)
{
    ACS_TRACE("RTSupplier::RTSupplier");
    
    //spawn the thread that ends up publishing events
    threadManager_mp = new BACIThreadManager();
    threadManager_mp->create("worker", 
			      (void*)RTSupplier::worker, 
			      static_cast<void *>(this));
    threadManager_mp->resume("worker");
    rtSupGuardb = new Logging::RepeatGuardLogger<Logging::BaseLog>
        (30000000,50);
    rtSupGuardex = new Logging::RepeatGuardLogger<ACSErr::ACSbaseExImpl>
        (30000000,50);
}
//-----------------------------------------------------------------------------
void 
RTSupplier::disconnect()
{
    ACS_TRACE("RTSupplier::~RTSupplier");

    //Delete the thread manager
    if(threadManager_mp != 0)
	{
	threadManager_mp->stop("worker");
	delete threadManager_mp;
	threadManager_mp=0;
	}
		    
    //Now we rush through and publish any events still left in the queue
    //must use a mutex for any STL write operation
    eventQueueMutex_m.acquire();
    while(unpublishedEvents_m.empty() != true)
	{
	try
	    {
	    //publish the event.
	    publishEvent(unpublishedEvents_m.front().event);

	    //remove the last event
	    unpublishedEvents_m.pop();
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_ERROR,
			   "RTSupplier::disconnect() %s channel - problem publishing a saved event!",
			   channelName_mp));
	    }
	}
    //do NOT release mutex to ensure events can no longer be published
    //eventQueueMutex_m.release();

    //just call superclass disconnect
    Supplier::disconnect();
}

unsigned int RTSupplier::getQueueSize()
{
    ACE_Guard<ACE_Thread_Mutex> guard(eventQueueMutex_m);
    return unpublishedEvents_m.size();
}
//-----------------------------------------------------------------------------
void 
RTSupplier::worker(void* param_p)
{
    //sanity check
    if (param_p == 0) 
	{
	return;
	}
    
    //get access to ourself...
    BACIThreadParameter *baciParameter_p = static_cast<BACIThreadParameter *>(param_p);
    BACIThread *myself_p = baciParameter_p->getBACIThread();
    
    // Variables have to be passed explicitly
    RTSupplier *supplier_p = static_cast<RTSupplier *>(const_cast<void *>(baciParameter_p->getParameter()));
    
    if (BACIThread::InitThread != 0) 
	{
	BACIThread::InitThread("worker");
	}
    
    // Control loop
    while(myself_p->check() == true)
	{
	//there are unpublished events
        if((myself_p->isSuspended() == false) && (supplier_p->unpublishedEvents_m.empty() != true))
        {	
            //Now we rush through and publish any events in the queue
            while(supplier_p->unpublishedEvents_m.empty() != true)
            {
                try
                {
                    //no need for the mutex here (reads are thread safe)
                    supplier_p->publishEvent(supplier_p->unpublishedEvents_m.front().event);

                    //must use a mutex for any STL write operation
                    ACE_Guard<ACE_Thread_Mutex> guard(supplier_p->eventQueueMutex_m);//.acquire();
                    supplier_p->unpublishedEvents_m.pop();
                }
                catch(ACSErrTypeCommon::CORBAProblemEx &_ex)
                {
                    char strlog[1024];
                    Logging::Logger::LoggerSmartPtr logger = getLogger();
                    sprintf(strlog, " %s channel - problem publishing a saved event!",
                            supplier_p->channelName_mp);
                    supplier_p->rtSupGuardb->log(logger, Logging::Logger::LM_ERROR,
                            strlog, __FILE__, __LINE__, "RTSupplier::worker()");
                    ACSErrTypeCommon::CORBAProblemExImpl ex(_ex);
                    supplier_p->rtSupGuardex->log(ex);
                    supplier_p->unpublishedEvents_m.front().tries++;
                    //Use the callback to to notify the user that the message is dropped
                    if(supplier_p->unpublishedEvents_m.front().tries > 5){
                        struct unpublishedEventData data = 
                            supplier_p->unpublishedEvents_m.front();
                        supplier_p->unpublishedEvents_m.pop();
                        if(data.callback != NULL){
                            ::CORBA::Any event = data.event.filterable_data[0].value;
                            data.callback->eventDropped(&event);
                        }
                    }
                }
                catch(CORBA::SystemException &ex)
                {
                    //tbd: we have to improve here the erro handling. Now we print out more that is necessary
                    char strlog[1024];
                    Logging::Logger::LoggerSmartPtr logger = getLogger();
                    sprintf(strlog, " %s channel - problem publishing a saved event!",
                            supplier_p->channelName_mp);
                    supplier_p->rtSupGuardb->log(logger, Logging::Logger::LM_ERROR,
                            strlog, __FILE__, __LINE__, "RTSupplier::worker()");
                    ACSErrTypeCommon::CORBAProblemExImpl corbaProblemEx(__FILE__, __LINE__,
                            "RTSupplier::worker()");
                    corbaProblemEx.setMinor(ex.minor());
                    corbaProblemEx.setCompletionStatus(ex.completed());
                    corbaProblemEx.setInfo(ex._info().c_str());
                    supplier_p->rtSupGuardex->log(corbaProblemEx);
                    supplier_p->unpublishedEvents_m.front().tries++;
                    //Use the callback to to notify the user that the message is dropped
                    if(supplier_p->unpublishedEvents_m.front().tries > 5){
                        struct unpublishedEventData data = 
                            supplier_p->unpublishedEvents_m.front();
                        supplier_p->unpublishedEvents_m.pop();
                        if(data.callback != NULL){
                            ::CORBA::Any event = data.event.filterable_data[0].value;
                            data.callback->eventDropped(&event);
                        }
                    }
                }
                catch(...)
                {
                    char strlog[1024];
                    Logging::Logger::LoggerSmartPtr logger = getLogger();
                    sprintf(strlog, " %s channel - problem publishing a saved event!",
                            supplier_p->channelName_mp);
                    supplier_p->rtSupGuardb->log(logger, Logging::Logger::LM_ERROR,
                            strlog, __FILE__, __LINE__, "RTSupplier::worker()");
                    supplier_p->unpublishedEvents_m.front().tries++;
                    //Use the callback to to notify the user that the message is dropped
                    if(supplier_p->unpublishedEvents_m.front().tries > 5){
                        struct unpublishedEventData data = 
                            supplier_p->unpublishedEvents_m.front();
                        supplier_p->unpublishedEvents_m.pop();
                        if(data.callback != NULL){
                            ::CORBA::Any event = data.event.filterable_data[0].value;
                            data.callback->eventDropped(&event);
                        }
                    }
                }
            }
        }
        myself_p->sleep();
    }
    
    if (BACIThread::DoneThread != 0) 
    {
        BACIThread::DoneThread();
    }
    delete baciParameter_p;
    myself_p->setStopped();
}
//-----------------------------------------------------------------------------
RTSupplier::~RTSupplier(){
    delete rtSupGuardb;
    delete rtSupGuardex;
}
//----------------------------------------------------------------------
 }; 






