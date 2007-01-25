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
* "@(#) $Id: acsncRTSupplierImpl.cpp,v 1.11 2007/01/25 10:31:34 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david  20/09/02  created 
*/
//-----------------------------------------------------------------------------
#include "acsncRTSupplier.h"
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
	    publishEvent(unpublishedEvents_m.front());

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
		    supplier_p->publishEvent(supplier_p->unpublishedEvents_m.front());

		    //must use a mutex for any STL write operation
		    ACE_Guard<ACE_Thread_Mutex> guard(supplier_p->eventQueueMutex_m);//.acquire();
		    supplier_p->unpublishedEvents_m.pop();
//		    supplier_p->eventQueueMutex_m.release();
		    }
		catch(...)
		    {
		    ACS_SHORT_LOG((LM_ERROR,"RTSupplier::worker() %s channel - problem publishing a saved event!",
				   supplier_p->channelName_mp));
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
RTSupplier::~RTSupplier(){;}
//----------------------------------------------------------------------
 }; 






