#ifndef RT_SUPPLIER_I
#define RT_SUPPLIER_I
/*    @(#) $Id: acsncRTSupplier.i,v 1.24 2010/04/29 20:00:45 javarias Exp $
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
 */

/** @file acsncSimpleSupplier.i
 *  Header file for the real-time Supplier class implementation.
 */

namespace nc {
//----------------------------------------------------------------------    
template <class T> void
RTSupplier::publishData(T data, EventProcessingCallback *evProcCallback)
{
    try
	{
	//acquire the mutex first
	ACE_Guard<ACE_Thread_Mutex> guard(eventQueueMutex_m);//.acquire();
	
	if (unpublishedEvents_m.size()>10000)
	    {
	    // here the callback handles the error
        if(evProcCallback != NULL){
            any_m <<= data;
            evProcCallback->queueIsFull(&any_m);
            return;
        }
        //classical approach
        //this should throw a different exception
        else{
	        char buf[100];
	        ACSErrTypeCommon::CORBAProblemExImpl ex(__FILE__, __LINE__, "RTSupplier<>::publishData");
            sprintf(buf, "RTSupplier sueue size exceed 10000 (%d)", unpublishedEvents_m.size());
	        ex.setInfo(buf);
	        throw ex.getCORBAProblemEx();
        }
	    }
	//convert user data to an any
	any_m <<= data;
	//"fill out" the entire structured event
	populateHeader(any_m);
	
	//save the event in our list
    struct unpublishedEventData data;
    data.event = event_m;
    data.callback = evProcCallback;
    data.tries = 0;
	unpublishedEvents_m.push(data);
	
	//done...release it
//	eventQueueMutex_m.release();
	}
    catch(ACSErrTypeCommon::CORBAProblemEx)
	{
	//an exception from subclasses...OK to rethrow
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"RTSupplier::publishData() %s channel - unknown error!",
		       channelName_mp));
	ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"nc::RTSupplier::publishData");
	throw err;
	}
}
//----------------------------------------------------------------------
 }; 
#endif
