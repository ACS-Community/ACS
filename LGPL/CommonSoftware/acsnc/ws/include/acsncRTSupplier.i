#ifndef RT_SUPPLIER_I
#define RT_SUPPLIER_I
/*    @(#) $Id: acsncRTSupplier.i,v 1.20 2007/01/25 10:31:34 bjeram Exp $
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
RTSupplier::publishData(T data)
    throw (CORBAProblemEx)
{
    try
	{
       
	//acquire the mutex first
	ACE_Guard<ACE_Thread_Mutex>  guard(eventQueueMutex_m);//.acquire();

/*	if (unpublishedEvents_m.size()>10000)
	    {
	    printf ("\n============>  Queue size exceed 10000 (%d) !!! <===========\n\n", unpublishedEvents_m.size());
	    }
*/
	//convert user data to an any
	any_m <<= data;
	//"fill out" the entire structured event
	populateHeader(any_m);
	
	//save the event in our list
	unpublishedEvents_m.push(event_m);
	
	//done...release it
//	eventQueueMutex_m.release();
	}
    catch(CORBAProblemEx)
	{
	//an exception from subclasses...OK to rethrow
	throw;
	}
    catch(...)
	{
	ACS_SHORT_LOG((LM_ERROR,"RTSupplier::publishData() %s channel - unknown error!",
		       channelName_mp));
	CORBAProblemExImpl err = CORBAProblemExImpl(__FILE__,__LINE__,"nc::RTSupplier::publishData");
	throw err;
	}
}
//----------------------------------------------------------------------
 }; 
#endif
