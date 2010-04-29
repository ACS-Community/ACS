#ifndef SIMPLE_SUPPLIER_I
#define SIMPLE_SUPPLIER_I
/*    @(#) $Id: acsncSimpleSupplier.i,v 1.30 2010/04/29 20:00:45 javarias Exp $
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
 *  Header file for the Supplier-derived class that should be used from within
 *  components to publish events.
 *  @exception acsncErrType::PublishEventFailureExImpl
 */
#include <acsncErrType.h>

namespace nc {
//----------------------------------------------------------------------
template <class T> void
SimpleSupplier::publishData(T data, EventProcessingCallback<T> *evProcCallback)
{
	try
	{
		any_m <<= data;
		Supplier::publishEvent(any_m);
      if(evProcCallback != NULL)
         evProcCallback->eventSent(data);
	}
   catch(CORBA::TRANSIENT &ex)
   {
      if(evProcCallback != NULL)
         evProcCallback->eventStoredInQueue(data);
   }
   catch(EventDroppedException &ex)
   {
      if(evProcCallback != NULL){
         evProcCallback->eventDropped(data);
         evProcCallback->eventStoredInQueue(data);
      }
   }
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		acsncErrType::PublishEventFailureExImpl nex(ex, __FILE__, __LINE__, "nc::Supplier::publishData");
		nex.setChannelName(channelName_mp);
		nex.log(LM_DEBUG);
		throw nex;
	}
	catch(CORBA::SystemException &ex)
	{
		ACSErrTypeCommon::CORBAProblemExImpl cex(__FILE__,
				__LINE__,
				"nc::SimpleSupplier::publishData");
		cex.setMinor(ex.minor());
		cex.setCompletionStatus(ex.completed());
		cex.setInfo(ex._info().c_str());

		acsncErrType::PublishEventFailureExImpl ex(cex, __FILE__, __LINE__, "nc::Supplier::publishData");
		ex.setChannelName(channelName_mp);
		ex.log(LM_DEBUG);
		throw ex;
	}
	catch(...)
	{
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__, "nc::Supplier::publishData");

		acsncErrType::PublishEventFailureExImpl ex(uex, __FILE__, __LINE__, "nc::Supplier::publishData");
		ex.setChannelName(channelName_mp);
		ex.log(LM_DEBUG);
		throw ex;
	}//try-catch
}//publishData
//----------------------------------------------------------------------
 };
#endif
