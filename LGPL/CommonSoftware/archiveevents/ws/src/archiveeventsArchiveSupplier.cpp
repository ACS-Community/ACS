/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
* "@(#) $Id: archiveeventsArchiveSupplier.cpp,v 1.6 2009/05/20 17:19:48 javarias Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/

#include <acscommonC.h>
#include <acsncErrType.h>
#include <ACSErrTypeCORBA.h>
#include "archiveeventsArchiveSupplier.h"
//-----------------------------------------------------------------------------
ArchiveSupplier::ArchiveSupplier() :
    BaseSupplier(acscommon::ARCHIVING_CHANNEL_NAME)
{
    //no-op
}
//-----------------------------------------------------------------------------
ArchiveSupplier::~ArchiveSupplier()
{
    //TBD - should we really disconnect here
    //this->disconnect();
}
//-----------------------------------------------------------------------------
void 
ArchiveSupplier::send_event(CORBA::Short priority,
			    ACS::Time timeStamp,
			    const std::string& component,
			    const std::string& property,
			    CORBA::Any value,
			    const std::string& container)
    
{
    //would save space to make this a member variable but there could
    //be problems with multi-threaded apps
    CosNotification::StructuredEvent archiving_event;
    populateHeader(archiving_event);

    //the eventName consists of container named concatenated with the
    //component and property names, delimited by ':'s.
    const char *separator = ":";
    ACE_CString eventName = container.c_str();
    eventName += separator; 
    eventName += component.c_str();
    eventName += separator;
    eventName += property.c_str();
    //std::string eventName = container + ":" + component + ":" + property;
    
    //save a CORBA any by placing the container/component/property names in the event_name
    archiving_event.header.fixed_header.event_name = CORBA::string_dup(eventName.c_str());

    //there are only two elements within filterable_data: the time_stamp and the value
    archiving_event.filterable_data.length(2);
    archiving_event.filterable_data[0].name = CORBA::string_dup("time_stamp");
    archiving_event.filterable_data[0].value <<= timeStamp;
    archiving_event.filterable_data[1].name = CORBA::string_dup("value");
    archiving_event.filterable_data[1].value = value;
    
    //delegate to another method which will actually send the event
	 try{
		 this->publishEvent(archiving_event);
	 }
	 catch(ACSErrTypeCORBA::CORBAReferenceNilExImpl& ex1)
	 {
		 acsncErrType::PublishEventFailureExImpl
			 ex2 (__FILE__, __LINE__, "ArchiveSupplier::send_event");
		 ex2.setEventName("archiving_event");
		 ex2.setChannelName(channelName_mp);
		 throw ex2;
	 }
	 catch(ACSErrTypeCORBA::NarrowFailedExImpl& ex1)
	 {
		 acsncErrType::PublishEventFailureExImpl
			 ex2 (__FILE__, __LINE__, "ArchiveSupplier::send_event");
		 ex2.setEventName("archiving_event");
		 ex2.setChannelName(channelName_mp);
		 throw ex2;
	 }
	 catch(ACSErrTypeCORBA::FailedToResolveServiceExImpl& ex1)
	 {
		 acsncErrType::PublishEventFailureExImpl
			 ex2 (__FILE__, __LINE__, "ArchiveSupplier::send_event");
		 ex2.setEventName("archiving_event");
		 ex2.setChannelName(channelName_mp);
		 throw ex2;
	 }
}
//-----------------------------------------------------------------------------
