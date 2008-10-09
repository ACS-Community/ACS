/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) UNSPECIFIED - FILL IN, 2005 
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
* "@(#) $Id: acsncArchiveConsumer.cpp,v 1.10 2008/10/09 07:57:41 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-12  created 
*/



#include "acsncArchiveConsumer.h"
#include <acscommonC.h>

#define ALL_EVENT_TYPE "*"

static char *rcsId="@(#) $Id: acsncArchiveConsumer.cpp,v 1.10 2008/10/09 07:57:41 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace nc {
    //----------------------------------------------------------
    ArchiveConsumer::ArchiveConsumer(ArchiveHandlerSmartPtr handler) :
	Consumer(acscommon::ARCHIVING_CHANNEL_NAME),
	handler_m(handler)
    {
	subscribeAllEvents();
    }
    //---------------------------------------------------------- 
    ArchiveConsumer::ArchiveConsumer(CORBA::ORB_ptr orb_p,
				     ArchiveHandlerSmartPtr handler) :
	Consumer(acscommon::ARCHIVING_CHANNEL_NAME, orb_p),
	handler_m(handler)
    {
	subscribeAllEvents();
    }
    //----------------------------------------------------------
    ArchiveConsumer::ArchiveConsumer(int argc, 
				     char *argv[],
				     ArchiveHandlerSmartPtr handler) :
	Consumer(acscommon::ARCHIVING_CHANNEL_NAME, argc, argv),
	handler_m(handler)
    {
	subscribeAllEvents();
    }
    //---------------------------------------------------------- 
    /*
    * @throw CosEventComm::Disconnected
    */
    void
    ArchiveConsumer::push_structured_event(const CosNotification::StructuredEvent &se)
    {
	std::string eventName = CORBA::string_dup(se.header.fixed_header.event_name);
	std::string containerName = "";
	std::string deviceName    = "";
	std::string parameterName = "";

	//if the first element of eventName is not ':'
	if(eventName.at(0) != ':')
	    {
	    //then it must be the container name.
	    containerName = eventName.substr(0, eventName.find(':'));
	    }
	//remove container name and the ':'
	eventName = eventName.substr(eventName.find(':')+1);

	//get the device name
	deviceName = eventName.substr(0, eventName.find(':'));
	//remove device name and the ':'
	eventName = eventName.substr(eventName.find(':')+1);

	//whatever is left must be the property name
	parameterName = eventName;

	ACS::Time timeStamp = 0ULL;
	if((se.filterable_data[0].value >>= timeStamp) == false)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "ArchiveConsumer::push_structured_event(...) - failed to get the timestamp!"));
	    }
	
	try
	    {
	    handler_m->receive(timeStamp,
			       deviceName,
			       parameterName,
			       se.filterable_data[1].value);
	    }
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "ArchiveConsumer::push_structured_event(...) - failed to handle an event!"));
	    }
    }
    //---------------------------------------------------------- 
    const char* 
    ArchiveConsumer::getChannelKind()
    {
	return acscommon::ARCHIVING_CHANNEL_KIND;
    }
    //---------------------------------------------------------- 
    const char* 
    ArchiveConsumer::getChannelDomain()
    {
	//return acscommon::ARCHIVING_CHANNEL_DOMAIN;
	return "*";
    }
    //---------------------------------------------------------- 
    void
    ArchiveConsumer::subscribeAllEvents()
    {
	ACS_TRACE("ArchiveConsumer::subscribeAllEvents");
	init();
	
	CosNotification::EventTypeSeq added(1);
	CosNotification::EventTypeSeq removed(0);
	added.length(1);
	removed.length(0);
	
	added[0].domain_name = getChannelDomain();
	added[0].type_name   = ALL_EVENT_TYPE;
	
	try
	    {
	    consumerAdmin_m->subscription_change(added, removed);
	    } 
	catch(...)
	    {
	    ACS_SHORT_LOG((LM_ERROR,"ArchiveConsumer::subscribeAllEvents failed!"));
	    ACSErrTypeCommon::CORBAProblemExImpl err = ACSErrTypeCommon::CORBAProblemExImpl(__FILE__,__LINE__,"nc::ArchiveConsumer::subscribeAllEvents");
	    throw err.getCORBAProblemEx();
	    }	
    }
    //---------------------------------------------------------- 
};
/*___oOo___*/
