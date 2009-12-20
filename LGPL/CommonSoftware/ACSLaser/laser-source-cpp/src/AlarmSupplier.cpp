/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005 
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
* "@(#) $Id: AlarmSupplier.cpp,v 1.12 2009/12/20 15:08:37 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-11-15  created
* sharring 2005-11-22  documented
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*   
* 
*   DESCRIPTION
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"

#include "AlarmSupplier.h"
#include "ACSJMSMessageEntityC.h"
#include "acsutilTimeStamp.h"
#include <string>
#include <acsncC.h>
#include <acsncErrType.h>
#include <ACSErrTypeCORBA.h>

using acsalarm::ASIMessage;
using std::string;

/**
 * Constructor.
 * @param channelName the name of the channel to use for publishing events
 */
AlarmSupplier::AlarmSupplier(const char* channelName) :
    BaseSupplier(channelName, acsnc::ALARMSYSTEM_DOMAIN_NAME)
{
	ACS_TRACE("AlarmSupplier::AlarmSupplier()");
}

/**
 * Destructor.
 */
AlarmSupplier::~AlarmSupplier()
{
	ACS_TRACE("AlarmSupplier::~AlarmSupplier()");
}


/*
 * Method to publish an event over CORBA notification channel.
 * @param msg the ASIMessage to publish.
 */
void AlarmSupplier::publishEvent(ASIMessage &msg)
{ 
	ACS_TRACE("AlarmSupplier::publishEvent()");

	CosNotification::StructuredEvent event;
	populateHeader(event);    

	// populate event's description; while this isn't needed per se (as in, it isn't really used anywhere),
	// java consumers cannot receive events if this info isn't populated.
	acsnc::EventDescription description;
	description.timestamp = getTimeStamp();
	description.count = 0;
	description.name = "AlarmSupplier";
	event.remainder_of_body <<= description;

	// populate event's filterable data with XML representation of the alarm
	event.filterable_data.length(1);
	com::cosylab::acs::jms::ACSJMSMessageEntity msgForNotificationChannel;
	string xmlToSend = msg.toXML();
	string xmlToLog = "AlarmSupplier::publishEvent()\n\nAbout to send XML of: \n\n" + xmlToSend + "\n\n";
	ACS_SHORT_LOG((LM_TRACE, xmlToLog.c_str()));
	msgForNotificationChannel.text = xmlToSend.c_str();
	event.filterable_data[0].value <<= msgForNotificationChannel;
	
	ACS_SHORT_LOG((LM_DEBUG,"AlarmSupplier::publishEvent(): Preparing to send XML."));
	try{
		BaseSupplier::publishEvent(event);
	}
	catch(ACSErrTypeCORBA::CORBAReferenceNilExImpl& ex1)
	{
		acsncErrType::PublishEventFailureExImpl 
			ex2 (__FILE__, __LINE__, "AlarmSupplier::publishEvent");
		ex2.setEventName("event");
		ex2.setChannelName(channelName_mp);
		throw ex2;
	}
	catch(ACSErrTypeCORBA::NarrowFailedExImpl& ex1)
	{
		acsncErrType::PublishEventFailureExImpl 
			ex2 (__FILE__, __LINE__, "AlarmSupplier::publishEvent");
		ex2.setEventName("event");
		ex2.setChannelName(channelName_mp);
		throw ex2;
	}
	catch(ACSErrTypeCORBA::FailedToResolveServiceExImpl& ex1)
	{
		acsncErrType::PublishEventFailureExImpl 
			ex2 (__FILE__, __LINE__, "AlarmSupplier::publishEvent");
		ex2.setEventName("event");
		ex2.setChannelName(channelName_mp);
		throw ex2;
	}
	//ACS_SHORT_LOG((LM_DEBUG,"AlarmSupplier::publishEvent(): Sent XML."));
}
