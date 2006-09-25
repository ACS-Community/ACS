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
* "@(#) $Id: AlarmSupplier.cpp,v 1.2 2006/09/25 08:52:37 acaproni Exp $"
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
#include <logging.h>
#include <string>

static char *rcsId="@(#) $Id: AlarmSupplier.cpp,v 1.2 2006/09/25 08:52:37 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

/**
 * Constructor.
 * @param channelName the name of the channel to use for publishing events
 */
AlarmSupplier::AlarmSupplier(const char* channelName) :
    BaseSupplier(channelName)
{
   //no-op
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::AlarmSupplier(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::AlarmSupplier(): exiting.");
}

/**
 * Destructor.
 */
AlarmSupplier::~AlarmSupplier()
{
   //no-op
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::~AlarmSupplier(): entering.");
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::~AlarmSupplier(): exiting.");
}


/*
 * Method to publish an event over CORBA notification channel.
 * @param msg the ASIMessage to publish.
 */
void AlarmSupplier::publishEvent(laserSource::ASIMessage &msg)
{ 
	Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::publishEvent(): entering.");

	CosNotification::StructuredEvent event;
	populateHeader(event);    
	event.filterable_data.length(1);

	com::cosylab::acs::jms::ACSJMSMessageEntity msgForNotificationChannel;
	string xmlToSend = msg.toXML();

	string xmlToLog = "AlarmSupplier::publishEvent()\n\nAbout to send XML of: \n\n" + xmlToSend + "\n\n";
	myLoggerSmartPtr->log(Logging::Logger::LM_DEBUG, xmlToLog);

	msgForNotificationChannel.text = CORBA::string_dup(xmlToSend.c_str());
    
	event.filterable_data[0].value <<= msgForNotificationChannel;
	
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::publishEvent(): Preparing to send XML.");
	BaseSupplier::publishEvent(event);
	myLoggerSmartPtr->log(Logging::Logger::LM_TRACE, "AlarmSupplier::publishEvent(): Sent XML.");
}
