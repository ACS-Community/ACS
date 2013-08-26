/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
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
 *
 * "@(#) "
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * javarias  May 6, 2010  	 created
 */

#include "loggingStdoutlayout.h"
#include "loggingACSLoggingEvent.h"

#include <log4cpp/LoggingEvent.hh>
#include <log4cpp/Priority.hh>
#include <iostream>

using namespace logging;

std::string ACSstdoutLayout::format(const ::log4cpp::LoggingEvent& event) {
	::std::string retStr = "";
	const ACSLoggingEvent* acsLogEvent =
			static_cast<const ACSLoggingEvent *> (&event);
	ACE_Time_Value time(event.timeStamp.getSeconds(),
			event.timeStamp.getMicroSeconds() * 1000
					+ event.timeStamp.getMilliSeconds());
	ACE_TCHAR timestamp[24];
	//TODO: should this conversion be improved to remove the ACE dependency?
	ACSstdoutLayout::formatISO8601inUTC(time, timestamp);
	retStr += timestamp;
	if (event.priority <= ::log4cpp::Priority::INFO)
		retStr += " [" + event.categoryName + "] ";
	else
		retStr += " [" + event.categoryName + " - " + acsLogEvent->routineName
				+ "] ";
	retStr += event.message + "\n";
	return retStr;
}

//Recycled method from loggingLoggingProxy
void ACSstdoutLayout::formatISO8601inUTC(const ACE_Time_Value &timestamp,
		ACE_TCHAR str[]) {
	ACE_TCHAR ctp[20];
	time_t ut(timestamp.sec());
	struct tm *utc = ACE_OS::gmtime(&ut);
	ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H:%M:%S", utc);

	ACE_OS::sprintf(str, ACE_TEXT("%s.%03ld"), ctp, timestamp.usec() / 1000);
}

ACSstdoutLayout::~ACSstdoutLayout() {
}
