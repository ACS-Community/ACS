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

#ifndef LOGGING_ACSLOGGINGEVENT_H_
#define LOGGING_ACSLOGGINGEVENT_H_

#define LOG4CPP_FIX_ERROR_COLLISION 1
#include <iostream>
#include <log4cpp/LoggingEvent.hh>

namespace logging{
struct ACSLoggingEvent: public ::log4cpp::LoggingEvent{

	/**
	 * Name of the routine which generate the log.
	 */
	::std::string routineName;
	/**
	 *  Name of the file which generate the routine (__FILE__)
	 */
	::std::string fileName;
	unsigned int line;
	/**
	 * Name of the host, it should be used ACE_OS::hostname() to retrieve the name
	 */
	::std::string hostName;
	::std::string contextName;
	::std::string audienceName;
	::std::string sourceObject;
	::std::string array;
	::std::string antenna;
	::std::string stackId;
	int stackLevel;
	::std::string uri;

	ACSLoggingEvent();

	ACSLoggingEvent(const std::string& logger, const std::string& message,
			::log4cpp::Priority::Value priority, const std::string& routine,
			const std::string& file, unsigned int line,
			const std::string& host, const std::string& context,
			const std::string& audience, const std::string& sourceObject,
			const std::string& array, const std::string& antenna,
			const std::string& stackId, const int stackLevel,
			const std::string& uri);
};
}

#endif /* ACSLOGGINGEVENT_H_ */
