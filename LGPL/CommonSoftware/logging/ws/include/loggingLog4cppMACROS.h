/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) Associated Universities Inc., 2005
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
* "@(#) $Id: loggingLog4cppMACROS.h,v 1.1 2011/02/14 21:15:08 javarias Exp $"
*/

#ifndef LOGGING_LOG4CPP_MACROS_H_
#define LOGGING_LOG4CPP_MACROS_H_

#include "loggingLog4cpp.h"
#include "loggingACSLoggingEvent.h"

#define LOG4CPP_LOG(priority, routine, text) \
	LOGGER_FACTORY->getGlobalLogger()->log(text, priority, routine, __FILE__, __LINE__, \
			"", "", "", "", "", "", "", 0, "");

#define LOG4CPP_LOG_FULL(logPriority, logRoutine, logMessage, logAudience, logArray, logAntenna) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logPriority,logRoutine , __FILE__, __LINE__, \
				"", "", logAudience, "", logArray, logAntenna, "", 0, "");

#define LOG4CPP_LOG_WITH_ANTENNA_CONTEXT(logPriority, logRoutine, logMessage, logArray, logAntenna) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logPriority,logRoutine , __FILE__, __LINE__, \
				"", "", "", "", logArray, logAntenna, "", 0, "");

#define LOG4CPP_LOG_TO_AUDIENCE(logPriority, logRoutine, logMessage, logAudience) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logPriority,logRoutine , __FILE__, __LINE__, \
				"", "", logAudience, "", "", "", "", 0, "");

#define LOG4CPP_LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logSource) \
		LOGGER_FACTORY->getLogger(logSource)->log(logMessage, logPriority,logRoutine , logFile, logLine, \
				"", "", "", "", "", "", "", 0, "");

#define LOG4CPP_LOG_GLOBAL_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logPriority,logRoutine , logFile, logLine, \
				"", "", "", "", "", "", "", 0, "");

#define LOG4CPP_STATIC_LOG(priority, routine, text) \
		LOGGER_FACTORY->getStaticLogger()->log(text, priority, routine, __FILE__, __LINE__, \
				"", "", "", "", "", "", "", 0, "");

#define LOG4CPP_STATIC_LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
		LOGGER_FACTORY->getStaticLogger()->log(logMessage, logPriority,logRoutine , logFile, logLine, \
				"", "", "", "", "", "", "", 0, "");

#define LOG4CPP_STATIC_LOG_TO_AUDIENCE(priority, routine, text, logAudience) \
		LOGGER_FACTORY->getStaticLogger()->log(text, priority, routine, __FILE__, __LINE__, \
				"", "", logAudience, "", "", "", "", 0, "");

//TODO: Re-implement AUTO TRACE MACROS

#endif
