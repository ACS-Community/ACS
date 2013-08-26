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
* "@(#) $Id: loggingLog4cppMACROS.h,v 1.3 2011/03/23 23:27:48 javarias Exp $"
*/

#ifndef LOGGING_MACROS_H_
#define LOGGING_MACROS_H_

#ifdef ENABLE_LOG4CPP_MACROS

#include "loggingLog4cpp.h"
#include "loggingACSLoggingEvent.h"

#include <acscommonC.h>

#include <memory>

#define LOG(priority, routine, text) \
	LOGGER_FACTORY->getGlobalLogger()->log(text, priority, routine, __FILE__, __LINE__, \
			"", "", "", "", "", "", "", 0, "");

#define LOG_FULL(logPriority, logRoutine, logMessage, logAudience, logArray, logAntenna) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logging::convertPriority(logPriority), logRoutine , __FILE__, __LINE__, \
				"", "", logAudience, "", logArray, logAntenna, "", 0, "");

#define LOG_WITH_ANTENNA_CONTEXT(logPriority, logRoutine, logMessage, logArray, logAntenna) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logging::convertPriority(logPriority), logRoutine , __FILE__, __LINE__, \
				"", "", "", "", logArray, logAntenna, "", 0, "");

#define LOG_TO_AUDIENCE(logPriority, logRoutine, logMessage, logAudience) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logging::convertPriority(logPriority), logRoutine , __FILE__, __LINE__, \
				"", "", logAudience, "", "", "", "", 0, "");

#define LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logSource) \
		LOGGER_FACTORY->getLogger(logSource)->log(logMessage, logging::convertPriority(logPriority), logRoutine , logFile, logLine, \
				"", "", "", "", "", "", "", 0, "");

#define LOG_GLOBAL_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
		LOGGER_FACTORY->getGlobalLogger()->log(logMessage, logging::convertPriority(logPriority), logRoutine , logFile, logLine, \
				"", "", "", "", "", "", "", 0, "");

#define STATIC_LOG(priority, routine, text) \
		LOGGER_FACTORY->getStaticLogger()->log(text, logging::convertPriority(priority), routine, __FILE__, __LINE__, \
				"", "", "", "", "", "", "", 0, "");

#define STATIC_LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
		LOGGER_FACTORY->getStaticLogger()->log(logMessage, logging::convertPriority(logPriority), logRoutine , logFile, logLine, \
				"", "", "", "", "", "", "", 0, "");

#define STATIC_LOG_TO_AUDIENCE(priority, routine, text, logAudience) \
		LOGGER_FACTORY->getStaticLogger()->log(text, logging::convertPriority(priority), routine, __FILE__, __LINE__, \
				"", "", logAudience, "", "", "", "", 0, "");

#define AUTO_TRACE(routine) \
		std::auto_ptr<logging::LogTrace> __x_logging__auto_trace__routine (new logging::LogTrace(LOGGER_FACTORY->getGlobalLogger(), routine, __FILE__, __LINE__));

#define AUTO_STATIC_TRACE(routine) \
		std::auto_ptr<logging::LogTrace>(new logging::LogTrace(LOGGER_FACTORY->getStaticLogger(), routine, __FILE__, __LINE__));

#define LOG_TO_DEVELOPER(logPriority, logMessage) \
		LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::DEVELOPER);

#define STATIC_LOG_TO_DEVELOPER(logPriority, logMessage) \
		STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::DEVELOPER);

#define LOG_TO_OPERATOR( logPriority, logMessage) \
		LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::OPERATOR);

#define STATIC_LOG_TO_OPERATOR( logPriority, logMessage) \
		STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::OPERATOR);

#define LOG_TO_SCIENCE( logPriority, logMessage) \
		LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

#define STATIC_LOG_TO_SCIENCE( logPriority, logMessage) \
		STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

#define LOG_TO_SCILOG( logPriority, logMessage) \
		LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

#define STATIC_LOG_TO_SCILOG( logPriority, logMessage) \
		STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

#define LOG_TO_AUDIENCE_WITH_LOGGER(logPriority, logMessage, logAudience, logger) \
{ \
	if (logger != NULL) {\
		logger->log(logMessage, logPriority, __PRETTY_FUNCTION__, __FILE__, __LINE, "", "", logAudience, "", "", "", "", 0, ""); \
	}\
}

#endif
#endif
