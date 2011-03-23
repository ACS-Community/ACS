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
 * "@(#) $Id: loggingLog4cpp.h,v 1.3 2011/03/23 23:27:48 javarias Exp $"
 */
#ifndef LOGGING_LOG4CPP_H_
#define LOGGING_LOG4CPP_H_

#include "loggingACSCategory.h"
#include "logging_idlC.h"

#include <log4cpp/LayoutAppender.hh>
#include <ace/Singleton.h>
#include <ace/Synch.h>
#include "loggingBaseLog.h"

#define LM_DELOUSE 010000U

namespace logging {

struct BasicLogInfo {
	log4cpp::Priority::PriorityLevel priority;
	std::string message;
};

class Logger {
public:

	Logger();
	~Logger();

	ACSCategory* getLogger(const std::string& loggerName);
	ACSCategory* getGlobalLogger();
	ACSCategory* getStaticLogger();

	void enableRemoteAppender(Logging::AcsLogService_ptr loggingService);
	void enableSyslogAppender();

	static BasicLogInfo formatLog(log4cpp::Priority::PriorityLevel priority, const char *fmt, ...);
	static BasicLogInfo formatLog(ACE_Log_Priority priority, const char *fmt, ...);
	static BasicLogInfo formatLog(unsigned int priority, const char *fmt, ...);

private:
	bool remoteAppenderEnabled;
	bool syslogAppenderEnabled;
	unsigned int localLogLevel;
	unsigned int remoteLogLevel;
	unsigned int syslogLogLevel;
	Logging::AcsLogService_ptr loggingService;
	//Naming Service here
	ACSCategory* initLogger(const std::string& loggerName);
};

class LogTrace {
public:
	LogTrace (ACSCategory* logger, const std::string &method,
			const std::string &file, const unsigned long line);
	LogTrace (ACSCategory* logger, const std::string &method);
	~LogTrace();

private:
	ACSCategory* logger;
	const std::string method;
	const std::string file;
	const unsigned long line;
};

log4cpp::Priority::PriorityLevel convertPriority(unsigned int logLevel);
log4cpp::Priority::PriorityLevel convertPriority(ACE_Log_Priority logLevel);
log4cpp::Priority::PriorityLevel convertPriority (Logging::BaseLog::Priority logLevel);
}

#define LOGGER_FACTORY ACE_Singleton<logging::Logger, ACE_Null_Mutex>::instance()
#endif
