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
 * "@(#) $Id: loggingLog4cpp.h,v 1.7 2011/04/26 20:19:22 javarias Exp $"
 */
#ifndef LOGGING_LOG4CPP_H_
#define LOGGING_LOG4CPP_H_

#include "loggingACSCategory.h"
#include "logging_idlC.h"

#include <log4cpp/LayoutAppender.hh>
#include <ace/Singleton.h>
#include <ace/Synch.h>
#include <orbsvcs/CosNamingC.h>

#include "loggingBaseLog.h"

#ifndef LM_DELOUSE
#define LM_DELOUSE 010000U
#endif

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

	void enableRemoteAppender(unsigned long cacheSize = 100,
			unsigned int autoFlushTimeoutSec = 3,
			Logging::AcsLogService_ptr loggingService = Logging::AcsLogService::_nil(),
			CosNaming::NamingContext_ptr namingService = CosNaming::NamingContext::_nil(),
			int maxLogsPerSecond = -1);
	void enableSyslogAppender();

	void setLogLevels(const std::string& loggerName, log4cpp::Priority::PriorityLevel remote, log4cpp::Priority::PriorityLevel local);

	static BasicLogInfo formatLog(log4cpp::Priority::PriorityLevel priority, const char *fmt, ...)
#if defined(__GNUC__)
	__attribute__ ((format (printf, 2, 3)))
#endif
		;
	static BasicLogInfo formatLog(ACE_Log_Priority priority, const char *fmt, ...)
#if defined(__GNUC__)
	__attribute__ ((format (printf, 2, 3)))
#endif
		;
	static BasicLogInfo formatLog(unsigned int priority, const char *fmt, ...)
#if defined(__GNUC__)
	__attribute__ ((format (printf, 2, 3)))
#endif
		;

private:
	bool remoteAppenderEnabled;
	bool syslogAppenderEnabled;
	unsigned int localLogLevel;
	unsigned int remoteLogLevel;
	unsigned int syslogLogLevel;
	//Configuration parameters for ACS Remote Logger
	unsigned long cacheSize;
	unsigned int autoFlushTimeoutSec;
	Logging::AcsLogService_ptr loggingService;
	CosNaming::NamingContext_ptr namingService;
	int maxLogsPerSecond;
	//
	ACE_Thread_Mutex initMutex;
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
log4cpp::Priority::PriorityLevel convertPriority(Logging::BaseLog::Priority logLevel);
log4cpp::Priority::PriorityLevel convertPriority(AcsLogLevels::logLevelValue logLevel);
}

#define LOGGER_FACTORY ACE_Singleton<logging::Logger, ACE_Null_Mutex>::instance()
#endif
