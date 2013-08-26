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
 * "@(#) $Id: loggingLog4cpp.cpp,v 1.6 2011/04/08 14:33:38 javarias Exp $"
 */

#include "loggingLog4cpp.h"
#include "loggingStdoutlayout.h"
#include "loggingXmlLayout.h"
#include "loggingACSRemoteAppender.h"

#include <log4cpp/OstreamAppender.hh>
#include <log4cpp/SyslogAppender.hh>

#define DEFAULT_LOG_LEVEL_STDOUT 4
#define DEFUALT_LOG_LEVEL_REMOTE 3
#define DEFAULT_LOG_LEVEL_SYSLOG 3

#define STDOUT_APPENDER_NAME "STDOUT_appender"
#define SYSLOG_APPENDER_NAME "SYSLOG_appender"
#define REMOTE_APPENDER_NAME "ACS_REMOTE_appender"

using namespace logging;

Logger::Logger() :
		remoteAppenderEnabled(false),
		syslogAppenderEnabled(false),
		localLogLevel(DEFAULT_LOG_LEVEL_STDOUT),
		remoteLogLevel(DEFUALT_LOG_LEVEL_REMOTE),
		syslogLogLevel(DEFAULT_LOG_LEVEL_SYSLOG),
		loggingService(NULL){

	char* local_log_level = getenv("ACS_LOG_STDOUT");
	char* remote_log_level = getenv("ACS_LOG_REMOTE");
	char* syslog_log_level = getenv("ACS_LOG_SYSLOG");

	if (local_log_level != NULL)
		localLogLevel = atoi(local_log_level);
	if (remote_log_level != NULL)
		remoteLogLevel = atoi(remote_log_level);
	if (syslog_log_level != NULL)
		syslogLogLevel = atoi(syslog_log_level);

	getGlobalLogger();
	getStaticLogger();
}

Logger::~Logger() {
	static RemoteLoggerBuffer* buffer;
	if (buffer != NULL) {
		delete buffer;
		buffer = NULL;
	}
	//Clean the Categories?
}

void Logger::enableRemoteAppender(unsigned long cacheSize,
		unsigned int autoFlushTimeoutSec,
		Logging::AcsLogService_ptr loggingService,
		CosNaming::NamingContext_ptr namingService,
		int maxLogsPerSecond) {
	initMutex.acquire();
	if(!remoteAppenderEnabled) {
		remoteAppenderEnabled = true;
		this->cacheSize = cacheSize;
		this->autoFlushTimeoutSec = autoFlushTimeoutSec;
		if (!CORBA::is_nil(loggingService))
			this->loggingService = Logging::AcsLogService::_duplicate(loggingService);
		if(!CORBA::is_nil(namingService))
			this->namingService = CosNaming::NamingContext::_duplicate(namingService);
		this->maxLogsPerSecond = maxLogsPerSecond;

		std::vector<log4cpp::Category*>* loggers =
						ACSHierarchyMaintainer::getDefaultMaintainer().getCurrentCategories();
		std::vector<log4cpp::Category*>::iterator it;
		for (it = loggers->begin(); it < loggers->end(); it++) {
			if ((*it)->getAppender(REMOTE_APPENDER_NAME) != NULL)
				continue;
			::log4cpp::Appender* remoteAppender =
					new logging::ACSRemoteAppender(REMOTE_APPENDER_NAME,
					this->cacheSize, this->autoFlushTimeoutSec,
					this->loggingService,
					this->maxLogsPerSecond);
			//TODO: use the naming service
			remoteAppender->setLayout(new logging::ACSXmlLayout());
			remoteAppender->setThreshold(convertPriority(remoteLogLevel));
			(*it)->addAppender(remoteAppender);
		}
	}
	initMutex.release();
}

void Logger::enableSyslogAppender() {
	initMutex.acquire();
	if (!syslogAppenderEnabled) {
		syslogAppenderEnabled = true;
		std::vector<log4cpp::Category*>* loggers =
				ACSHierarchyMaintainer::getDefaultMaintainer().getCurrentCategories();
		std::vector<log4cpp::Category*>::iterator it;
		for (it = loggers->begin(); it < loggers->end(); it++) {
			if ((*it)->getAppender(SYSLOG_APPENDER_NAME) != NULL)
				continue;
			::log4cpp::Appender * syslogAppender =
					new ::log4cpp::SyslogAppender(SYSLOG_APPENDER_NAME, "ACS");
			syslogAppender->setLayout(new logging::ACSstdoutLayout());
			syslogAppender->setThreshold(convertPriority(syslogLogLevel));
			(*it)->addAppender(syslogAppender);
		}
	}
	initMutex.release();
}

void Logger::setLogLevels(const std::string& loggerName, log4cpp::Priority::PriorityLevel remote,
		log4cpp::Priority::PriorityLevel local) {
	ACSCategory* logger = ACSCategory::exist(loggerName);
	if (logger == NULL)
		return;
	if (logger->getAppender(REMOTE_APPENDER_NAME))
		logger->getAppender(REMOTE_APPENDER_NAME)->setThreshold(remote);
	if (logger->getAppender(STDOUT_APPENDER_NAME))
		logger->getAppender(STDOUT_APPENDER_NAME)->setThreshold(local);
	if (logger->getAppender(SYSLOG_APPENDER_NAME))
		logger->getAppender(SYSLOG_APPENDER_NAME)->setThreshold(local);
}

ACSCategory* Logger::getGlobalLogger() {
	return getLogger("GlobalLogger");
}

ACSCategory* Logger::getStaticLogger() {
	return getLogger("StaticMethodLogger");
}

ACSCategory* Logger::getLogger(const std::string& loggerName) {
	ACSCategory* logger = ACSCategory::exist(loggerName);
	if (logger == NULL)
		logger = initLogger(loggerName);
	return logger;
}

ACSCategory* Logger::initLogger(const std::string& loggerName) {
	::log4cpp::Appender* localAppender = new ::log4cpp::OstreamAppender(STDOUT_APPENDER_NAME, &::std::cout);
	localAppender->setLayout(new logging::ACSstdoutLayout());
	localAppender->setThreshold(convertPriority(localLogLevel));
	ACSCategory &logger = ACSCategory::getInstance(loggerName);
	logger.addAppender(localAppender);

	initMutex.acquire();
	if (syslogAppenderEnabled && logger.getAppender(SYSLOG_APPENDER_NAME) == NULL) {
		::log4cpp::Appender* syslogAppender = new ::log4cpp::SyslogAppender(SYSLOG_APPENDER_NAME, "ACS");
		syslogAppender->setLayout(new logging::ACSstdoutLayout());
		syslogAppender->setThreshold(convertPriority(syslogLogLevel));
		logger.addAppender(syslogAppender);
	}
	initMutex.release();

	initMutex.acquire();
	if (remoteAppenderEnabled && logger.getAppender(REMOTE_APPENDER_NAME) == NULL) {
		::log4cpp::Appender* remoteAppender = new logging::ACSRemoteAppender(REMOTE_APPENDER_NAME,
				this->cacheSize, this->autoFlushTimeoutSec,
				this->loggingService,
				this->maxLogsPerSecond);
		remoteAppender->setLayout(new logging::ACSXmlLayout());
		remoteAppender->setThreshold(convertPriority(remoteLogLevel));
		logger.addAppender(remoteAppender);
	}
	initMutex.release();

	return ACSCategory::exist(loggerName);
}

BasicLogInfo Logger::formatLog(log4cpp::Priority::PriorityLevel priority, const char *fmt, ...) {
	BasicLogInfo retVal;
	retVal.priority = priority;
	va_list argp;
	va_start(argp, fmt);
	char tmp_msg[1024];
	vsnprintf(tmp_msg, sizeof(char) * 1024, fmt, argp);
	va_end(argp);
	retVal.message = tmp_msg;
	return retVal;
}

BasicLogInfo Logger::formatLog(ACE_Log_Priority priority, const char *fmt, ...) {
	BasicLogInfo retVal;
	retVal.priority = convertPriority(priority);
	va_list argp;
	va_start(argp, fmt);
	char tmp_msg[1024];
	vsnprintf(tmp_msg, sizeof(char) * 1024, fmt, argp);
	va_end(argp);
	retVal.message = tmp_msg;
	return retVal;
}

BasicLogInfo Logger::formatLog(unsigned int priority, const char *fmt, ...) {
	BasicLogInfo retVal;
	retVal.priority = convertPriority(priority);
	va_list argp;
	va_start(argp, fmt);
	char tmp_msg[1024];
	vsnprintf(tmp_msg, sizeof(char) * 1024, fmt, argp);
	va_end(argp);
	retVal.message = tmp_msg;
	return retVal;
}

LogTrace::LogTrace(ACSCategory* logger, const std::string &method,
		const std::string &file, const unsigned long line) :
		logger(logger), method(method), file(file), line(line) {
	if(logger != NULL)
		logger->log("Entering", log4cpp::Priority::TRACE, method, file, line);
	else {
		std::cerr << "SEVERE LOGGING ERROR IN LogTrace/AUTO_TRACE - logger/getLogger() is NULL: routine=";
		std::cerr << method << " file: " << file << " line: " << line << std::endl;
	}
}

LogTrace::LogTrace (ACSCategory* logger, const std::string &method) :
	logger(logger), method(method), file("Unavailable"), line(0UL) {
	if(logger != NULL)
		logger->log("Entering...", log4cpp::Priority::TRACE, method, file, line);
	else {
		std::cerr << "SEVERE LOGGING ERROR IN LogTrace/AUTO_TRACE - logger/getLogger() is NULL: routine=";
		std::cerr << method << " file: " << file << " line: " << line << std::endl;
	}
}

LogTrace::~LogTrace() {
	if(logger != NULL)
		logger->log("Exiting...", log4cpp::Priority::TRACE, method, file, line);
}



log4cpp::Priority::PriorityLevel logging::convertPriority (unsigned int logLevel) {
	switch (logLevel) {
	case 1:
		return log4cpp::Priority::TRACE;
	case 2:
		return log4cpp::Priority::DELOUSE;
	//Old LM_DELOUSE value = 010000
	case LM_DELOUSE:
		return log4cpp::Priority::DELOUSE;
	case 3:
		return log4cpp::Priority::DEBUG;
	case 4:
		return log4cpp::Priority::INFO;
	case 5:
		return log4cpp::Priority::NOTICE;
	case 6:
		return log4cpp::Priority::WARNING;
	case 8:
		return log4cpp::Priority::ERROR;
	case 9:
		return log4cpp::Priority::CRITICAL;
	case 10:
		return log4cpp::Priority::ALERT;
	case 11:
		return log4cpp::Priority::EMERGENCY;
	default:
		return log4cpp::Priority::NOTSET;
	}
}

log4cpp::Priority::PriorityLevel logging::convertPriority (Logging::BaseLog::Priority logLevel) {
	switch (logLevel) {
	case Logging::BaseLog::LM_TRACE:
		return log4cpp::Priority::TRACE;
	case 03:
		return log4cpp::Priority::DELOUSE;
	case Logging::BaseLog::LM_DEBUG:
		return log4cpp::Priority::DEBUG;
	case Logging::BaseLog::LM_INFO:
		return log4cpp::Priority::INFO;
	case Logging::BaseLog::LM_NOTICE:
		return log4cpp::Priority::NOTICE;
	case Logging::BaseLog::LM_WARNING:
		return log4cpp::Priority::WARNING;
	case Logging::BaseLog::LM_ERROR:
		return log4cpp::Priority::ERROR;
	case Logging::BaseLog::LM_CRITICAL:
		return log4cpp::Priority::CRITICAL;
	case Logging::BaseLog::LM_ALERT:
		return log4cpp::Priority::ALERT;
	case Logging::BaseLog::LM_EMERGENCY:
		return log4cpp::Priority::EMERGENCY;
	default:
		return log4cpp::Priority::NOTSET;
	}
}

log4cpp::Priority::PriorityLevel logging::convertPriority(ACE_Log_Priority logLevel) {
	switch (logLevel) {
	case LM_TRACE:
		return log4cpp::Priority::TRACE;
	case LM_DEBUG:
		return log4cpp::Priority::DEBUG;
	case LM_INFO:
		return log4cpp::Priority::INFO;
	case LM_NOTICE:
		return log4cpp::Priority::NOTICE;
	case LM_WARNING:
		return log4cpp::Priority::WARNING;
	case LM_ERROR:
		return log4cpp::Priority::ERROR;
	case LM_CRITICAL:
		return log4cpp::Priority::CRITICAL;
	case LM_ALERT:
		return log4cpp::Priority::ALERT;
	case LM_EMERGENCY:
		return log4cpp::Priority::EMERGENCY;
	default:
		return log4cpp::Priority::NOTSET;
	}
}

log4cpp::Priority::PriorityLevel logging::convertPriority (AcsLogLevels::logLevelValue logLevel) {
	switch (logLevel) {
	case AcsLogLevels::TRACE_VAL:
		return log4cpp::Priority::TRACE;
	case AcsLogLevels::DELOUSE_VAL:
		return log4cpp::Priority::DELOUSE;
	case AcsLogLevels::DEBUG_VAL:
		return log4cpp::Priority::DEBUG;
	case AcsLogLevels::INFO_VAL:
		return log4cpp::Priority::INFO;
	case AcsLogLevels::NOTICE_VAL:
		return log4cpp::Priority::NOTICE;
	case AcsLogLevels::WARNING_VAL:
		return log4cpp::Priority::WARNING;
	case AcsLogLevels::ERROR_VAL:
		return log4cpp::Priority::ERROR;
	case AcsLogLevels::CRITICAL_VAL:
		return log4cpp::Priority::CRITICAL;
	case AcsLogLevels::ALERT_VAL:
		return log4cpp::Priority::ALERT;
	case AcsLogLevels::EMERGENCY_VAL:
		return log4cpp::Priority::EMERGENCY;
	default:
		return log4cpp::Priority::NOTSET;
	}
}

