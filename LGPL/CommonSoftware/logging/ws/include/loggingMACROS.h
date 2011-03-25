#ifndef logging_macros_H
#define logging_macros_H
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
* "@(#) $Id: loggingMACROS.h,v 1.32 2011/03/25 23:42:49 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created
*/


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include "loggingLogger.h"
#include "loggingLogTrace.h"
#include "loggingGetLogger.h"
#include <iostream>
#include <acsutilTimeStamp.h>

#ifndef ENABLE_LOG4CPP_MACROS

#define LM_DELOUSE 010000


/**
 * Used to send logs. This macro is primarily useful because it automatically
 * determines the file name and line number for the developer. It is important
 * to note that getLogger() is defined in a header file other than what's included
 * above. UPDATE: I just included the file, don't know why it wasn't included before.
 * @param priority Logging::BaseLog::Priority of the log message
 * @param routine Name of the routine in which this macro is being used from (std::string)
 * @param text Log message (std::string)
 */
#define LOG(priority, routine, text) \
if (getLogger()!=0) \
{ \
  getLogger()->log(priority, text, __FILE__, __LINE__, routine); \
} \
else \
{ \
  std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
  std::cerr << __FILE__ << ", line=" << __LINE__ << std::endl; \
}


/**
 * Used to send logs. This macro is primarily useful because it automatically
 * determines the file name and line number for the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logRoutine Name of the routine in which this macro is being used from (std::string)
 * @param logMessage Log message (std::string)
 * @param logAudience intended receiver of this log message
 * @param logArray array where the log was generated
 * @param logAntenna antenna where the log was generated
 */
#define LOG_FULL(logPriority, logRoutine, logMessage, logAudience, logArray, logAntenna) \
if (getLogger()!=0) \
{ \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = Logging::ace2acsPriority(logPriority); \
    lr.message   = logMessage; \
    lr.file      = __FILE__; \
    lr.line      = __LINE__; \
    lr.method    = logRoutine; \
    lr.timeStamp = getTimeStamp(); \
    LoggingProxy::audience(logAudience); \
    LoggingProxy::array(logArray); \
    LoggingProxy::antenna(logAntenna); \
    LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT); \
    getLogger()->log(lr); \
	 LoggingProxy::audience(NULL); \
} \
else \
{ \
  std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
  std::cerr << __FILE__ << ", line=" << __LINE__ << std::endl; \
}

/**
 * Used to send logs. This macro is primarily useful because it automatically
 * determines the file name and line number for the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logRoutine Name of the routine in which this macro is being used from (std::string)
 * @param logMessage Log message (std::string)
 * @param logArray array where the log was generated
 * @param logAntenna antenna where the log was generated
 */
#define LOG_WITH_ANTENNA_CONTEXT(logPriority, logRoutine, logMessage, logArray, logAntenna) \
if (getLogger()!=0) \
{ \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = Logging::ace2acsPriority(logPriority); \
    lr.message   = logMessage; \
    lr.file      = __FILE__; \
    lr.line      = __LINE__; \
    lr.method    = logRoutine; \
    lr.timeStamp = getTimeStamp(); \
    LoggingProxy::array(logArray); \
    LoggingProxy::antenna(logAntenna); \
    LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT); \
    getLogger()->log(lr); \
} \
else \
{ \
  std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
  std::cerr << __FILE__ << ", line=" << __LINE__ << std::endl; \
}

/**
 * Used to send logs. This macro is primarily useful because it automatically
 * determines the file name and line number for the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logRoutine Name of the routine in which this macro is being used from (std::string)
 * @param logMessage Log message (std::string)
 * @param logAudience intended receiver of this log message
 */
#define LOG_TO_AUDIENCE(logPriority, logRoutine, logMessage, logAudience) \
if (getLogger()!=0) \
{ \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = Logging::ace2acsPriority(logPriority); \
    lr.message   = logMessage; \
    lr.file      = __FILE__; \
    lr.line      = __LINE__; \
    lr.method    = logRoutine; \
    lr.timeStamp = getTimeStamp(); \
    LoggingProxy::audience(logAudience); \
    LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT); \
    getLogger()->log(lr); \
	 LoggingProxy::audience(NULL); \
} \
else \
{ \
  std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
  std::cerr << __FILE__ << ", line=" << __LINE__ << std::endl; \
}

/**
 * Used to publish a log record. Useful for doing things like setting
 * a specific time the log was sent (rather than letting ACS figure this
 * out for you).
 * @param logPriority Logging::BaseLog::Priority (ACS) priority of the log.
 * @param logMessage  Log message (string).
 * @param logFile  Name of the file the log was published from (__FILE__)
 * @param logLine  Line number from where the log was published (__LINE__)
 * @param logRoutine  Name of the routine from where the log was published (string)
 * @param logTime  Time the log was published (ACS::Time)
 * @param logSource Source of the log (i.e., container name, component name, etc)
 */
#define LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime, logSource) \
if (getLogger()!=0) \
{ \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = logPriority; \
    lr.message   = logMessage; \
    lr.file      = logFile; \
    lr.line      = logLine; \
    lr.method    = logRoutine; \
    lr.timeStamp = logTime; \
    getNamedLogger(logSource)->log(lr); \
} \
else \
{ \
  std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
  std::cerr << logFile << ", line=" << logLine << std::endl; \
}


/**
 * Used to publish a log record to the global logger. Useful for doing things like setting
 * a specific time the log was sent (rather than letting ACS figure this
 * out for you).
 * @param logPriority  Logging::BaseLog::Priority (ACS) priority of the log.
 * @param logMessage  Log message (string).
 * @param logFile  Name of the file the log was published from (__FILE__)
 * @param logLine  Line number from where the log was published (__LINE__)
 * @param logRoutine  Name of the routine from where the log was published (string)
 * @param logTime  Time the log was published (ACS::Time)
 */
#define LOG_GLOBAL_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
if (getLogger()!=0) \
{ \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = logPriority; \
    lr.message   = logMessage; \
    lr.file      = logFile; \
    lr.line      = logLine; \
    lr.method    = logRoutine; \
    lr.timeStamp = logTime; \
    getLogger()->log(lr); \
} \
else \
{ \
  std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
  std::cerr << logFile << ", line=" << logLine << std::endl; \
}


/**
 * Used to send logs from a static context (such as from static methods). This macro
 * is primarily useful because it automatically determines the file name and line
 * number for the developer.
 * @param priority Logging::BaseLog::Priority of the log message
 * @param routine Name of the routine in which this macro is being used from (std::string)
 * @param text Log message (std::string)
 */
#define STATIC_LOG(priority, routine, text) \
Logging::Logger::getStaticLogger()->log(priority, text, __FILE__, __LINE__, routine);

/**
 * Used to publish a log record from a static context. Useful for doing things like setting
 * a specific time the log was sent (rather than letting ACS figure this
 * out for you).
 * @param logPriority Logging::BaseLog::Priority (ACS) priority of the log.
 * @param logMessage Log message (std::string).
 * @param logFile  Name of the file the log was published from (__FILE__)
 * @param logLine  Line number from where the log was published (__LINE__)
 * @param logRoutine  Name of the routine from where the log was published (string)
 * @param logTime  Time the log was published (ACS::Time)
 */
#define STATIC_LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
{ \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = logPriority; \
    lr.message   = logMessage; \
    lr.file      = logFile; \
    lr.line      = logLine; \
    lr.method    = logRoutine; \
    lr.timeStamp = logTime; \
    Logging::Logger::getStaticLogger()->log(lr); \
}

/**
 * Used to send logs from a static context (such as from static methods).
 * This macro is primarily useful because it automatically determines the file 
 * name and line
 * @param logPriority ACE_Log_Priority of the log message
 * @param logRoutine Name of the routine in which this macro is being used from (std::string)
 * @param logMessage Log message (std::string)
 * @param logAudience intended receiver of this log message
 */
#define STATIC_LOG_TO_AUDIENCE(priority, routine, text, logAudience) \
    LoggingProxy::audience(logAudience); \
    LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT); \
    Logging::Logger::getStaticLogger()->log(Logging::ace2acsPriority(priority), text, __FILE__, __LINE__, routine);

/**
 * This macro creates a LogTrace object which in turn logs a trace message where it is
 * immediately declared and then logs another trace message when it is destroyed. It can
 * only be used once per namespace.
 * @param routine Name of the routine in which this macro is being used from (std::string)
 */
#define AUTO_TRACE(routine) \
Logging::LogTrace::LogTraceSmartPtr __autoTraceLogTraceSmartPtrInstance(new Logging::LogTrace(getLogger(), routine, __FILE__, __LINE__));

/**
 * This macro is static version of AUTO_TRACE macro that can be used inside a static methods
 * where AUTO_TRACE does not work (it does not compile).
 * For details see description of AUTO_TRACE macro.
 * @param routine Name of the routine in which this macro is being used from (std::string)
 */
#define AUTO_STATIC_TRACE(routine) \
Logging::LogTrace::LogTraceSmartPtr __autoTraceLogTraceSmartPtrInstance(new Logging::LogTrace(Logging::Logger::getStaticLogger(), routine, __FILE__, __LINE__));

/**
 * Used to send logs to the developer. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define LOG_TO_DEVELOPER(logPriority, logMessage) \
    LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::DEVELOPER);

/**
 * Used to send logs to the developer. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define STATIC_LOG_TO_DEVELOPER(logPriority, logMessage) \
    STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::DEVELOPER);

/**
 * Used to send logs to the operator. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define LOG_TO_OPERATOR( logPriority, logMessage) \
    LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::OPERATOR);

/**
 * Used to send logs to the operator. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define STATIC_LOG_TO_OPERATOR( logPriority, logMessage) \
    STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::OPERATOR);

/**
 * Used to send logs to the science logs. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the scientists.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define LOG_TO_SCIENCE( logPriority, logMessage) \
    LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

/**
 * Used to send logs to the science logs. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the scientists.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define STATIC_LOG_TO_SCIENCE( logPriority, logMessage) \
    STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

/**
 * Used to send logs to the science logs. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the scientists.
 *
 * Note: Replaced by LOG_TO_SCIENCE.  This macro will be removed after the ALMA 7.1 release.
 * 
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define LOG_TO_SCILOG( logPriority, logMessage) \
    LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

/**
 * Used to send logs to the science logs. This macro is primarily useful because
 * it automatically determines the file name, line number and function name for
 * the scientists.
 *
 * Note: Replaced by LOG_TO_SCIENCE.  This macro will be removed after the ALMA 7.1 release.
 * 
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 */
#define STATIC_LOG_TO_SCILOG( logPriority, logMessage) \
    STATIC_LOG_TO_AUDIENCE(logPriority, __PRETTY_FUNCTION__, logMessage, log_audience::SCILOG);

/**
 * Used to send logs. This macro is primarily useful because it automatically
 * determines the file name and line number for the developer.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logRoutine Name of the routine in which this macro is being used from (std::string)
 * @param logMessage Log message (std::string)
 * @param logAudience intended receiver of this log message
 * @param logger The logger to use (Logging::Logger::LoggerSmartPtr)
 */
#define LOG_TO_AUDIENCE_WITH_LOGGER(logPriority, logMessage, logAudience, logger) \
if (logger != 0) { \
    Logging::BaseLog::LogRecord lr; \
    lr.priority  = Logging::ace2acsPriority(logPriority); \
    lr.message   = logMessage; \
    lr.file      = __FILE__; \
    lr.line      = __LINE__; \
    lr.method    = __PRETTY_FUNCTION__; \
    lr.timeStamp = ::getTimeStamp();     \
    LoggingProxy::audience(logAudience); \
    LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT); \
    logger->log(lr); \
} else { \
    std::cerr << "SEVERE LOGGING ERROR - getLogger() returned NULL: file="; \
    std::cerr << __FILE__ << ", line=" << __LINE__ << std::endl; \
}

/**
 * Used to send logs to the operator. This macro is primarily useful because
 * it automatically determines the file name, line number, function name and
 * set the audience to the operator. This can be used in static functions.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 * @param logger The logger to use (Logging::Logger::LoggerSmartPtr)
 */
#define LOG_TO_OPERATOR_WITH_LOGGER( logPriority, logMessage, logger) \
    LOG_TO_AUDIENCE_WITH_LOGGER(logPriority, logMessage, log_audience::OPERATOR, logger);

/**
 * Used to send logs to the developer. This macro is primarily useful because
 * it automatically determines the file name, line number, function name and
 * set the audience to the developer. This can be used in static functions.
 * @param logPriority ACE_Log_Priority of the log message
 * @param logMessage Log message (std::string)
 * @param logger The logger to use (Logging::Logger::LoggerSmartPtr)
 */
#define LOG_TO_DEVELOPER_WITH_LOGGER(logPriority, logMessage, logger) \
    LOG_TO_AUDIENCE_WITH_LOGGER(logPriority, logMessage, log_audience::DEVELOPER, logger);

#else
#include "loggingLog4cppMACROS.h"
#endif

#endif /*!_H*/
