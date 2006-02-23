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
* "@(#) $Id: loggingMACROS.h,v 1.13 2006/01/25 15:39:12 dfugate Exp $"
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
#include <iostream>

/**
 * Used to send logs. This macro is primarily useful because it automatically
 * determines the file name and line number for the developer. It is important
 * to note that getLogger() is defined in a header file other than what's included
 * above.
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
 * Used to publish a log record. Useful for doing things like setting
 * a specific time the log was sent (rather than letting ACS figure this
 * out for you).
 * @param logPriority  ACS priority of the log.
 * @param logMessage  Log message (string).
 * @param logFile  Name of the file the log was published from (__FILE__)
 * @param logLine  Line number from where the log was published (__LINE__)
 * @param logRoutine  Name of the routine from where the log was published (string)
 * @param logTime  Time the log was published (ACS::Time)
 */
#define LOG_RECORD(logPriority, logMessage, logFile, logLine, logRoutine, logTime) \
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
getNamedLogger(Logging::BaseLog::STATIC_LOGGER_NAME)->log(priority, text, __FILE__, __LINE__, routine);

/**
 * Used to publish a log record from a static context. Useful for doing things like setting
 * a specific time the log was sent (rather than letting ACS figure this
 * out for you).
 * @param logPriority  ACS priority of the log.
 * @param logMessage  Log message (string).
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
    getNamedLogger(Logging::BaseLog::STATIC_LOGGER_NAME)->log(lr); \
}

/**
 * This macro creates a LogTrace object which in turn logs a trace message where it is 
 * immediately declared and then logs another trace message when it is destroyed. It can 
 * only be used once per namespace.
 * @param routine Name of the routine in which this macro is being used from (std::string)
 */
#define AUTO_TRACE(routine) \
Logging::LogTrace::LogTraceSmartPtr __autoTraceLogTraceSmartPtrInstance(new Logging::LogTrace(getLogger(), routine, __FILE__, __LINE__));

#endif /*!_H*/
