#ifndef logging_ace_macros_H
#define logging_ace_macros_H
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
* "@(#) $Id: loggingACEMACROS.h,v 1.12 2011/03/25 23:42:00 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-25  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingMACROS.h"
#include <ace/Log_Record.h>
#include "loggingLogSvcHandler.h"
#include "loggingLoggingProxy.h"
#include "loggingACSLogger.h"

#ifndef ENABLE_LOG4CPP_MACROS

/** @file loggingACSMACROS.h
 * <h2>Description</h2>
 * This file contains ACS logging macros based on the 
 * standar ACE logging patterns.
 * These macros are the easiest way to use the ACS logging
 * system in CPP applications.
 */

//-----------------------------------------------------------------------------
/**
 * Helper macro which check to see if a global logger has been set. If not, it
 * sets it to be a new ACSLogger.
 */
#define ACS_CHECK_LOGGER \
if (Logging::Logger::getGlobalLogger() == 0) \
{  \
   Logging::Logger::LoggerSmartPtr loggersp(new Logging::ACSLogger(Logging::BaseLog::GLOBAL_LOGGER_NAME)); \
   Logging::Logger::setGlobalLogger(loggersp); \
}

/**
 * The pre-defined macro for outputting debug log entries. It accepts a 
 * @param routine The fully qualified name of the routine where the 
 *        log-entry is being generated. 
 * @param text String containing debugging info
 *
 * Usage example:
 *   ACS_DEBUG("maci::ContainerImpl::init", "Activator narrowed");
 */
#define ACS_DEBUG(routine, text) \
ACS_CHECK_LOGGER; \
LOG(Logging::ace2acsPriority(LM_DEBUG), routine, text);

/**
 * This macro is identical to the similarly named macro
 * in almost every respect except that it is designed to 
 * be used from a static context.
 */
#define ACS_STATIC_DEBUG(routine, text) \
ACS_CHECK_LOGGER; \
STATIC_LOG(Logging::ace2acsPriority(LM_DEBUG), routine, text);
//-----------------------------------------------------------------------------
/**
 * Identical to ACS_DEBUG except that this macro allows the developer to pass one
 * printf-style parameter.
 * @param routine The fully qualified name of the routine where the 
 *        log-entry is being generated. 
 * @param text String containing debugging info
 * @param param A parameter to be placed into text using printf-style 
 *        formatting.
 * 
 * @warning The formatted string created by combining param with text must not
 * exceeed 1000 characters. If it does, you must use format the message on your
 * own and pass it to the ACS_DEBUG macro.
 */
#define ACS_DEBUG_PARAM(routine, text, param) \
{ \
    ACS_CHECK_LOGGER; \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted(LM_DEBUG, \
							    text, \
							    param); \
    LOG(tStruct.priority, routine, tStruct.message); \
}

/**
 * This macro is identical to the similarly named macro
 * in almost every respect except that it is designed to 
 * be used from a static context.
 */
#define ACS_STATIC_DEBUG_PARAM(routine, text, param) \
{ \
    ACS_CHECK_LOGGER; \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted(LM_DEBUG, \
							    text, \
							    param); \
    STATIC_LOG(tStruct.priority, routine, tStruct.message); \
}
//-----------------------------------------------------------------------------
/**
 * The pre-defined macro for outputting trace log entries.
 * @param routine The fully qualified name of the routine where the 
 *        log-entry is being generated.  
 *
 * Usage example:
 *    ACS_TRACE("maci::ContainerImpl::init");
 */
#define ACS_TRACE(routine) \
ACS_CHECK_LOGGER; \
LOG(Logging::ace2acsPriority(LM_TRACE), routine, Logging::BaseLog::FIELD_UNAVAILABLE);

/**
 * This macro is identical to the similarly named macro
 * in almost every respect except that it is designed to 
 * be used from a static context.
 */
#define ACS_STATIC_TRACE(routine) \
ACS_CHECK_LOGGER; \
STATIC_LOG(Logging::ace2acsPriority(LM_TRACE), routine, Logging::BaseLog::FIELD_UNAVAILABLE);
//-----------------------------------------------------------------------------
/**
 * The pre-defined macro for outputting log entries. 
 *
 * - log: Formatted as (log_type, format_string, . . .). Passed as a parameter
 * to ACEs logging macros.
 *
 * Usage example:
 *    ACS_SHORT_LOG((LM_INFO, "A sample log entry %d", i));
 * 
 * @warning The formatted string printf-style string passed into this macro must 
 * not exceeed 1000 characters. If it does, you must use format the message on your
 * own and pass it to the LOG macro.
 */
#define ACS_SHORT_LOG(X) \
{ \
    ACS_CHECK_LOGGER; \
    LoggingProxy::Flags(LM_FULL_INFO); \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted X; \
    LOG(tStruct.priority, Logging::BaseLog::FIELD_UNAVAILABLE, tStruct.message); \
}

/**
 * This macro is identical to the similarly named macro
 * in almost every respect except that it is designed to 
 * be used from a static context.
 */
#define ACS_STATIC_SHORT_LOG(X) \
{ \
    ACS_CHECK_LOGGER; \
    LoggingProxy::Flags(LM_FULL_INFO); \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted X; \
    STATIC_LOG(tStruct.priority, Logging::BaseLog::FIELD_UNAVAILABLE, tStruct.message); \
}
//-----------------------------------------------------------------------------
/**
 * The pre-defined macro for outputting log entries. It accepts three parameters
 *
 * @param flags This parameter specifies the priority and additional log-entry
 *        flags, such as whether to output the runtime context (thread & process)
 *        or not.
 * @param routine The fully qualified name of the routine where the log-entry is
 *        being generated. Can be 0, in which case the routine name is not output.
 * @param log Formatted as (log_type, format_string, . . .). Passed as a parameter
 *        to ACEs logging macros.
 *
 * Usage example:
 *       ACS_LOG(LM_SOURCE_INFO | LM_PRIORITY(7),
 *		"maci::ContainerImpl::init",
 *		(LM_INFO, "A sample log entry %d", i));
 *  
 * @warning The formatted string printf-style string passed into this macro must 
 * not exceeed 1000 characters. If it does, you must use format the message on your
 * own and pass it to the LOG macro.
 */
#define ACS_LOG(flags, routine, X) \
{ \
    ACS_CHECK_LOGGER; \
    LoggingProxy::Flags(flags); \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted X; \
    LOG(tStruct.priority, routine, tStruct.message); \
}

/**
 * This macro is identical to the similarly named macro
 * in almost every respect except that it is designed to 
 * be used from a static context.
 */
#define ACS_STATIC_LOG(flags, routine, X) \
{ \
    ACS_CHECK_LOGGER; \
    LoggingProxy::Flags(flags); \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted X; \
    STATIC_LOG(tStruct.priority, routine, tStruct.message); \
}
//-----------------------------------------------------------------------------
/**
 * The pre-defined macro for outputting log entries with specified time. It accepts four parameters
 *
 * - flags: This parameter specifies the priority and additional log-entry
 * flags, such as whether to output the runtime context (thread & process)
 * or not.
 * - timeStamp: time stamp in 100th of nanoseconds
 * - routine: The fully qualified name of the routine where the log-entry is
 * being generated. Can be 0, in which case the routine name is not output.
 * - _log: logging message (not formated!!!)
 *
 * Usage example:
 *
 * ACS_LOG_TIME(LM_SOURCE_INFO | LM_PRIORITY(7), 
 *		234243, "maci::ContainerImpl::init",
 *		(LM_INFO, "A sample log entry "));
 */
#define ACS_LOG_TIME(flags, tStamp, routine, _log) \
{ \
    ACS_CHECK_LOGGER; \
    LoggingProxy::Flags(flags); \
    Logging::LogSvcHandler::DeprecatedLogInfo tStruct; \
    tStruct = Logging::LogSvcHandler::unformatted2formatted _log; \
    LOG_RECORD(tStruct.priority, tStruct.message, __FILE__, __LINE__, routine, tStamp, getLogger()->getName()); \
}

/**
 * Manipulate priority contained in the log entrys flags. The priority can
 * be from 0 ("use default") through 1 (lowest) to 31 (highest).
 */
#define LM_PRIORITY(p) p

/** Maximum priority */
#define LM_MAX_PRIORITY 0x0F

#else
#include "loggingLog4cppACEMACROS.h"
#endif

#endif /*!_H*/
