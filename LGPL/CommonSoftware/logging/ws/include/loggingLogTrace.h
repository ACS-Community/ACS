#ifndef logging_log_trace_H
#define logging_log_trace_H
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
* "@(#) $Id: loggingLogTrace.h,v 1.5 2011/09/14 11:31:09 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-09  created
*/

/** @file loggingLogTrace.h
 *  Header file for abstract Logger.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include "loggingLogger.h"
#include <OS_NS_time.h>

namespace Logging 
{
    //------------------------------------------------------------------------------
    /**
     * LogTrace class that logs an "Entering" message when instantiated and an "Exiting" message when destroyed
     */
    class LogTrace
    {
      public:
	///LogTrace smart pointer
	typedef Loki::SmartPtr<LogTrace, 
			       Loki::NoCopy, 
			       Loki::DisallowConversion,
			       Loki::RejectNull,
			       Loki::DefaultSPStorage> LogTraceSmartPtr;

	/**
	 * Constructor
	 * @param logger Logger to be used
	 * @param method Name of the method from where the log was published.
	 * @param file Name of the file from which the log came from.
	 * @param line Line number from where the log was published.
	 */
	LogTrace(Logger::LoggerSmartPtr logger,
		 const std::string &method,
		 const std::string &file,
		 unsigned long line);

	/**
	 * Constructor
	 * @param logger Logger to be used
	 * @param method Name of the method from where the log was published.
	 */
	LogTrace(Logger::LoggerSmartPtr logger,
		 const std::string &method);
	
	/**
	 * Destructor
	 */
	virtual 
	~LogTrace();

      protected:
	
	/**
	 * Helper method called by the various constructors to log the trace entry.
	 * @param logger Logger to be used
	 * @param method Name of the method from where the log was published.
	 * @param file Name of the file from which the log came from.
	 * @param line Line number from where the log was published.
	 */
	virtual void
	entryLog(Logger::LoggerSmartPtr logger,
		 std::string method,
		 std::string file,
		 unsigned long line);

      private:
	///logger reference saved for destructor use
	Logger::LoggerSmartPtr logger_m;

	///method name saved for destructor use
	std::string methodName_m;

	//file name saved for destructor use
	std::string fileName_m;

	//line number saved for destructor use
	unsigned long lineNumber_m;

	ACE_Time_Value start_time;
	ACE_Time_Value end_time;
    };
    //------------------------------------------------------------------------------
};

#endif /*!_H*/
