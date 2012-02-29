#ifndef logging_log_svc_handler_H
#define logging_log_svc_handler_H
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
* "@(#) $Id: loggingLogSvcHandler.h,v 1.12 2012/02/29 12:50:09 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingHandler.h"
#include "loggingExport.h"
#include <stdarg.h>
#include <ace/Log_Priority.h>

namespace Logging {
    
    /**
     * Helper function which converts ACE logging priorities
     * to ACS logging priorities.
     * @param acePriority ACE logging priority
     * @return the corresponding ACS logging priority
     */
    Logging::BaseLog::Priority
    ace2acsPriority(ACE_Log_Priority acePriority);
    
    /**
     * Helper function which converts ACS logging priorities
     * to ACE logging priorities. This is not a static method
     * of LogSvcHandler because of namespace conflicts.
     * @param acsPriority ACS logging priority
     * @return the corresponding ACE logging priority
     */
    ACE_Log_Priority
    acs2acePriority(Logging::BaseLog::Priority acsPriority);
    

    /**
     *
     */
    class logging_EXPORT LogSvcHandler : public virtual Handler
    {
      public:
	// --------------------------------------------------------
	/**
	 * Standard constructor.
	 * @param soName Name of the source object to be sent to 
	 *        the LoggingProxy. Typically this is the name of the
	 *        Logger which is using this Handler-derived object.
	 */
	LogSvcHandler(const std::string& soName);

	/**
	 * Overridden from baseclass.
	 */ 
	virtual void
	log(const LogRecord&);

	/**
	 * Overridden from baseclass.
	 */
	virtual std::string
	getName() const;

	/**
	 * Overridden from baseclass.
	 */
	virtual void
	setLevels(Priority remotePriority, Priority localPriority, int type);
	

    //---------------------------------------------
	//--The following section exists solely to remain
	//--backwards compatible with the ACS 4.0 and earlier
	//--logging systems. This is provided so that formatted
	//--(i.e., printf, scanf, etc) messages are supported.
	//--At some point in time this should
	//--be removed!
	//---------------------------------------------

	/**
	 * This structure's only purpose is to hold information
	 * retrieved by the unformatted2formatted static method.
	 */
	struct DeprecatedLogInfo 
	{
	    Priority priority;
	    std::string message;
	};

	/**
	 * Maximum size of a formatted log message passed to
	 * unformatted2formatted.
	 */
	static const int MAX_MESSAGE_SIZE = 1000;
	
	/**
	 * Method used to convert unformatted printf-style messages
	 * into C++ strings. It must also take take in an ACE Log
	 * Priority parameter because of the way the ACS logging 
	 * macros were setup in ACS 4.0.
	 * @param messagePriority priority of the logging message
	 * @param fmt format of the message (similary to printf's first
	 *        parameter)
	 * @param ... Variable length parameters to be placed into fmt.
	 * @return a structure containing the message priority and the 
	 *         formatted message.
	 */
	static DeprecatedLogInfo
	unformatted2formatted(ACE_Log_Priority messagePriority,
			      const char *fmt,
			      ...) 
#if defined(__GNUC__)
	__attribute__ ((format (printf, 2, 3)))
#endif
	;
	
      private:
	/**
	 * Name of the source object.
	 */
	std::string sourceObjectName_m;
    };

};


#endif /*!logging_log_svc_handler_H*/
