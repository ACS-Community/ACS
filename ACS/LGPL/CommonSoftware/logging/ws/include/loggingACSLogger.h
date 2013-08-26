#ifndef logging_acs_logger_H
#define logging_acs_logger_H
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
* "@(#) $Id: loggingACSLogger.h,v 1.2 2005/04/12 22:47:49 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created
*/


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingLogger.h"
#include <ace/Thread_Mutex.h>

namespace Logging {

    /**
     * Concrete implementation of the abstract Logger class. This class
     * includes public constructors and has at least one Handler registered
     * with it (i.e., the one dealing with the acsLogSvc).
     */
    class ACSLogger : public virtual Logger
    {
      public:
	// -------------------------------------------------------------
	/**
	 * Stamdard constructor.
	 * @param loggerName name of this logger
	 */
	ACSLogger(const std::string &loggerName);

	/**
	 * Destructor
	 */
	virtual ~ACSLogger();

	/**
	 * Overridden from baseclass. Returns an ACSLogger.
	 */
	virtual LoggerSmartPtr
	getLogger(const std::string &loggerName);

      protected:
	// -------------------------------------------------------------
	/**
	 * Overridden from baseclass.
	 */ 
	virtual void
	acquireHandlerMutex();

	/**
	 * Overridden from baseclass.
	 */ 
	virtual void 
	releaseHandlerMutex();

      private:
	// -------------------------------------------------------------
	/**
	 * mutex which guards the handlers making this class completely
	 * thread-safe
	 */
	ACE_Thread_Mutex handlersMutex_m;
    };
};


#endif /*!logging_acs_logger_H*/
