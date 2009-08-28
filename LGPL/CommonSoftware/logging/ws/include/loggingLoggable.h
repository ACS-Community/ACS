#ifndef logging_loggable_H
#define logging_loggable_H
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
* "@(#) $Id: loggingLoggable.h,v 1.2 2009/08/28 08:27:26 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-09  created
*/

/** @file loggingLogging.h
 *  Header file for abstract Logger.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingLogger.h"


namespace Logging 
{
    //------------------------------------------------------------------------------
    /**
     * Loggable is a base class for all classes that want to be able
     * to use the logging system.
     * It has just to very basic simple features:
     * - Upon construction it gets a Logger
     * - It returns the Logger upon request 
     */
    class Loggable
    {
      public:
	/**
	 * Constructor without parameters.
	 * This will set the default global logger.
	 */
	Loggable();

	/**
	 * Constructor with a logger name.
	 * This will instantiate a new logger with the given name.
	 * @param loggerName A name for the logger. This should be a 
	 *        dot-separated name and should normally be based on the container/
	 *        component/client name.
	 */
	Loggable(const std::string &loggerName);

	/**
	 * Constructor from another logger.
	 * @param logger A smart pointer to an existing logger. 
	 *               This is assigned to the internal smart pointer
	 *               so that the reference counting will make sure
	 *               it will not be destroyed until in usage.
	 */
	Loggable(Logger::LoggerSmartPtr logger);


	/**
	 * Returns the Logger to be used for logs using a standrd interface.
	 * Can be overwritten in special cases.
	 * @return The Logger instance.
	 */
	virtual Logger::LoggerSmartPtr	getLogger() const;
	

	/**
	 * Sets logger. In this way is possible to  (re)set the logger latter.
	  A smart pointer to an existing logger.
	 *               This is assigned to the internal smart pointer
	 *               so that the reference counting will make sure
	 *               it will not be destroyed until in usage.
	 */
	virtual void setLogger(Logger::LoggerSmartPtr logger){ logger_m = logger; }

	/**
	 * Destructor.
	 */
	virtual ~Loggable();
	
      private:
	/**
	 * The logger
	 */
	Logger::LoggerSmartPtr logger_m;
    };
    //------------------------------------------------------------------------------
};

#endif /*!_H*/
