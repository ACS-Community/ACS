#ifndef logging_stdout_handler_H
#define logging_stdout_handler_H
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
* "@(#) $Id: loggingStdoutHandler.h,v 1.2 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "loggingHandler.h"


namespace Logging {
    
    /**
     * Handler class which sends logs to stdout
     */
    class StdoutHandler : public virtual Handler
    {
      public:
	// --------------------------------------------------------
	/**
	 * Standard constructor.
	 * @param soName Name of the source object. Typically this is the name of the
	 *        Logger which is using this Handler-derived object.
	 */
	StdoutHandler(const std::string& soName);

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

      private:
	/**
	 * Name of the source object.
	 */
	std::string sourceObjectName_m;
    };

};


#endif /*!logging_stdout_handler_H*/
