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
* "@(#) $Id: loggingBaseLog.cpp,v 1.6 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-28  created 
*/

#include "loggingBaseLog.h"
#include "acsutilTimeStamp.h"

static char *rcsId="@(#) $Id: loggingBaseLog.cpp,v 1.6 2006/01/05 18:45:10 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace Logging {
    //-----------------------------------------------------------------------------------
    const std::string BaseLog::FIELD_UNAVAILABLE     = std::string("Unavailable");
    const std::string BaseLog::GLOBAL_LOGGER_NAME    = std::string("GlobalLogger");
    const std::string BaseLog::ANONYMOUS_LOGGER_NAME = std::string("");
    const std::string BaseLog::STATIC_LOGGER_NAME    = std::string("StaticMethodLogger");
    //-----------------------------------------------------------------------------------
    void
    BaseLog::log(Priority priority,
		 const std::string &message,
		 const std::string &file,
		 unsigned long line,
		 const std::string &method)
    {
	//create a log record
	LogRecord lr;
	
	//fill it out
	lr.priority  = priority;
	lr.message   = message;
	lr.file      = file;
	lr.line      = line;
	lr.method    = method;
	lr.timeStamp = getTimeStamp();

	//delegate to another signature
	log(lr);
    }
};

// -------------------------------------------------------
/*___oOo___*/
