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
* "@(#) $Id: loggingLogTrace.cpp,v 1.4 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-28  created 
*/

#include "loggingLogTrace.h"

static char *rcsId="@(#) $Id: loggingLogTrace.cpp,v 1.4 2006/01/05 18:45:10 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace Logging {
    //------------------------------------------------------------------------------
    LogTrace::LogTrace(Logger::LoggerSmartPtr logger,
		       const std::string &method,
		       const std::string &file,
		       unsigned long line) :
	logger_m(logger),
	methodName_m(method),
	fileName_m(file),
	lineNumber_m(line)
    {
	//just delegate to helper method.
	entryLog(logger_m,
		 methodName_m,
		 fileName_m,
		 lineNumber_m);
    }
    //------------------------------------------------------------------------------
    LogTrace::LogTrace(Logger::LoggerSmartPtr logger,
		       const std::string &method) :
	logger_m(logger),
	methodName_m(method),
	fileName_m(std::string("Unavailable")),
	lineNumber_m(0UL)
    {
	//just delegate to helper method.
	entryLog(logger_m,
		 methodName_m,
		 fileName_m,
		 lineNumber_m);
    }
    //------------------------------------------------------------------------------
    void
    LogTrace::entryLog(Logger::LoggerSmartPtr logger,
		       std::string method,
		       std::string file,
		       unsigned long line)
    {
	logger->log(Logger::LM_TRACE,
		    std::string("Entering..."),
		    file,
		    line,
		    method);	
    }
    //------------------------------------------------------------------------------
    LogTrace::~LogTrace()
    {
	logger_m->log(Logger::LM_TRACE,
			       std::string("Exiting..."),
			       fileName_m,
			       lineNumber_m,
			       methodName_m);	
    }
    //------------------------------------------------------------------------------
};


/*___oOo___*/
