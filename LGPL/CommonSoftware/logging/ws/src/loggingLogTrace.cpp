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
* "@(#) $Id: loggingLogTrace.cpp,v 1.5 2010/02/12 07:33:09 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-28  created 
*/

#include "loggingLogTrace.h"
#include <iostream>


static char *rcsId="@(#) $Id: loggingLogTrace.cpp,v 1.5 2010/02/12 07:33:09 bjeram Exp $"; 
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
    	if (logger!=0)
    	{
    		logger->log(Logger::LM_TRACE,
    				std::string("Entering..."),
    				file,
    				line,
    				method);
    	}
    	else
    	{
    		std::cerr << "SEVERE LOGGING ERROR IN LogTrace/AUTO_TRACE - logger/getLogger() is NULL: routine=";
    		std::cerr << method << " file: " << file << " line: " << line << std::endl;
    	}//if-else
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
