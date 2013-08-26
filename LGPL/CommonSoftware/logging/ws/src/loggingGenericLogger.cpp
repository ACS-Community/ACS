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
* "@(#) $Id: loggingGenericLogger.cpp,v 1.1 2005/08/09 00:45:39 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created 
*/

#include "loggingGenericLogger.h"
#include "loggingStdoutHandler.h"

static char *rcsId="@(#) $Id: loggingGenericLogger.cpp,v 1.1 2005/08/09 00:45:39 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace Logging {
    // -------------------------------------------------------------
    GenericLogger::GenericLogger(const std::string &loggerName) :
	Logger(loggerName)
    {
	Handler::HandlerSmartPtr acsHandler(new StdoutHandler(this->getName()));
	this->addHandler(acsHandler);
    }
    // -------------------------------------------------------------
    GenericLogger::~GenericLogger()
    {
    }
    // -------------------------------------------------------------
    Logger::LoggerSmartPtr
    GenericLogger::getLogger(const std::string &loggerName)
    {
	LoggerSmartPtr retVal(new GenericLogger(loggerName));
	return retVal;
    }
    // -------------------------------------------------------------
};

/*___oOo___*/
