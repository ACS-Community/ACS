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
* "@(#) $Id: loggingACSLogger.cpp,v 1.5 2008/01/22 12:00:35 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created 
*/

#include "loggingACSLogger.h"
#include "loggingLogSvcHandler.h"

static char *rcsId="@(#) $Id: loggingACSLogger.cpp,v 1.5 2008/01/22 12:00:35 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

namespace Logging {
    // -------------------------------------------------------------
    ACSLogger::ACSLogger(const std::string &loggerName) :
	Logger(loggerName)
    {
	Handler::HandlerSmartPtr acsHandler(new LogSvcHandler(this->getName()));
	this->addHandler(acsHandler);
    }
    // -------------------------------------------------------------
    ACSLogger::~ACSLogger()
    {
    }
    // -------------------------------------------------------------
    Logger::LoggerSmartPtr
    ACSLogger::getLogger(const std::string &loggerName)
    {
	LoggerSmartPtr retVal(new ACSLogger(loggerName));
	return retVal;
    }
    // -------------------------------------------------------------
    void
    ACSLogger::acquireHandlerMutex()
    {
	ACE_Time_Value rs_timevalue(30, 0);
        ACE_Time_Value sleepTime(ACE_OS::gettimeofday() + rs_timevalue);
        if(handlersMutex_m.acquire(&sleepTime)==-1)
	    {
	    printf("====================ACSLogger::acquireHandlerMutex Mutex acquiring failed!!!!!!!!!!!! Forcing abort\n\n\n");
	    abort();
	    }
    }
    // -------------------------------------------------------------
    void
    ACSLogger::releaseHandlerMutex()
    {
	handlersMutex_m.release();
    }
    // -------------------------------------------------------------
};


/*___oOo___*/
