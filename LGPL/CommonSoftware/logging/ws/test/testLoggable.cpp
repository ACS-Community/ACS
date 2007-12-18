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
* "@(#) $Id: testLoggable.cpp,v 1.2 2007/12/18 09:33:14 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-31  created
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "logging.h"
#include "loggingLoggable.h"

static char *rcsId="@(#) $Id: testLoggable.cpp,v 1.2 2007/12/18 09:33:14 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


class TestLoggable : public virtual Logging::Loggable
{
  public:
    TestLoggable(): Loggable()
	{
	}
    TestLoggable(const std::string &loggerName): Loggable(loggerName)
	{
	}
    TestLoggable(Logging::Logger::LoggerSmartPtr logger): Loggable(logger)
	{
	}
    void doTestLogs()
	{
	    std::string message = "This is a logger named: " + getLogger()->getName();
	    getLogger()->log(Logging::Logger::LM_INFO, message);
	}
    static void doTestStaticLogs()
	{
	    AUTO_STATIC_TRACE("TestLoggable::doTestStaticLogs");
	}
};

int main(int argc, char *argv[])
{
    ACS_CHECK_LOGGER;
    Logging::Logger::LoggerSmartPtr globalLoggerSmartPtr = getNamedLogger("MainLogger");

    printf("-------------------------------------------------------------------------\n");
    globalLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "This is a global level log message.");
    
    
    TestLoggable noName = TestLoggable();
    TestLoggable withName = TestLoggable("loggableWithName");
    TestLoggable fromLogger = TestLoggable(globalLoggerSmartPtr);

    noName.doTestLogs();
    withName.doTestLogs();
    fromLogger.doTestLogs();
    noName.doTestLogs();
    withName.doTestLogs();
    fromLogger.doTestLogs();

    printf("-------------------------------------------------------------------------\n");
    printf("Here we test if static log macro(s) work(s)\n");
    TestLoggable::doTestStaticLogs();

    return 0;
}
