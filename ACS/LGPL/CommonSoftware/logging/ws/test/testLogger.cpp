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
* "@(#) $Id: testLogger.cpp,v 1.7 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-31  created
*/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <iostream>

#include "loggingACSLogger.h"
#include "loggingHandler.h"
#include "loggingLogTrace.h"
#include "logging.h"

namespace Logging {
    
    class TestHandler : public virtual Handler
    {
      public:
	TestHandler(const std::string& name)
	    {
		setLevel(LM_TRACE);
		name_m = name;
	    }
	~TestHandler()
	    {
		std::cout << "TestHandler destructor: " << getName() << std::endl;
	    }


	virtual void
	log(const LogRecord& lr)
	    {
		std::cout << lr.priority << " " << lr.message << " " << lr.file << " " << lr.line << " " << lr.method << std::endl;
	    }

	virtual std::string
	getName() const
	    {
		return name_m;
	    }
	
	std::string name_m;
    };
};

static char *rcsId="@(#) $Id: testLogger.cpp,v 1.7 2006/01/05 18:45:10 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


int main(int argc, char *argv[])
{
    ACS_CHECK_LOGGER;
    Logging::Logger::LoggerSmartPtr myLoggerSmartPtr = getLogger();

    printf("-------------------------------------------------------------------------\n");
    //simple test should print the message just once
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Simple test...should see once for ACS log svc handler.");
    
    //add custom handler.
    {
    Logging::Handler::HandlerSmartPtr localHandler(new Logging::TestHandler("h1"));
    myLoggerSmartPtr->addHandler(localHandler);
    }
    
    //add second custom handler of same name
    {
    Logging::Handler::HandlerSmartPtr localHandler(new Logging::TestHandler("h1"));
    myLoggerSmartPtr->addHandler(localHandler);
    }

    //add another handler
    Logging::Handler::HandlerSmartPtr localHandler2(new Logging::TestHandler("h2"));
    myLoggerSmartPtr->addHandler(localHandler2);

    //add another handler
    Logging::Handler::HandlerSmartPtr localHandler3(new Logging::TestHandler("h3"));
    myLoggerSmartPtr->addHandler(localHandler3);
    
    {
    printf("-------------------------------------------------------------------------\n");
    printf("Should be a trace begin here...\n");
    Logging::LogTrace::LogTraceSmartPtr joe(new Logging::LogTrace(getLogger(),
								  "no method name",
								  __FILE__,
								  __LINE__));
    printf("...something in the middle...\n");
    } 
    printf("...and the trace ending should be done now.\n");
    printf("-------------------------------------------------------------------------\n");
    //simple test should print the message for each handler plus one for the 
    //log svc. handler
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Simple test with multiple handlers...should see five times.");
    printf("-------------------------------------------------------------------------\n");
    //test priority
    localHandler2->setLevel(Logging::Logger::LM_WARNING);
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Test priority...should see four times");
    localHandler2->setLevel(Logging::Logger::LM_TRACE);
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Test priority...should see five times");
    //take it out of commission so to speak...
    localHandler2->setLevel(Logging::Logger::LM_WARNING);
    printf("-------------------------------------------------------------------------\n");
    //tests removing a handler
    myLoggerSmartPtr->removeHandler(localHandler3->getName());
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Test remove 1 handler...should see three times");
    printf("-------------------------------------------------------------------------\n");
    //try removing a handler that doesn't exist
    myLoggerSmartPtr->removeHandler("nada");
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Test removing non-existant handler...should see three times");
    printf("-------------------------------------------------------------------------\n");
    myLoggerSmartPtr->removeHandler("h1");
    myLoggerSmartPtr->log(Logging::Logger::LM_INFO,
			  "Test with two more handlers of identical names removed - should see once");
    
    return 0;
}
