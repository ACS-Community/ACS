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
* "@(#) $Id: loggingLogger.cpp,v 1.10 2006/01/05 18:45:10 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-28  created 
*/

#include "loggingLogger.h"
#include <functional>
#include <iostream>

static char *rcsId="@(#) $Id: loggingLogger.cpp,v 1.10 2006/01/05 18:45:10 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);
// -------------------------------------------------------
//helper function
//returns true if the name of a string matches the name of the handler
bool 
checkHandlerEquality(Logging::Handler::HandlerSmartPtr handler,
		     std::string name)
{
    if (handler->getName()==name)
	{
	return true;
	}
    else
	{
	return false;
	}
}
// -------------------------------------------------------
namespace Logging {
    Logger::LoggerSmartPtr Logger::globalLogger_m = (Logger *)0;
    Logger::LoggerSmartPtr Logger::anonymousLogger_m = (Logger *)0;
    // -------------------------------------------------------
    Logger::LoggerSmartPtr
    Logger::getAnonymousLogger()
    {
	//first check to see if the static member has been
	//set yet...
	if (anonymousLogger_m == 0)
	    {
	    //ok, now check is getGlobalLogger returns something
	    //other than null...
	    if (getGlobalLogger()!=0)
		{
		//delegate to the global logger
		anonymousLogger_m = getGlobalLogger()->getLogger(BaseLog::ANONYMOUS_LOGGER_NAME);
		}
	    }

	//just delegate to abstract method implementation
	return anonymousLogger_m;
    }
    // -------------------------------------------------------
    Logger::LoggerSmartPtr
    Logger::getGlobalLogger()
    {
	return globalLogger_m;
    }
    // -------------------------------------------------------
    void
    Logger::setGlobalLogger(Logger::LoggerSmartPtr globalLogger)
    {
	globalLogger_m = globalLogger;
    }
    // -------------------------------------------------------
    void
    Logger::setAnonymousLogger(Logger::LoggerSmartPtr anonyLogger)
    {
	anonymousLogger_m = anonyLogger;
    }
    // -------------------------------------------------------
    std::string
    Logger::getName() const
    {
	return loggerName_m;
    }
    // -------------------------------------------------------
    void
    Logger::setName(const std::string &newName) 
    {
	loggerName_m = newName;
    }   
    // -------------------------------------------------------
    Logger::~Logger()
    {
	//to be thread safe
	acquireHandlerMutex();
	//simply clear the list of handlers
	handlers_m.clear();
	//make sure it's released
	releaseHandlerMutex();
    }
    // -------------------------------------------------------
    void
    Logger::addHandler(Handler::HandlerSmartPtr newHandler_p)
    {
	//to be thread safe
	acquireHandlerMutex();
	//add the copy to our own list of handlers
	handlers_m.push_back(newHandler_p);
	//make sure it's released
	releaseHandlerMutex();
    }
    // -------------------------------------------------------
    bool
    Logger::removeHandler(const std::string &handlerName)
    {
	//assume the handler we're searching for does not 
	//exist
	bool retVal = false;

	//initial number of elements
	unsigned int numElements = handlers_m.size();

	//to be thread safe
	acquireHandlerMutex();
     
	//sanity check. should be the case but who knows?
	if (handlers_m.empty()==false)
	    {
	    //C++ requires us to provide a comparison function
	    //for Handler objects and strings.
	    handlers_m.remove_if(std::bind2nd(std::ptr_fun(checkHandlerEquality), 
					      handlerName));
	    }

	//check to see if something got removed
	if (numElements != handlers_m.size())
	    {
	    retVal = true;
	    }

	//make sure it's released
	releaseHandlerMutex();
	
	return retVal;
    }
    // -------------------------------------------------------
    void
    Logger::log(const LogRecord &lr)
    {
// remove copy this std::list, because the core file shows something wrong here
// according to trace, handlers_m never changed after initialized, so, it seems OK to use handlers_m directly
//	//to be thread safe
//	acquireHandlerMutex();
//	
//	//sanity check. should never be the case but who knows?
//	if (handlers_m.empty()==true)
//	    {
//	    //make sure it's released
//	    releaseHandlerMutex();
//	    return;
//	    }
//	
//	std::list<Handler::HandlerSmartPtr> handlersCopy = handlers_m;
//	releaseHandlerMutex();
	
	std::list<Handler::HandlerSmartPtr>::iterator pos;
	
//	for (pos = handlersCopy.begin();
//	     pos != handlersCopy.end();
	for (pos = handlers_m.begin();
	     pos != handlers_m.end();
	     pos++)
	    {
	    //if the priority of this log is greater than or equal
	    //to the minimum logging level the handler is interested
	    //in...
	    if(lr.priority >= (*pos)->getLevel())
		{
		//...go ahead and pass the log message to the handler
		try
		    {
		    (*pos)->log(lr);
		    }
		//in the event the handler throws any type of exception
		//send the failure to stdout (relogging it at LM_CRITICAL
		//priority could lead to infinite recursion)
		catch(...)
		    {
		    std::cerr << "CRITICAL LOGGER FAILURE: the '";
		    std::cerr << (*pos)->getName() << "' Handler registered to process logs for the '";
		    std::cerr << this->getName()   << "' Logger failed to log the '";
		    std::cerr << lr.message           << "' message originating from the '";
		    std::cerr << lr.file              << "' file!" << std::endl;
		    }
		}
	    }

	//make sure it's released
	// releaseHandlerMutex();
    }
    // -------------------------------------------------------
    Logger::Logger(const std::string &loggerName) :
	loggerName_m(loggerName)
    {
	
    }
    // -------------------------------------------------------
};

/*___oOo___*/
