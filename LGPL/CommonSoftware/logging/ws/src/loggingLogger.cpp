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
* "@(#) $Id: loggingLogger.cpp,v 1.22 2012/01/20 22:07:44 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-28  created 
*/

#include "loggingLogger.h"
#include <functional>
#include <iostream>
#include <sstream>
#include <ace/Recursive_Thread_Mutex.h>

static char *rcsId="@(#) $Id: loggingLogger.cpp,v 1.22 2012/01/20 22:07:44 tstaig Exp $"; 
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
    ACE_Thread_Mutex Logger::loggersMutex_m;
    Logger::ConfigureLoggerFunction Logger::configureLoggerFunction_m = (ConfigureLoggerFunction)0;
    // -------------------------------------------------------
    Logger::LoggerSmartPtr
        Logger::getStaticLogger()
        {
    	if (ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->staticLogger_m == 0)
    	    {
    	    //ok, now check is getGlobalLogger returns something other than null...
    	    if (getGlobalLogger()!=0)
    		{
    		//delegate to the global logger
    		ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->staticLogger_m = getGlobalLogger()->getLogger(BaseLog::STATIC_LOGGER_NAME);
    		}//if
    	 }//if

    	//just delegate to abstract method implementation
    	return ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->staticLogger_m;

        }//Logger::getStaticLogger()
        // -------------------------------------------------------
    Logger::LoggerSmartPtr
    Logger::getAnonymousLogger()
    {
	if (ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->anonymousLogger_m == 0)
	    {
	    //ok, now check is getGlobalLogger returns something
	    //other than null...
	    if (getGlobalLogger()!=0)
		{
		//delegate to the global logger
		ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->anonymousLogger_m = getGlobalLogger()->getLogger(BaseLog::ANONYMOUS_LOGGER_NAME);
		}
	    }

	//just delegate to abstract method implementation
	return ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->anonymousLogger_m;

    }//Logger::getAnonymousLogger()
    // -------------------------------------------------------
    Logger::LoggerSmartPtr
    Logger::getGlobalLogger()
    {
	return ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->globalLogger_m;
    }
    // -------------------------------------------------------
    void
    Logger::setGlobalLogger(Logger::LoggerSmartPtr globalLogger)
    {
	ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->globalLogger_m = globalLogger;
    }
    // -------------------------------------------------------
    void
    Logger::setStaticLogger(Logger::LoggerSmartPtr staticLogger)
    {
    ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->staticLogger_m = staticLogger;
    }
    // -------------------------------------------------------
    void
    Logger::setAnonymousLogger(Logger::LoggerSmartPtr anonyLogger)
    {
	ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->anonymousLogger_m = anonyLogger;
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
    // remove from loggers list
    if (loggerName_m != BaseLog::GLOBAL_LOGGER_NAME &&
    	this != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->globalLogger_m && 
    	this != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->anonymousLogger_m && 
    	this != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->staticLogger_m
    	)
	{
        //printf("Deleting Logger: \"%s\"\n",loggerName_m.c_str());
        loggersMutex_m.acquire();
        ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.remove(this);
        loggersMutex_m.release();
	}//if
	
	//to be thread safe
	acquireHandlerMutex();
	//simply clear the list of handlers
	handlers_m.clear();
	//make sure it's released
	releaseHandlerMutex();
    }//Logger::~Logger
    // -------------------------------------------------------
    void
    Logger::addHandler(Handler::HandlerSmartPtr newHandler_p)
    {
	//to be thread safe
	acquireHandlerMutex();
	// just in case we put here try-catch
	try
	    {
	//add the copy to our own list of handlers
	//if(exists(newHandler_p->getName())){ return;}
	handlers_m.push_back(newHandler_p);
	    }
	catch(...)
	    {
	    printf("\n\n ===> Logger::addHandler exception thrown !!!!!! Thread ID: %lu.\n\n", (long)ACE_Thread_Manager::instance()->thr_self());
	    }
	//make sure it's released
	releaseHandlerMutex();
	// configure logger (this will also configure handler)
	configureLogger(loggerName_m);
    }
    void
	Logger::setLevelsLoggerHandlers(Priority remotePriority,Priority localPriority, int type)
    {
	//to be thread safe
	acquireHandlerMutex();
	// just in case - for debugging 
	try
	    {
	    std::list<Handler::HandlerSmartPtr>::iterator pos;
	
	    for (pos = handlers_m.begin();
		 pos != handlers_m.end();
		 pos++){
	    
	    (*pos)->setLevels(remotePriority,localPriority,type);
	    }
	    }
	catch(...)
	    {
	    printf("\n\n ===> Logger::setLevelsLoggerHandlers exception thrown !!!!!! Thread ID: %lu.\n\n", (long)ACE_Thread_Manager::instance()->thr_self());
	    }
	//make sure it's released
	releaseHandlerMutex();
    }
    // -------------------------------------------------------
    void
	Logger::setLevels(Priority remotePriority,Priority localPriority, int type)
    {
	//to be thread safe
	loggersMutex_m.acquire();
     
	LoggerList::iterator pos;
	
	for (pos = ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.begin();
	     pos != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.end();
	     pos++)
			(*pos)->setLevelsLoggerHandlers(remotePriority, localPriority, type);
	//make sure it's released
	loggersMutex_m.release();
    }

    bool
    Logger::removeHandler(const std::string &handlerName)
    {
	//assume the handler we're searching for does not 
	//exist
	bool retVal = false;


	//to be thread safe
	acquireHandlerMutex();
	
	//initial number of elements
	unsigned int numElements = handlers_m.size();
     
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

    int Logger::getLocalLevel(){
       std::list<Handler::HandlerSmartPtr>::iterator pos; 
        for (pos = handlers_m.begin();
             pos != handlers_m.end();
             pos++){
                return (*pos)->getLocalLevel();
        }
        return -1;
    }

        
    int Logger::getRemoteLevel(){
       std::list<Handler::HandlerSmartPtr>::iterator pos; 
        for (pos = handlers_m.begin();
             pos != handlers_m.end();
             pos++){
                return (*pos)->getRemoteLevel();
        }
        return -1;
    }

    int Logger::getLocalLevel(const std::string &loggerName){
            
        LoggerList::iterator pos;
        
        for (pos = ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.begin();
             pos != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.end();
             pos++){
            if ((*pos)->getName() == loggerName){
                return (*pos)->getLocalLevel();
            }
        }
        return -1;
    }

    int Logger::getRemoteLevel(const std::string &loggerName){
            
        LoggerList::iterator pos;
        
        for (pos = ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.begin();
             pos != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.end();
             pos++){
            if ((*pos)->getName() == loggerName){
                return (*pos)->getRemoteLevel();
            }
        }
        return -1;
    }

    
    void
	Logger::setLevels(const std::string &loggerName, Priority remotePriority, Priority localPriority, int type)
    {
	//to be thread safe
	loggersMutex_m.acquire();
     
	LoggerList::iterator pos;
	
	for (pos = ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.begin();
	     pos != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.end();
	     pos++)
		if ((*pos)->getName() == loggerName){
			(*pos)->setLevelsLoggerHandlers(remotePriority, localPriority, type);
		}
	//make sure it's released
	loggersMutex_m.release();
    }
    // -------------------------------------------------------
    bool
	Logger::exists(const std::string &loggerName)
    {
	//to be thread safe
	loggersMutex_m.acquire();

	bool found = false;
	LoggerList::iterator pos;
	for (pos = ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.begin();
	     !found && pos != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.end();
	     pos++){
		if ((*pos)->getName() == loggerName)
			found = true;
	}
	//make sure it's released
	loggersMutex_m.release();
	
	return found;
    }
    // -------------------------------------------------------
    std::list<std::string>
	Logger::getLoggerNames()
    {
	std::list<std::string> names;
	//to be thread safe
	loggersMutex_m.acquire();
     
	LoggerList::iterator pos;
	
	for (pos = ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.begin();
	     pos != ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.end();
	     pos++)
	    if (find(names.begin(), names.end(), (*pos)->getName()) == names.end())
	    	names.push_back((*pos)->getName());

	//make sure it's released
	loggersMutex_m.release();

	return names;
    }
    // -------------------------------------------------------
    void
    Logger::log(const LogRecord &lr)
    {
    	// Message sizes greater than ACE_MAXLOGMSGLEN cause memory errors
    	// So they are trunkated and a warning is sent
    	if (lr.message.size() > ACE_MAXLOGMSGLEN)
    	{
    		// Send the information via stdout
    		std::cout << "The following log message exceeds ACE_MAXLOGMSGLEN and will be truncated: " << lr.message << std::endl;
    		// Message is trunkated
    		lr.message.resize(ACE_MAXLOGMSGLEN);
    		// Add specific log message
			LogRecord logItem;
			logItem.priority = lr.priority;
			logItem.file = __FILE__;
			logItem.line = __LINE__;
			logItem.method = __PRETTY_FUNCTION__;
			logItem.timeStamp = getTimeStamp();
			std::ostringstream oss;
			oss.clear();
			oss.str(std::string());
			oss << "A log message has been truncated to the value of ACE_MAXLOGMSGLEN = " << ACE_MAXLOGMSGLEN;
			logItem.message = oss.str();
			log(logItem);
    	}

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
	    //in...(we cannot suppose this, because of the env variables
	    
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
    	// add to logger list
    	if (loggerName_m != BaseLog::GLOBAL_LOGGER_NAME)
    	{
                //printf("Adding Logger: \"%s\"\n",loggerName_m.c_str());
		loggersMutex_m.acquire();
    		ACE_Singleton<Logger_ptr, ACE_Recursive_Thread_Mutex>::instance()->loggers_m.push_back(this);
    		loggersMutex_m.release();
    	}
    }
    // -------------------------------------------------------
};

/*___oOo___*/
