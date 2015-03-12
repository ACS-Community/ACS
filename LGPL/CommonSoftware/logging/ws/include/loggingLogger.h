#ifndef logging_logger_H
#define logging_logger_H
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
* "@(#) $Id: loggingLogger.h,v 1.27 2012/02/29 12:50:09 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-03-09  created
*/

/** @file loggingLogging.h
 *  Header file for abstract Logger.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <lokiSmartPtr.h>
#include <list>
#include "loggingBaseLog.h"
#include "loggingHandler.h"
#include "acsutilTimeStamp.h"
#include <ace/Thread_Mutex.h>
#include <ace/Singleton.h>
#include <ace/Thread_Manager.h>   // it is here just that we can print ID of the thread !!!

namespace Logging 
{

    //------------------------------------------------------------------------------
    /**
     * Logger is an abstract class which provides the interface used to log
     * messages and errors throughout ALMA software. This class assumes nothing
     * about the ACS logging service and is based in part on the implementation
     * of the Java Logger interface.
     */
    class loggingBase_EXPORT Logger : public virtual BaseLog
    {
      public:
	//----------------------------------------------------
	///Logger smart pointer
#ifndef MAKE_VXWORKS
	typedef Loki::SmartPtr<Logger, 
#ifdef __CYGWIN__
			       Logging::RefCounted,
#else
			       Loki::RefCountedMTAdj<Loki::ObjectLevelLockable>::RefCountedMT,
#endif
			       Loki::AllowConversion,
			       Loki::NoCheck,
			       Loki::DefaultSPStorage> LoggerSmartPtr;
#else
	typedef Loki::SmartPtr<Logger, 
			       RefCountedMT,
			       Loki::AllowConversion,
			       Loki::NoCheck,
			       Loki::DefaultSPStorage> LoggerSmartPtr;
#endif

	// Loggers list
	typedef std::list<Logger*> LoggerList;
	
	// Implementation of external function that configures logger 
	typedef void (*ConfigureLoggerFunction)(const std::string& loggerName);
	
	/**
	 * Create a logger for a named subsystem. This method is pure abstract
	 * requiring subclasses to return an instance of their own subclass.
	 * @param loggerName Name of the logger desired (i.e., "POWER_SUPPLY1")
	 * @return A new Logger instance.
	 */
	virtual LoggerSmartPtr
	getLogger(const std::string &loggerName) = 0;

    int getLocalLevel();
    int getRemoteLevel();
    int
    getLocalLevel(const std::string &loggerName);
    int
    getRemoteLevel(const std::string &loggerName);
	/**
	 * Static method used to get a nameless Logger object. 
	 * @return A Logger with an empty name.
	 */
	static LoggerSmartPtr getAnonymousLogger();

	/**
	 * Static method used to set the anonymous Logger
	 * object.
	 * @param The anonymous Logger.
	 */
	static void	setAnonymousLogger(LoggerSmartPtr anonyLogger);
	
	/**
	 * Static method used to get the Logger object for logging from static methods.
	 *  getStaticLogger() is used in ACS_STATIC_XXX macros
	 * @return The static Logger.
	 */
	static LoggerSmartPtr getStaticLogger();

	/**
	 * Static method used to set the static Logger
	 * object.
	 * @param The static Logger.
	 */
	static void	setStaticLogger(LoggerSmartPtr anonyLogger);
	
	/**
	 * Static method used to get the global Logger
	 * object.
	 * @return The global Logger.
	 */
	static LoggerSmartPtr getGlobalLogger();
	
	/**
	 * Static method used to set the global Logger
	 * object.
	 * @param The global Logger.
	 */
	static void setGlobalLogger(LoggerSmartPtr globalLogger);
	
	/**
	 * Retrieves the name (usually a container/component/client/)
	 * of this Logger instance
	 * @return The name of this Logger instance
	 */
	virtual std::string	getName() const;
	
	/**
	 * Destructor.
	 */
	virtual ~Logger();
	
	/**
	 * Add a log Handler to receive logging messages.
	 * 
	 * By default, Loggers also send their output to their parent logger. 
	 * Typically the root Logger is configured with a set of Handlers that 
	 * essentially act as default handlers for all loggers.
	 * @param newHandler_p A handler to add.
	 */
	virtual void
	addHandler(Handler::HandlerSmartPtr newHandler_p);
	
	/**
	 * Remove a log Handler.
	 * @param handlerName is the name of the handler as returned by the 
	 * the getName method of Handler objects.
	 * @return true if the Handle is found/removed and false otherwise.
	 */
	virtual bool 
	removeHandler(const std::string &handlerName);
	
	///Overridden. Invokes the log method of registered Handler objects.
	virtual void
	log(const LogRecord &lr);

	///Overridden. For some reason this signature of "log" cannot be resolved
	///from BaseLog. As a result, it must be defined here and just delegates
	///to BaseLog
	virtual void
	log(Priority priority,
	    const std::string &message,
	    const std::string &file,
	    unsigned long line,
	    const std::string &method)
	    {
		BaseLog::log(priority, message, file, line, method);
	    }

	/**
	 * This signature of log is provided as a pure convenience
	 * to developers not wishing to deal with the file name,
	 * line number, etc.
	 * @param priority Priority of the log
	 * @param message Log message.
	 * @return void
	 */
	virtual void
	log(Priority priority,
	    const std::string &message)
	    {
		log(priority,
		    message,
		    FIELD_UNAVAILABLE,
		    0UL,
		    FIELD_UNAVAILABLE);
	    }
	
	/**
	 * Facilitates changing the name of this logger.
	 * @param newName New name for this Logger instance
	 * @return void
	 */
	virtual void 
	setName(const std::string &newName);

	/**
	 * Set levels for local and remote logging. 
	 * There levels are passed to the handlers.
	 * @return void
	 */
	virtual void
	setLevels(Priority remotePriority,Priority localPriority, int type);
	
	/**
	 * Set levels for local and remote logging of a child logger with given name. 
	 * There levels are passed to the handlers.
	 * @param loggerName logger name.
	 * @return void
	 */
	virtual void
	setLevels(const std::string &loggerName, Priority remotePriority,Priority localPriority, int type);
    
    void setLevelsLoggerHandlers(Priority remotePriority,Priority localPriority, int type);
	/**
	 * Logger existance check.
	 * @param loggerName logger name.
	 * @return bool 
	 */
	virtual bool
	exists(const std::string &loggerName);

	/**
	 * Get all logger names. 
	 * @return logger names.
	 */
	virtual std::list<std::string>
	getLoggerNames();

	/**
	 * Set logger configuration function.
	 */
	static void
	setConfigureLoggerFunction(ConfigureLoggerFunction configureLoggerFunction) {configureLoggerFunction_m=configureLoggerFunction;}
	
	/**
	 * Configure logger.
	 */
	static void
	configureLogger(const std::string& loggerName)
	{
		if (configureLoggerFunction_m)
			(*configureLoggerFunction_m)(loggerName);
	}
	
      protected:
	//----------------------------------------------------
	/**
	 * Constructor.
	 * @param loggerName A name for the logger. This should be a 
	 *        dot-separated name and should normally be based on the container/
	 *        component/client name.
	 */
	Logger(const std::string &loggerName);

	/**
	 * Mmethod used to facilitate logging within a 
	 * multi-threaded environment. Subclasses should override
	 * this method with code used to acquire a mutual exclusion lock.
	 * The developer is free to use any type of mutex they prefer 
	 * (e.g., ACE_Thread_Mutex for example).
	 */ 
	virtual void
	acquireHandlerMutex() {}

	/**
	 * Method used to facilitate logging within a 
	 * multi-threaded environment. Subclasses should override
	 * this method with code used to release a mutual exclusion lock.
	 * The developer is free to use any type of mutex they prefer 
	 * (e.g., ACE_Thread_Mutex for example).
	 */ 
	virtual void 
	releaseHandlerMutex() {}
	
      private:
	//----------------------------------------------------
	/**Name of this logger*/
	std::string loggerName_m;
	
	/** List of handlers which will have their "log" method invoked
	 *  each time a log is received. DWF-access to this should be guarded
	 *  by a mutex lock.
	 */
	std::list<Handler::HandlerSmartPtr> handlers_m;

        class Logger_ptr{
                public:
                        Logger_ptr(){globalLogger_m=(Logger *)0;anonymousLogger_m=(Logger *)0;staticLogger_m=(Logger *)0;}
                        ~Logger_ptr(){globalLogger_m=(Logger *)0;anonymousLogger_m=(Logger *)0;staticLogger_m=(Logger *)0;}
                        /**
                         * Global logger. By changing this (via the the setGlobalLogger),
                         * one can completely alter the way logs are handled throughout the system.
                         */
                        LoggerSmartPtr globalLogger_m;
                        /**
                         * Anonymous logger. To be used in situations where the logger's name is
                         * irrelevant.
                         */
                        LoggerSmartPtr anonymousLogger_m;
                        
                        /**
                          * Static logger. To be used in static methods.
                          */
                        LoggerSmartPtr staticLogger_m;
                        
                        /**
                         * List of all child loggers.
                         */
                        LoggerList loggers_m;
        };

	/**
	 * mutex which guards the loggers lost making this class completely
	 * thread-safe
	 */
	static ACE_Thread_Mutex loggersMutex_m;

	/**
	 * External function that configures logger.
	 */
	static ConfigureLoggerFunction configureLoggerFunction_m;

    };
    //------------------------------------------------------------------------------
};

#endif /*!_H*/
