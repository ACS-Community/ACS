#ifndef REPEATGUARDLOGGER_H
#define REPEATGUARDLOGGER_H
/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) Associated Universities Inc., 2007 
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
 * "@(#) $Id: RepeatGuardLogger.h,v 1.5 2007/03/23 09:50:06 nbarriga Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * nbarriga  2007-02-26  created
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutilTimeStamp.h>
#include <loggingACSLogger.h>

#include "RepeatGuard.h"


namespace Logging 
{

    /**
     * @class RepeatGuardLogger
     * @brief Guard template class against log repetitions.
     *
     * This is a template class for guarding logs against logging too often.
     * It inherits from RepeatGuard and adds different
     * implementations of the log() method.
     * 
     * In all cases the log() method checks the guarding condition
     * and, if it is the case, forwards the log() command
     * to the given underlying logger object.
     *
     * Before doing this, it adds the repeatCount additional data
     * field to the log to notify that repeated logs have been
     * skipped according to the guard condition.
     *
     * Notes:
     *
     * 1. The implementation of the template exploits
     *    "partial template instantiation" (see Alexandrescu, 
     *    Modern C++ Design, 1.8).
     *    The three signatures for the log() method are not supported
     *    by all loggers.
     *    What is important is that the log() signarues called for
     *    each specific instantiation of the template are supported.
     *    
     *    Notice that the log() methods cannot be virtual, otherwise
     *    partial template instantiation would not work.
     *
     * 2. The logger object is passed as argument of the log method
     *    and not in the constructor of the guard to allow using
     *    the same guard with multiple loggers.
     *    This (in my opinion) extremely useful additionl flexibility
     *    comes to the price of one additional parameter in the
     *    function call.
     *
     * 3. Now only the log() method is implemented.
     *    This automaticaly increments the ocunter.
     *    It would be probably much better to have instead two methods
     *    (as was in the original design):
     *    - log()
     *    - logAndIncrement()
     *    Can you confirm this?
     *    If nobody vote against, I will add the methods.
     *
     * 4. Here and in the base RepeatGuard class, the interval
     *    time is an integer in seconds.
     *    For consistency with the other interfaces
     *    it should be instead a TimeInterval (long long in 100ns())
     *    as defined in acscommon.idl.
     *    If nobody vote against, I will change the signature.
     */
    template <class ALogger> class RepeatGuardLogger : RepeatGuard 
    {

      private:

      protected:

      public:

	/** Constructor
	 *  @param interval minimum interval between allowing an action 
	 *         (i.e. check returns true)
	 *  @param maxRepetitions override minimum interval if 
	 *         maxRepetitions is reached.(0 disables this feature)
	 */
	RepeatGuardLogger(ACS::TimeInterval interval, 
			  unsigned int maxRepetitions=0); 
	virtual ~RepeatGuardLogger();

	void log(ALogger &logger );
	void log(Logging::Logger::LoggerSmartPtr &logger, Logging::BaseLog::Priority priority,
		 const std::string &message,
		 const std::string &file,
		 unsigned long line,
		 const std::string &method);
	void log(Logging::Logger::LoggerSmartPtr &logger, 
		 const Logging::BaseLog::LogRecord &lr);

	void logAndIncrement(ALogger &logger );
	void logAndIncrement(Logging::Logger::LoggerSmartPtr &logger, Logging::BaseLog::Priority priority,
		 const std::string &message,
		 const std::string &file,
		 unsigned long line,
		 const std::string &method);
	void logAndIncrement(Logging::Logger::LoggerSmartPtr &logger, 
		 const Logging::BaseLog::LogRecord &lr);

    }; /* end RepeatGuardLogger */

}; /* end namespace Logging */


/*********************************************************
 * @todo
 * Implementation of methods.
 * To be later on moved in .i file
 */

template <class ALogger> 
Logging::RepeatGuardLogger<ALogger>::RepeatGuardLogger(ACS::TimeInterval interval, 
						       unsigned int maxRepetitions) :  
    RepeatGuard(interval,maxRepetitions)
{
}

template <class ALogger> 
Logging::RepeatGuardLogger<ALogger>::~RepeatGuardLogger() 
{
}

template <class ALogger>     
void Logging::RepeatGuardLogger<ALogger>::log(ALogger &logger ) 
{
    if(check())
	{
	std::stringstream strstr;
	strstr << count();

	LoggingProxy::AddData("repeatCount", strstr.str().c_str() );

	logger.log();
	}
};

template <class ALogger>     
void Logging::RepeatGuardLogger<ALogger>::log(Logging::Logger::LoggerSmartPtr &logger, 
				    Logging::BaseLog::Priority priority,
				    const std::string &message,
				    const std::string &file,
				    unsigned long line,
				    const std::string &method)
{
    if(check())
	{
	std::stringstream strstr;
	strstr << count();

	LoggingProxy::AddData("repeatCount", strstr.str().c_str() );

	logger->log(priority, message,
		    file, line, method);
	}
}

    
template <class ALogger>     
void Logging::RepeatGuardLogger<ALogger>::log(Logging::Logger::LoggerSmartPtr &logger, 
				    const Logging::BaseLog::LogRecord &lr)
{
    if(check())
	{
	std::stringstream strstr;
	strstr << count();

	LoggingProxy::AddData("repeatCount", strstr.str().c_str() );

	logger->log(lr);
	}
}
template <class ALogger>     
void Logging::RepeatGuardLogger<ALogger>::logAndIncrement(ALogger &logger ) 
{
    if(checkAndIncrement())
	{
	std::stringstream strstr;
	strstr << count();

	LoggingProxy::AddData("repeatCount", strstr.str().c_str() );

	logger.log();
	}
};

template <class ALogger>     
void Logging::RepeatGuardLogger<ALogger>::logAndIncrement(Logging::Logger::LoggerSmartPtr &logger, 
				    Logging::BaseLog::Priority priority,
				    const std::string &message,
				    const std::string &file,
				    unsigned long line,
				    const std::string &method)
{
    if(checkAndIncrement())
	{
	std::stringstream strstr;
	strstr << count();

	LoggingProxy::AddData("repeatCount", strstr.str().c_str() );

	logger->log(priority, message,
		    file, line, method);
	}
}

    
template <class ALogger>     
void Logging::RepeatGuardLogger<ALogger>::logAndIncrement(Logging::Logger::LoggerSmartPtr &logger, 
				    const Logging::BaseLog::LogRecord &lr)
{
    if(checkAndIncrement())
	{
	std::stringstream strstr;
	strstr << count();

	LoggingProxy::AddData("repeatCount", strstr.str().c_str() );

	logger->log(lr);
	}
}


#endif /*!REPEATGUARDLOGGER_H*/
