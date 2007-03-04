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
* "@(#) $Id: loggingLogSvcHandler.cpp,v 1.21 2007/03/04 17:40:31 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created 
*/

#include "loggingLogSvcHandler.h"
#include <loggingLoggingProxy.h>
#ifdef MAKE_VXWORKS
#include <acsutil.h>
#endif

#include <ace/Log_Record.h>

static char *rcsId="@(#) $Id: loggingLogSvcHandler.cpp,v 1.21 2007/03/04 17:40:31 msekoran Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);




namespace Logging {
    // ----------------------------------------------------------
    //helper function used to convert from ACS logging priorities
    //to ACE log priorities
    ACE_Log_Priority
    acs2acePriority(Logging::BaseLog::Priority acsPriority)
    {
	ACE_Log_Priority retVal = LM_TRACE;
	
	switch(acsPriority)
	    {
	    case Logging::BaseLog::LM_TRACE:
		retVal = LM_TRACE;
		break;
		
	    case Logging::BaseLog::LM_DEBUG:
		retVal = LM_DEBUG;
		break;
		
	    case Logging::BaseLog::LM_INFO:
		retVal = LM_INFO;
		break;
		
	    case Logging::BaseLog::LM_NOTICE:
		retVal = LM_NOTICE;
		break;
		
	    case Logging::BaseLog::LM_WARNING:
		retVal = LM_WARNING;
		break;
		
	    case Logging::BaseLog::LM_ERROR:
		retVal = LM_ERROR;
		break;
		
	    case Logging::BaseLog::LM_CRITICAL:
		retVal = LM_CRITICAL;
		break;
		
	    case Logging::BaseLog::LM_ALERT:
		retVal = LM_ALERT;
		break;
		
	    case Logging::BaseLog::LM_EMERGENCY:
		retVal = LM_EMERGENCY;
		break;
		
	    default:
		//should NEVER be the case but 
		//if this does occur we default
		//to a level that will always be
		//logged
		retVal = LM_EMERGENCY;
	    }
	
	return retVal;
    }

    // ----------------------------------------------------------
    //helper function used to convert from ACE logging priorities
    //to ACS log priorities
    Logging::BaseLog::Priority
    ace2acsPriority(ACE_Log_Priority acePriority)
    {
	Logging::BaseLog::Priority retVal = Logging::BaseLog::LM_TRACE;
	
	switch(acePriority)
	    {
	    case LM_TRACE:
		retVal = Logging::BaseLog::LM_TRACE;
		break;
		
	    case LM_DEBUG:
		retVal = Logging::BaseLog::LM_DEBUG;
		break;
		
	    case LM_INFO:
		retVal = Logging::BaseLog::LM_INFO;
		break;
		
	    case LM_NOTICE:
		retVal = Logging::BaseLog::LM_NOTICE;
		break;
		
	    case LM_WARNING:
		retVal = Logging::BaseLog::LM_WARNING;
		break;
	    
	    case LM_ERROR:
		retVal = Logging::BaseLog::LM_ERROR;
		break;
		
	    case LM_CRITICAL:
		retVal = Logging::BaseLog::LM_CRITICAL;
		break;
		
	    case LM_ALERT:
		retVal = Logging::BaseLog::LM_ALERT;
		break;
		
	    case LM_EMERGENCY:
		retVal = Logging::BaseLog::LM_EMERGENCY;
		break;
		
	    default:
		//should NEVER be the case but 
		//if this does occur we default
		//to a level that will always be
		//logged
		retVal = Logging::BaseLog::LM_EMERGENCY;
	    }
	
	return retVal;
    }
    // ----------------------------------------------------------
    LogSvcHandler::LogSvcHandler(const std::string& soName) :
	sourceObjectName_m(soName), localPriority_m(LM_TRACE), remotePriority_m(LM_TRACE)
    {
	//if we ever do away with the LoggingProxy class, a more
	//intelligent mechanism needs to be used here (i.e., read
	//$ACS_LOG_STDOUT).
	setLevel(LM_TRACE);
    }
    // ----------------------------------------------------------
    void
    LogSvcHandler::log(const LogRecord& lr)
    {
	//may need to make some changes to the message if the CORBA
	//logging service is down
	std::string message = lr.message;

	//if it's a trace we set the message to be "".
	if((lr.priority==LM_TRACE) &&
	   (message==FIELD_UNAVAILABLE))
	    {
	    message = "";
	    }
	
	//add a newline if the logging service is unavailable
	if(LoggingProxy::isInit()==false)
	    {
	    message = message + '\n';
	    }

	LoggingProxy::File(lr.file.c_str());
	LoggingProxy::Line(lr.line);

	//only set the logging flags if the priority is debug or trace
	if((lr.priority==LM_DEBUG)||(lr.priority==LM_TRACE))
	    {
	    LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT);
	    }

	//if the field is available
	if(lr.method!=FIELD_UNAVAILABLE)
	    {
	    //set the routine name.
	    LoggingProxy::Routine(lr.method.c_str());
	    }
        else
            {
	    LoggingProxy::Routine("");
            }

	//set the component/container/etc name
	LoggingProxy::SourceObject(sourceObjectName_m.c_str());
	
	//figure out the time
	long sec_ =  ACE_static_cast(CORBA::ULongLong, lr.timeStamp) / ACE_static_cast(ACE_UINT32, 10000000u) - ACE_UINT64_LITERAL(0x2D8539C80); 
        long usec_ = (lr.timeStamp % 10000000u) / 10; 
        ACE_Time_Value time(sec_, usec_); 

	//create the ACE log message
	/**
         * @todo Here there was an assignment to
	 * a local variable never used.
	 * Why was that? 
         * I have now commented it out
	 * int __ace_error = ACE_Log_Msg::last_error_adapter();
	 */
	ACE_Log_Msg::last_error_adapter();

	ACE_Log_Msg *ace___ = ACE_Log_Msg::instance();
	
	//create the ACE log record using the message
	ACE_Log_Record log_record_(acs2acePriority(lr.priority), time, 0);
	log_record_.msg_data(message.c_str());
	
	// set private flags
	const int prohibitLocal  = lr.priority <  localPriority_m ? 1 : 0;
	const int prohibitRemote = lr.priority < remotePriority_m ? 2 : 0;
	LoggingProxy::PrivateFlags(prohibitLocal || prohibitRemote);
	
	ace___->log(log_record_, 0);
    }
    // ----------------------------------------------------------
    std::string
    LogSvcHandler::getName() const
    {
	return std::string("acsLogSvc");
    }
    // ----------------------------------------------------------
    void
	LogSvcHandler::setLevels(Priority localPriority, Priority remotePriority)
	{
		localPriority_m = localPriority;
		remotePriority_m = remotePriority;
		// set global level (min of both)
		setLevel(localPriority_m < remotePriority_m ? localPriority_m : remotePriority_m);  
	}
    // ----------------------------------------------------------
    //--The following section exists solely to remain
    //--backwards compatible with the ACS 4.0 and earlier
    //--logging systems. This is provided so that formatted
    //--(i.e., printf, scanf, etc) messages are supported.
    //--At some point in time this should
    //--be removed!
    //-----------------------------------------------------------
    LogSvcHandler::DeprecatedLogInfo
    LogSvcHandler::unformatted2formatted(ACE_Log_Priority messagePriority,
					 const char *fmt,
					 ...)
    {
	//tells the compiler to check the format
	//every time it is called for format string and arguments matching in number
	//DWF-for some reason this does not compile
	//__attribute__ ((format (printf, 2, 3)));

	LogSvcHandler::DeprecatedLogInfo retVal;
	retVal.priority = ace2acsPriority(messagePriority);
	
	//take the format string along with the parameters
	//and convert them into the formatted string.
	va_list argp;
	va_start(argp, fmt);

	//temporary storage to hold the entire formatted
	//message.
	char tempMessage[MAX_MESSAGE_SIZE];

	vsnprintf(tempMessage, sizeof(tempMessage), fmt, argp);

	va_end(argp);
	
	//copy the data
	retVal.message = tempMessage;
	
	return retVal;
    }  
};

/*___oOo___*/
