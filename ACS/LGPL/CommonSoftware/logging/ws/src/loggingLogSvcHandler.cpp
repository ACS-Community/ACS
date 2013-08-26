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
* "@(#) $Id: loggingLogSvcHandler.cpp,v 1.33 2010/04/30 10:26:48 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate  2005-04-04  created 
*/

#include "loggingLogSvcHandler.h"
#include <loggingLoggingProxy.h>
#include <loggingLogLevelDefinition.h>
#ifdef MAKE_VXWORKS
#include <acsutil.h>
#endif

#include <ace/Log_Record.h>

static char *rcsId="@(#) $Id: loggingLogSvcHandler.cpp,v 1.33 2010/04/30 10:26:48 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


namespace Logging {
    // ----------------------------------------------------------
    //helper function used to convert from ACS logging priorities
    //to ACE log priorities
    ACE_Log_Priority
    acs2acePriority(Logging::BaseLog::Priority acsPriority)
    {
	ACE_Log_Priority retVal = LM_SHUTDOWN;
	
	switch(acsPriority)
	    {
	    case Logging::BaseLog::LM_TRACE:
		retVal = LM_TRACE;
		break;
		
        case Logging::BaseLog::LM_DELOUSE:
        retVal = (ACE_Log_Priority)010000;
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
	    
            case Logging::BaseLog::LM_SHUTDOWN:
		retVal = LM_SHUTDOWN;
		break;
		
	    default:
		//should NEVER be the case but 
		//if this does occur we default
		//to a level that will always be
		//logged
		retVal = LM_SHUTDOWN;
	    }
	
	return retVal;
    }

    // ----------------------------------------------------------
    //helper function used to convert from ACE logging priorities
    //to ACS log priorities
    Logging::BaseLog::Priority
    ace2acsPriority(ACE_Log_Priority acePriority)
    {
	Logging::BaseLog::Priority retVal = Logging::BaseLog::LM_SHUTDOWN;
	
	switch(acePriority)
	    {
	    case LM_TRACE:
		retVal = Logging::BaseLog::LM_TRACE;
		break;

        case 010000:
        retVal = Logging::BaseLog::LM_DELOUSE;
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
	    
            case LM_SHUTDOWN:
		retVal = Logging::BaseLog::LM_SHUTDOWN;
		break;
		
	    default:
		//should NEVER be the case but 
		//if this does occur we default
		//to a level that will always be
		//logged
		retVal = Logging::BaseLog::LM_SHUTDOWN;
	    }
	
	return retVal;
    }
    // ----------------------------------------------------------
    LogSvcHandler::LogSvcHandler(const std::string& soName) :
	sourceObjectName_m(soName)
    {
        setRemoteLevelType(NOT_DEFINED_LOG_LEVEL);
        setLocalLevelType(NOT_DEFINED_LOG_LEVEL);
	char *acsSTDIO = getenv("ACS_LOG_STDOUT");
	int envStdioPriority = -1;
	if (acsSTDIO && *acsSTDIO)
	{
		envStdioPriority = atoi(acsSTDIO);
	}

	char *acsCentralizeLogger = getenv("ACS_LOG_CENTRAL");
	int envCentralizePriority = -1;
	if (acsCentralizeLogger && *acsCentralizeLogger)
	{
		envCentralizePriority = atoi(acsCentralizeLogger);
	}
	setLevels(LM_TRACE, LM_TRACE,DEFAULT_LOG_LEVEL);
	if(envStdioPriority >= 0)
		setLevels((Priority)-1, static_cast<Logging::BaseLog::Priority>(LogLevelDefinition::getACELogPriority(envStdioPriority)), ENV_LOG_LEVEL);
	
	if(envCentralizePriority >= 0)
		setLevels(static_cast<Logging::BaseLog::Priority>(LogLevelDefinition::getACELogPriority(envCentralizePriority)), (Priority)-1, ENV_LOG_LEVEL);
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

 	if(lr.priority == LM_SHUTDOWN)
	   message = " -- ERROR in the priority of this message, please check the source --" + message;
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
 	const int prohibitLocal  = getLocalLevel() == LM_SHUTDOWN || lr.priority <  getLocalLevel() ? 1 : 0;
 	const int prohibitRemote = getRemoteLevel() == LM_SHUTDOWN ||lr.priority < getRemoteLevel() ? 2 : 0;
  	LoggingProxy::PrivateFlags(prohibitLocal | prohibitRemote);
  	LoggingProxy::LogLevelLocalType(getLocalLevelType());	
  	LoggingProxy::LogLevelRemoteType(getRemoteLevelType());
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
	LogSvcHandler::setLevels(Priority remotePriority, Priority localPriority, int type)
	{
	    if(remotePriority >0 && (type == CDB_REFRESH_LOG_LEVEL  || getRemoteLevelType() >= type) ){
		    setRemoteLevelType(type);
		    setRemoteLevel(remotePriority);
	    }
	    if(localPriority > 0 && ( type == CDB_REFRESH_LOG_LEVEL || getLocalLevelType() >= type )){
		    setLocalLevelType(type);
		    setLocalLevel(localPriority);
	    }
	    // set global level (min of both)
	    setLevel(static_cast<Logging::BaseLog::Priority>(getLocalLevel() < getRemoteLevel() ? getLocalLevel() : getRemoteLevel()));  
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
