/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acslogSvcImpl.cpp,v 1.24 2010/03/30 21:49:27 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  11/09/01  created 
*/

static char *rcsId="@(#) $Id: acslogSvcImpl.cpp,v 1.24 2010/03/30 21:49:27 javarias Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acslogSvcImpl.h"
#include "acserr.h"

//This is to avoid conflict with the definition on LM_DELOUSE macro
#ifdef LM_DELOUSE
#undef LM_DELOUSE
#endif

// ----------------------------------------------------------
//helper function used to convert from ACSLog logging priorities
//to Logging log priorities
Logging::BaseLog::Priority acslog2loggingPriority(ACSLog::Priorities acslogPriority)
{
    Logging::BaseLog::Priority retVal = Logging::BaseLog::LM_TRACE;
	
    switch(acslogPriority)
	{
	case ACSLog::ACS_LOG_TRACE:
	    retVal = Logging::BaseLog::LM_TRACE;
	    break;
		
	case ACSLog::ACS_LOG_DELOUSE:
	    retVal = Logging::BaseLog::LM_DELOUSE;
	    break;
		
	case ACSLog::ACS_LOG_DEBUG:
	    retVal = Logging::BaseLog::LM_DEBUG;
	    break;
		
	case ACSLog::ACS_LOG_INFO:
	    retVal = Logging::BaseLog::LM_INFO;
	    break;
		
	case ACSLog::ACS_LOG_NOTICE:
	    retVal = Logging::BaseLog::LM_NOTICE;
	    break;
		
	case ACSLog::ACS_LOG_WARNING:
	    retVal = Logging::BaseLog::LM_WARNING;
	    break;
	    
	case ACSLog::ACS_LOG_ERROR:
	    retVal = Logging::BaseLog::LM_ERROR;
	    break;
		
	case ACSLog::ACS_LOG_CRITICAL:
	    retVal = Logging::BaseLog::LM_CRITICAL;
	    break;
		
	case ACSLog::ACS_LOG_ALERT:
	    retVal = Logging::BaseLog::LM_ALERT;
	    break;
		
	case ACSLog::ACS_LOG_EMERGENCY:
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

    
void ACSLogImpl::logTrace (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
            )			   
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_TRACE, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logDelouse (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
            )			   
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_DELOUSE, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}    

void ACSLogImpl::logDebug (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
            )			   
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_DEBUG, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}    

void ACSLogImpl::logInfo (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			  ) 
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_INFO, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logNotice (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			  ) 
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_NOTICE, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logWarning (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			  ) 
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_WARNING, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logError (const ACSErr::ErrorTrace & et) 
{
  ErrorTraceHelper err(const_cast<ACSErr::ErrorTrace &>(et));
  err.log();
}

void ACSLogImpl::logErrorWithPriority(const ACSErr::ErrorTrace &et, ACSLog::Priorities p) 
{
  ErrorTraceHelper err(const_cast<ACSErr::ErrorTrace &>(et));
  if(p == ACSLog::ACS_LOG_TRACE)
      err.log(ACE_Log_Priority(1 << 1));
  else if (p == ACSLog::ACS_LOG_DELOUSE)
      err.log(ACE_Log_Priority(010000));
  err.log(ACE_Log_Priority(1 << (p)));   // here we assume that enums in IDL starts from 0 
}//logErrorWithPriorty

void ACSLogImpl::logWithPriority (ACSLog::Priorities p,
			      acscommon::TimeStamp time,
			      const char * msg,
			      const ACSLog::RTContext & rtCont,
			      const ACSLog::SourceInfo & srcInfo,
			      const ACSLog::NVPairSeq & data,
			      const char * audience,
                              const char * array,
                              const char * antenna) 
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  if(audience!=NULL)LoggingProxy::audience(audience);
  if(array!=NULL)LoggingProxy::array(array);
  if(antenna!=NULL)LoggingProxy::antenna(antenna);
  LOG_RECORD(acslog2loggingPriority(p), msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}
void ACSLogImpl::logWithAudience (ACSLog::Priorities p,
			      acscommon::TimeStamp time,
			      const char * msg,
			      const ACSLog::RTContext & rtCont,
			      const ACSLog::SourceInfo & srcInfo,
			      const char * audience,
                              const char * array,
                              const char * antenna) 
{  
  PriorityFlag  flag = writeRTContext(rtCont)|writeSourceInfo(srcInfo);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  if(audience!=NULL)LoggingProxy::audience(audience);
  if(array!=NULL)LoggingProxy::array(array);
  if(antenna!=NULL)LoggingProxy::antenna(antenna);
  LOG_RECORD(acslog2loggingPriority(p), msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logCritical (acscommon::TimeStamp time,
			      const char * msg,
			      const ACSLog::RTContext & rtCont,
			      const ACSLog::SourceInfo & srcInfo,
			      const ACSLog::NVPairSeq & data) 
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_CRITICAL, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logAlert (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
)
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_ALERT, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logEmergency (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			  ) 
{  
  PriorityFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_EMERGENCY, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time, rtCont.sourceObject.in());
}

void ACSLogImpl::logXML (const char * xml)			 
{
  // first check if xml is XML string and if is DTD  (will be done in logXML !!!!!)
  m_logProxy.logXML (xml);
}

/*  void ACSLogImpl::logXMLTimed (acscommon::TimeStamp time,
			      const char * xml,
			      ) throw (CORBA::SystemException, ACSErr::ACSException )
{}
*/


bool ACSLogImpl::checkRTContext(const ACSLog::RTContext & rtCont)
{
  return (rtCont.thread.in()[0]!=0 ||
	  rtCont.process.in()[0]!=0 ||
	  rtCont.host.in()[0]!=0 ||
	  rtCont.addContext.in()[0]!=0 ||
          rtCont.sourceObject.in()[0]!=0);
}

bool ACSLogImpl::checkSourceInfo(const ACSLog::SourceInfo & srcInfo)
{
  /* ACE_OS::printf("file %s %d\n", srcInfo.file.in(), srcInfo.file.in()[0]!=0);
  ACE_OS::printf("routine %s %d\n", srcInfo.routine.in(), srcInfo.routine.in()[0]!=0);
  ACE_OS::printf("line %d %d\n", srcInfo.line, srcInfo.line>0);
  */
  return (srcInfo.file.in()[0]!=0 ||
          srcInfo.routine.in()[0]!=0);
}

PriorityFlag ACSLogImpl::writeRTContext(const ACSLog::RTContext & rtCont)
{
  if (checkRTContext (rtCont)){
    //ACE_OS::printf("context\n");
    ACE_Log_Msg::instance()->local_host(rtCont.host.in()); 
    LoggingProxy::ProcessName (rtCont.process.in());
    LoggingProxy::ThreadName (rtCont.thread.in());
    LoggingProxy::SourceObject (rtCont.sourceObject.in());
    return LM_RUNTIME_CONTEXT;
  }
  return 0;
}


PriorityFlag ACSLogImpl::writeSourceInfo(const ACSLog::SourceInfo & srcInfo)
{
 if (checkSourceInfo(srcInfo)){
    //    ACE_OS::printf("sourceInfo\n");
    ACE_Log_Msg::instance()->file (srcInfo.file.in()); 
    ACE_Log_Msg::instance()->linenum (srcInfo.line);
    LoggingProxy::Routine (srcInfo.routine.in()); 
    return LM_SOURCE_INFO;
  } 
 return 0;
}

unsigned int ACSLogImpl::writeData (const ACSLog::NVPairSeq & data){
  unsigned int j;
  for (j=0; j<data.length(); j++)  
    LoggingProxy::AddData (data[j].name.in(), data[j].value.in());
  return j-1;
}

PriorityFlag ACSLogImpl::write(const ACSLog::RTContext & rtCont,
		    const ACSLog::SourceInfo & srcInfo,
		    const ACSLog::NVPairSeq & data)
{
  PriorityFlag flag=0;
 
  flag |= writeRTContext (rtCont);
  flag |= writeSourceInfo (srcInfo);
 
  writeData (data);

  return flag;
}



