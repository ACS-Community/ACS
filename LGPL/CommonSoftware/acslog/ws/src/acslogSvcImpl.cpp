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
* "@(#) $Id: acslogSvcImpl.cpp,v 1.14 2006/01/09 18:27:56 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  11/09/01  created 
*/

static char *rcsId="@(#) $Id: acslogSvcImpl.cpp,v 1.14 2006/01/09 18:27:56 dfugate Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acslogSvcImpl.h"
#include "acserr.h"
    
void ACSLogImpl::logTrace (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_TRACE, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logDebug (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_DEBUG, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}    

void ACSLogImpl::logInfo (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_INFO, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logNotice (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_NOTICE, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logWarning (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_WARNING, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logError (const ACSErr::ErrorTrace & et) 
    throw ( CORBA::SystemException, ACSErr::ACSException )
{
  ErrorTraceHelper err(const_cast<ACSErr::ErrorTrace &>(et));
  err.log();
}

void ACSLogImpl::logCritical (acscommon::TimeStamp time,
			      const char * msg,
			      const ACSLog::RTContext & rtCont,
			      const ACSLog::SourceInfo & srcInfo,
			      const ACSLog::NVPairSeq & data) 
    throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_CRITICAL, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logAlert (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_ALERT, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logEmergency (acscommon::TimeStamp time,
			   const char * msg,
			   const ACSLog::RTContext & rtCont,
			   const ACSLog::SourceInfo & srcInfo,
			   const ACSLog::NVPairSeq & data
			   
			   ) throw ( CORBA::SystemException, ACSErr::ACSException )
{  
  PriortyFlag  flag = write (rtCont, srcInfo, data);
  ACS_CHECK_LOGGER;
  LoggingProxy::Flags(flag);
  LOG_RECORD(Logging::BaseLog::LM_EMERGENCY, msg, srcInfo.file.in(), srcInfo.line, srcInfo.routine.in(), time);
}

void ACSLogImpl::logXML (const char * xml
			 
			 ) throw (CORBA::SystemException, ACSErr::ACSException )
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

PriortyFlag ACSLogImpl::writeRTContext(const ACSLog::RTContext & rtCont)
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


PriortyFlag ACSLogImpl::writeSourceInfo(const ACSLog::SourceInfo & srcInfo)
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

PriortyFlag ACSLogImpl::write(const ACSLog::RTContext & rtCont,
		    const ACSLog::SourceInfo & srcInfo,
		    const ACSLog::NVPairSeq & data)
{
  PriortyFlag flag=0;
 
  flag |= writeRTContext (rtCont);
  flag |= writeSourceInfo (srcInfo);
 
  writeData (data);

  return flag;
}



