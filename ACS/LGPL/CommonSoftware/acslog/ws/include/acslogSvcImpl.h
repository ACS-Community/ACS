#ifndef _ACS_LOG_SVC_IMPL_H_
#define _ACS_LOG_SVC_IMPL_H_
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
* "@(#) $Id: acslogSvcImpl.h,v 1.19 2010/03/26 23:28:15 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2001-09-11 created 
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscommonC.h>

#include "acslogS.h"
#include "logging.h"
#include <loggingBaseLog.h>

#include <acsutilPorts.h>

typedef unsigned long PriorityFlag;

class ACSLogImpl : public POA_ACSLog::LogSvc {
public:
  ACSLogImpl(LoggingProxy &logProxy) : m_logProxy(logProxy){}
 
  /*
  * @throw ACSErr::ACSException
  */
  void logTrace (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logDelouse (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logDebug (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logInfo (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logNotice (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logWarning (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logError (const ACSErr::ErrorTrace &c
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logErrorWithPriority (const ACSErr::ErrorTrace &c,
			    ACSLog::Priorities p
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logWithPriority (ACSLog::Priorities p,
       			      acscommon::TimeStamp time,
                              const char * msg,
                              const ACSLog::RTContext & rtCont,
                              const ACSLog::SourceInfo & srcInfo,
                              const ACSLog::NVPairSeq & data,
                              const char * audience=NULL,
                              const char * array=NULL,
                              const char * antenna=NULL
		 );
 
  /*
  * @throw ACSErr::ACSException
  */
  void logWithAudience (ACSLog::Priorities p,
       			      acscommon::TimeStamp time,
                              const char * msg,
                              const ACSLog::RTContext & rtCont,
                              const ACSLog::SourceInfo & srcInfo,
                              const char * audience=NULL,
                              const char * array=NULL,
                              const char * antenna=NULL
		 );
 
  /*
  * @throw ACSErr::ACSException
  */
  void logCritical (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logAlert (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 );

  /*
  * @throw ACSErr::ACSException
  */
  void logEmergency (acscommon::TimeStamp time,
		 const char * msg,
		 const ACSLog::RTContext & rtCont,
		 const ACSLog::SourceInfo & srcInfo,
		 const ACSLog::NVPairSeq & data
		 );


  /*
  * @throw ACSErr::ACSException
  */
  void logXML (const char * xml
		 );
    
  /*
  * @throw ACSErr::ACSException
  */
  void logXMLTimed (acscommon::TimeStamp time,
		    const char * xml
		 );
private:
  bool checkRTContext (const ACSLog::RTContext & rtCont);
  bool checkSourceInfo (const ACSLog::SourceInfo & srcInfo);
  PriorityFlag writeRTContext (const ACSLog::RTContext & rtCont);
  PriorityFlag writeSourceInfo (const ACSLog::SourceInfo & srcInfo);
  unsigned int writeData (const ACSLog::NVPairSeq & data);
  PriorityFlag write(const ACSLog::RTContext & rtCont,
		    const ACSLog::SourceInfo & srcInfo,
		    const ACSLog::NVPairSeq & data);
  LoggingProxy &m_logProxy;
};



#endif





