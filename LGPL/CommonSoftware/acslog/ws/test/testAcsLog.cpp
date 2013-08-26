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
* "@(#) $Id: testAcsLog.cpp,v 1.21 2012/01/24 01:00:04 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  12/09/01  created
*/

// Uncomment this if you are using the VLT environment
// #include "vltPort.h"

static char *rcsId="@(#) $Id: testAcsLog.cpp,v 1.21 2012/01/24 01:00:04 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <stdlib.h>
#include <stdio.h>
#include <ace/SString.h>
#include <acsutilPorts.h>
#include <acserr.h>

#include "acslogS.h"
#include "ESTestC.h"

int main(int argc, char *argv[])
{
  CORBA::ORB_var orb;
  ACSLog::LogSvc_var log;

  int  nargc=0;
  char **nargv=0;
  
  ACE_CString argStr;
  for (int i=argc-1; i>=0; i--)
	argStr = ACE_CString(argv[i])+ " " + argStr;
  if (argStr.find ("-ORBInitRef ACSLogSvc=")==ACE_CString::npos)
      {
      const char *hn = ACSPorts::getIP();
      std::string port = ACSPorts::getLogPort();
	  
      argStr = argStr + "-ORBInitRef ACSLogSvc=iiop://"+hn+":"+port.c_str()+"/ACSLogSvc";
      }//if

  ACE_OS::printf ("Cmd Line: %s\n", argStr.c_str());

  ACE_OS::string_to_argv ((ACE_TCHAR*)argStr.c_str(), nargc, nargv);

  if (nargc<3){
    ACE_OS::printf ("usage: acslogTest <error Server name> [-ORBInitRef ACSLogSvc=iiop://HOST:port/ACSLogSvc]\n"); 
    return -1;
  }
  

  try
    {
      orb = CORBA::ORB_init (nargc, nargv, 0);
  
      ACE_OS::printf ("Resolve AcsLogSvc reference!\n");
      CORBA::Object_var obj = orb->resolve_initial_references ("ACSLogSvc");
  
      ACE_OS::printf ("Connected to object !\n");
      log = ACSLog::LogSvc::_narrow(obj.in());
    }
  catch( CORBA::Exception &ex )
    {    
      ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
    }

  try
    {
      ACE_OS::printf ("\nGetting error server test object reference ... \n");
      char fileName[64];
      sprintf(fileName, "file://%s.ior", argv[1]);
      CORBA::Object_var testObj = orb->string_to_object (fileName);

      ACE_OS::printf ( "Narrowing it .... \n");
      ESTest_var test = ESTest::_narrow (testObj.in());

      ACE_OS::printf ("Invoking remote call on error server to get error stack ...\n");
      ACSErr::CompletionImpl c = test->test (2, 1/*true*/);

      ACE_OS::printf ("Logging error stack using ACS Log Svc ... \n");
      log->logError(c.getErrorTraceHelper()->getErrorTrace());
      
      ACE_OS::printf ("Logging error stack using ACS Log Svc with different priorities \n");
      log->logErrorWithPriority(c.getErrorTraceHelper()->getErrorTrace(), ACSLog::ACS_LOG_DEBUG);

      log->logErrorWithPriority(c.getErrorTraceHelper()->getErrorTrace(), ACSLog::ACS_LOG_INFO);
    }
  catch( CORBA::Exception &ex )
    {    
      ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
    }


 try
   {

     ACSLog::RTContext con;
     ACSLog::SourceInfo si;
     ACSLog::NVPairSeq data;

      ACE_Time_Value tv = ACE_OS::gettimeofday();
      acscommon::TimeStamp ts = (ACE_UINT64_LITERAL(0x2D8539C80) + ACE_static_cast(CORBA::ULongLong, tv.sec())) *ACE_static_cast(ACE_UINT32, 10000000) + ACE_static_cast(CORBA::ULongLong, tv.usec() * 10);
      ACE_OS::printf("Test #1\n");      

      /*
       * GCH - 2009.07.28
       * It is ESSENTIAL to initialize si.line, the only non-string
       * in the structure.
       * If you do not do it, then you get garbage, because corba structures
       * are not initialized.
       */
      si.line = __LINE__+1;

      log->logTrace(ts, "test msg #0 (there should not be Context and Source info)", con, si, data);
      

      ACE_OS::printf("Test #2\n");
      si.routine = CORBA::string_dup("main");
      si.file = CORBA::string_dup("testAcsLog.cpp");
      si.line = __LINE__+1;
      log->logTrace(ts, "test msg #1 (there should be Source info and not Context info)", con, si, data);
      

      ACE_OS::printf("Test #3\n");
      con.process = CORBA::string_dup(argv[0]);
      con.thread = CORBA::string_dup("THREAD_ID");
      con.host = CORBA::string_dup("HOST"); 
      con.sourceObject = CORBA::string_dup("SOURCE_OBJECT");
      si.line = __LINE__+1;
      log->logTrace(ts, "test msg #2 (there should be Context and Source info)", con, si, data);
      

      ACE_OS::printf("Test #4\n");
      ACSLog::NVPair nv;
      nv.name = CORBA::string_dup ("Name #1");
      nv.value = CORBA::string_dup ("Value #1"); 
      data.length(1);
      data[0] = nv; 
      si.line = __LINE__+1;
      log->logTrace(ts, "test msg #3 (there should be Context and  Source info and Data)", con, si, data);
      
      ACE_OS::printf("Test #5 (Delouse)\n");
      si.line = __LINE__+1;
      log->logDelouse(ts, "test Delouse msg (there should be Context and  Source info and Data)", con, si, data);

      ACE_OS::printf("Test #5 (Debug)\n");
      si.line = __LINE__+1;
      log->logDebug(ts, "test Debug msg (there should be Context and  Source info and Data)", con, si, data);
      

      ACE_OS::printf("Test #6 (Info)\n");
      si.line = __LINE__+1;
      log->logInfo(ts, "test Info msg (there should be Context and  Source info and Data)", con, si, data);
      

      ACE_OS::printf("Test #7 (Notice)\n");
      si.line = __LINE__+1;
      log->logNotice(ts, "test Notice msg (there should be Context and  Source info and Data)", con, si, data);
      

      ACE_OS::printf("Test #8 (Warning)\n");
      si.line = __LINE__+1;
      log->logWarning(ts, "test Warning msg (there should be Context and  Source info and Data)", con, si, data);
      

      ACE_OS::printf("Test #9 (Critical)\n");
      si.line = __LINE__+1;
      log->logCritical(ts, "test Critical msg (there should be Context and  Source info and Data)", con, si, data);
      

      ACE_OS::printf("Test #10 (Alert)\n");
      si.line = __LINE__+1;
      log->logAlert(ts, "test Alert msg (there should be Context and  Source info and Data)", con, si, data);
      
      
      ACE_OS::printf("Test #11 (Emergency)\n");
      si.line = __LINE__+1;
      log->logEmergency(ts, "test Emergency msg (there should be Context and  Source info and Data)", con, si, data);
      
      ACE_OS::printf("logWithAudience Test #12 (Trace)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_TRACE, ts, "test logWithAudience Trace msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #13 (Debug)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_DEBUG, ts, "test logWithAudience Debug msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #14 (Info)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_INFO, ts, "test logWithAudience Info msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #15 (Notice)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_NOTICE, ts, "test logWithAudience Notice msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #16 (Warning)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_WARNING, ts, "test logWithAudience Warning msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #17 (Error)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_ERROR, ts, "test logWithAudience Error msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #18 (Critical)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_CRITICAL, ts, "test logWithAudience Critical msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #19 (Alert)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_ALERT, ts, "test logWithAudience Alert msg (there should be Context and  Source info)", con, si, "", "", "");
      
      ACE_OS::printf("logWithAudience Test #20 (Emergency)\n");
      si.line = __LINE__+1;
      log->logWithAudience(ACSLog::ACS_LOG_EMERGENCY, ts, "test logWithAudience Emergency msg (there should be Context and  Source info)", con, si, "", "", "");
      

    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "Exception cought");
      //    return -1;
    }
 
  return 0;
}












