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
* "@(#) $Id: acserrLegacy.cpp,v 1.21 2012/01/20 22:07:43 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2003-05-20 Reverted back to anonymous sequence. Forward decl. does not work with TAO IR.
* gchiozzi 2003-05-15 Replaced anonymous sequence with ErrorLinkedList
* bjeram 2003-03-06 added ACSErr prfix for types defined in the idl files
* msekoran 2002-09-23 fixed ACSError::orb memory management
* bjeram 2002-09-16 cuuren =0  is added to constructors where is no error trace (NO erro)
* bjeram 2002-04-10 added ACSError (const char* file, int line, ACSErr::ACSCompletion* c, bool release) constructior which can be used with ACE_ERROR
* bjeram 2002-04-10 added if_no_error check (ACSError)
* bjeram 2002-04-10 added if_no_error check (ACSCompletion)
* bjeram 2002-01-23 removed initLog from init and LoggingProxy::done out of done
* bjeram 2002-01-21 getpid is converted into unsgined long
* bjeram 2001-11-21 Removed dependency from maci.
* bjeram 2001-11-14 added init ....
* bjeram 2001-11-14 added reading error description from IDL files
* bjeram 2001-10-18 log also severity
* bjeram 2001-09-19 added new constructor ACSError (const ACSErr::ACSCompletion& completion)
* bjeram 2001-09-18 added generating of StackId
* almamgr  20/06/01  created 
*/

#include "acserrLegacy.h"
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include <time.h>
#include <stdio.h>
#include <acsutilFindFile.h>
#include <acsutilTimeStamp.h>
#include <fstream>
#include <cstring>
#include "acserrOldDescriptions.h"
#include "acserrACSbaseExImpl.h"
#include "acserrHandlers.h"
#include "ace/UUID.h"

static char *rcsId="@(#) $Id: acserrLegacy.cpp,v 1.21 2012/01/20 22:07:43 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);


// static
const ACSErr::ACSErrType ACSError::ACSErrTypeOK = 0;   // this constant is redefined here otherwise we depend on code generated from ACSErrTypeOK
const ACSErr::ErrorCode ACSError::ACSErrOK = 0; // 
bool ACSError::initialized = false;
CORBA::ORB_var ACSError::orb;
std::unexpected_handler  ACSError::m_oldUnexpected;
std::terminate_handler  ACSError::m_oldTerminate;

ACS::Time ACSError::getTime()
{
    return ::getTimeStamp();
}

ACSError::ACSError (const ACSErr::ErrorTrace& errortrace) :
 errorTrace(errortrace){
  current = & this->errorTrace;
  depth=1;
  while (current->previousError.length()!=0){
    depth ++;
    current = &current->previousError[0];
  }
  ACS_DEBUG ("ACSError::ACSError", "ACSError created from errortrace");
}

ACSError::ACSError (ACSErr::ErrorTrace& errortrace) :
 errorTrace(errortrace)
{
  current = & this->errorTrace;
  depth=1;
  while (current->previousError.length()!=0){
    depth ++;
    current = &current->previousError[0];
  }
  ACS_DEBUG ("ACSError::ACSError", "ACSError created from errortrace");
}

ACSError::ACSError (const char* file, int line, ACSErr::ErrorTrace* c, bool release) :
  errorTrace (*c)
{
    current = &errorTrace;  // calculating depth of stack
    depth=1;
    while (current->previousError.length()!=0)
	{
	depth ++;
	current = &current->previousError[0];
	}

    if (release) delete c;
    current = &errorTrace;
    ACS_DEBUG ("ACSError::ACSError", "ACSError created from errortrace (*)");
}

ACSError::ACSError (ACSErr::ErrorTrace *c, bool release) :
  errorTrace (*c) {
 
  current = &errorTrace;  // calculating depth of stack
  depth=1;
  while (current->previousError.length()!=0)
      {
      depth ++;
      current = &current->previousError[0];
      }

  if (release) delete c;
  current = &errorTrace;
  ACS_DEBUG ("ACSError::ACSError", "ACSError created from errortrace (*)");
}

ACSError::ACSError (ACSErr::ACSException& exception) :
  errorTrace (exception.error){
  
  current = &errorTrace;  // calculating depth of stack
  depth=1;
  while (current->previousError.length()!=0){
    depth ++;
    current = &current->previousError[0];
  }

  current = &errorTrace; // point to the beginign of tehe error stack
 ACS_DEBUG ("ACSError::ACSError", "ACSError created from ACSException ");
}

ACSError::ACSError (){
    current = 0; //NULL

    errorTrace.timeStamp = getTime();

    errorTrace.lineNum = 0;

    errorTrace.severity = DEFAULT_SEVERITY;  

    errorTrace.errorCode = ACSErrOK; 
    errorTrace.errorType = ACSErrTypeOK;
    current = &errorTrace;

    depth = 1;
    ACS_DEBUG ("ACSError::ACSError", "ACSError created (no error) - w/o runtime & source info");
}

ACSError::ACSError (const char* file, int line){
    current = 0; //NULL

    errorTrace.timeStamp = getTime();
    errorTrace.lineNum = 0;
  
    errorTrace.severity = DEFAULT_SEVERITY;  

    errorTrace.errorCode = ACSErrOK; 
    errorTrace.errorType = ACSErrTypeOK;
    current = &errorTrace;

    depth = 1;
    ACS_DEBUG ("ACSError::ACSError", "ACSError created (no error) - w/o runtime & source info");
}

ACSError::ACSError (const char* file, int line, const char* routine){
    fill (ACSErrTypeOK, ACSErrOK, DEFAULT_SEVERITY, file, line, routine);
    depth = 1;
    ACS_DEBUG ("ACSError::ACSError", "ACSError created (no error)");
}

ACSError::ACSError (const char* file, int line, ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		    const char* routine, ACSErr::Severity severity){
    
    fill (et, ec, severity, file, line, routine);
    depth = 1;
    ACS_DEBUG ("ACSError::ACSError", "ACSError created");
}

ACSError::ACSError (const char* file, int line, ACSError &err, ACSErr::ACSErrType et, ACSErr::ErrorCode ec, const char *routine, ACSErr::Severity severity)
{   
    if (!err.isOK())
	{ 
	fill (et, ec, severity, file, line, routine);
	depth = err.depth + 1;

        /**
	 * @todo
	 * G.Chiozzi 203-05-20
	 * Should be like this, but it does not work with TAO IR
	 * errorTrace.previousError = ACSErr::ErrorLinkedList (1, &err.errorTrace, 0);
	 */
	errorTrace.previousError.length(1);
	errorTrace.previousError[0] = err.errorTrace;
//          ACSErr::ErrorTrace::_tao_seq_ACSErr_ErrorTrace__1 (1, &err.errorTrace, 0);
	}
    else  // if no previos error
	{
	ACS_DEBUG ("ACSError::ACSError", "previos ACSError does not contain error");
	fill  (ACSErrTypeOK, ACSErrOK,  DEFAULT_SEVERITY, file, line, routine); // ?file="", line=0, routine="" ???
	depth = 1;
	}

    ACS_DEBUG ("ACSError::ACSError", "ACSError created with previous ACSError&");
}

ACSError::ACSError (const char* file, int line, ACSError *err, ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
		    const char *routine, ACSErr::Severity severity, bool release)
{   
    if (!err->isOK())
	{ 
	fill (et, ec, severity, file, line, routine);
	depth = err->depth + 1;
        /**
	 * @todo
	 * G.Chiozzi 203-05-20
	 * Should be like this, but it does not work with TAO IR
	 * errorTrace.previousError = ACSErr::ErrorLinkedList (1, &err.errorTrace, 0);
	 */
	errorTrace.previousError.length(1);
	errorTrace.previousError[0] = err->errorTrace;
	    //ACSErr::ErrorTrace::_tao_seq_ACSErr_ErrorTrace__1 (1, &err->errorTrace, 0); //add previos error
	}
    else  // if no previos error
	{
	ACS_DEBUG ("ACSError::ACSError", "previos ACSError does not contain error");
	fill  (ACSErrTypeOK, ACSErrOK,  DEFAULT_SEVERITY, file, line, routine); // ?file="", line=0, routine="" ???
	depth = 1;
	}

    if (release) delete err;
    ACS_DEBUG ("ACSError::ACSError", "ACSError created with previous ACSError*");
}


ACSError::ACSError (const char* file, int line, ACSErr::ACSException &pex, ACSErr::ACSErrType et,
		    ACSErr::ErrorCode ec, const char *routine, ACSErr::Severity severity)
{
// previos exception can not be errorless !!!  
    current = &pex.error;
    depth=1;
    while (current->previousError.length()!=0)
	{
	depth ++;
	current = &current->previousError[0];
	}
    depth++;
    
    fill (et, ec, severity, file, line, routine);
    /**
     * @todo
     * G.Chiozzi 203-05-20
     * Should be like this, but it does not work with TAO IR
     * errorTrace.previousError = ACSErr::ErrorLinkedList (1, &err.errorTrace, 0);
     */
    errorTrace.previousError.length(1);
    errorTrace.previousError[0] = pex.error;
	// ACSErr::ErrorTrace::_tao_seq_ACSErr_ErrorTrace__1 (1,  &pex.error, 0); //add previos error
    
    ACS_DEBUG ("ACSError::ACSError", "ACSError created with previous ACSException");
}

ACSError::ACSError (const char* file, int line, ACSErr::ErrorTrace &c, ACSErr::ACSErrType et,
		    ACSErr::ErrorCode ec, const char *routine, ACSErr::Severity severity){
  
    if (c.errorCode!=ACSErrOK || c.errorType!=ACSErrTypeOK )
	{
	current = &c;
	depth=1;
	while (current->previousError.length()!=0){
	depth ++;
	current = &current->previousError[0];
	}
	depth++;

	fill (et, ec, severity, file, line, routine);
        /**
	 * @todo
	 * G.Chiozzi 203-05-20
	 * Should be like this, but it does not work with TAO IR
	 * errorTrace.previousError = ACSErr::ErrorLinkedList (1, &err.errorTrace, 0);
	 */
	errorTrace.previousError.length(1);
	errorTrace.previousError[0] = c;
//	    ACSErr::ErrorTrace::_tao_seq_ACSErr_ErrorTrace__1 (1,  &c, 0); //add previos error
	}
      else  // if no previos error
	{
	ACS_DEBUG ("ACSError::ACSError", "previos ErrorTrace does not contain error");
	// ... fill with no error information
	fill  (ACSErrTypeOK, ACSErrOK,  DEFAULT_SEVERITY, file, line, routine); // ?file="", line=0, routine="" ???
	depth = 1;
	}
  ACS_DEBUG ("ACSError::ACSError", "ACSError created with previous ErrorTrace");
}

void ACSError::addData (const char* name, const char* value){
  int length = current->data.length();
  ACSErr::NameValue nv;
  nv.name = CORBA::string_dup (name);
  nv.value = CORBA::string_dup (value); 
  current->data.length (length+1);
  current->data[length] = nv;
}

void ACSError::fill (ACSErr::ACSErrType et, ACSErr::ErrorCode ec, ACSErr::Severity severity,
		     const char* file, int line, const char* routine){
//  errorTrace.description = CORBA::string_dup (ed);
  errorTrace.timeStamp = getTime();
  errorTrace.file = CORBA::string_dup (file);
  errorTrace.lineNum = line;
  errorTrace.routine = CORBA::string_dup (routine);

  char tid[64];
  ACE_OS::sprintf( tid, "ID: %lu", (long)ACE_Thread_Manager::instance()->thr_self() );
  errorTrace.thread = CORBA::string_dup (tid);
  // setting process name
  if (ErrorTraceHelper::m_processName[0]==0){
    unsigned long pid = (unsigned long)ACE_OS::getpid();
    ACE_OS::sprintf (ErrorTraceHelper::m_processName, "PID: %lu", pid);
  }
  errorTrace.process = CORBA::string_dup (ErrorTraceHelper::m_processName);
  //setting host name
  if (ErrorTraceHelper::m_hostName[0]==0){
    ACE_OS::hostname (ErrorTraceHelper::m_hostName, 64);
  }
  errorTrace.host = CORBA::string_dup (ErrorTraceHelper::m_hostName);
  
  errorTrace.severity = severity;  

  errorTrace.errorCode = ec; 
  errorTrace.errorType = et;
  current = &errorTrace;
}

ACSError::~ACSError(){ 
  ACS_DEBUG ("ACSError::~ACSError", "ACSError object deleted");
}

ACSErr::ErrorTrace* ACSError::returnErrorTrace(bool deletion){
  ACSErr::ErrorTrace *tmp = new ACSErr::ErrorTrace(*current);
  if (deletion) delete this;
  return tmp;
}


ACSErr::ErrorTrace* ACSError::getNext(){ 
  if (current->previousError.length()!=0) {
    current = &current->previousError[0];
    return current; 
  }else{
    return NULL;
  }
}

char* ACSError::getDescription(){ 
    return getDescription (current->errorType, current->errorCode);
}

void ACSError::hostName (const char* hn){
  strncpy (ErrorTraceHelper::m_hostName, hn, 64);
}

void ACSError::processName (const char *pn)
{
    ACSError::setProcessName(pn);
}

void ACSError::setProcessName (const char *pn)
{
  strncpy (ErrorTraceHelper::m_processName, pn, 64);
}

ACSErr::ErrorTrace createErrorTrace (const char* file, int line, ACSError &er)
{ 
    er.setLineNumber(line);
    //?er.setFileName();
    
    return er.getErrorTrace(); 
}

ACSErr::ErrorTrace ACSError::createErrorTrace (const char* file, int line, 
						      ACSErr::ACSException &pex, ACSErr::ACSErrType et,
						      ACSErr::ErrorCode ec, const char *routine,
						      ACSErr::Severity severity)
{
    ACSError er(file, line, pex, et, ec, routine, severity);
    return er.getErrorTrace();
}

ACSErr::ErrorTrace ACSError::createErrorTrace (const char* file, int line, 
						      ACSErr::ACSErrType et, ACSErr::ErrorCode ec,\
						      const char *routine,
						      ACSErr::Severity severity)
{
    ACSError er(file, line, et, ec, routine, severity);
    return er.getErrorTrace();
} 


void ACSError::log (ACSErr::ErrorTrace * c, int level){
  unsigned int j;
 

  ACE_Time_Value tv = ACE_OS::gettimeofday();
  char timeBuf[25];
  sprintf(timeBuf, "%ld", tv.sec());
  LoggingProxy::StackId (timeBuf); // unique stack ID
  LoggingProxy::StackLevel (level);

  CORBA::ULong en;
  en = c->errorCode;
  LoggingProxy::AddData ("Error Number", "%u", en);  

  for (j=0; j<c->data.length(); j++)  
      LoggingProxy::AddData (c->data[j].name.in(), c->data[j].value.in());

  char *descript = getDescription (c->errorType, c->errorCode);

  if (c->errorType!=ACSErrTypeOK || c->errorCode!=ACSErrOK || c->lineNum!=0)
      {
      // set source info
      ACE_Log_Msg::instance()->file (c->file.in()); 
      ACE_Log_Msg::instance()->linenum (c->lineNum);
      LoggingProxy::Routine (c->routine.in()); 

      // set runtime context
      ACE_Log_Msg::instance()->local_host(c->host.in()); 
      LoggingProxy::ProcessName (c->process.in());
      LoggingProxy::ThreadName (c->thread.in());

      ACS_LOG_TIME (LM_SOURCE_INFO | LM_RUNTIME_CONTEXT, c->timeStamp, c->routine.in(), 
		(ACE_Log_Priority(1 << (c->severity+7)), "%s (type=%d, code=%d)", descript, 
		 c->errorType, c->errorCode));
      }
  else
      {
      ACS_LOG_TIME (0, c->timeStamp, c->routine.in(), 
		    (ACE_Log_Priority(1 << (c->severity+7)), "%s (type=%d, code=%d)", descript, 
		     c->errorType, c->errorCode));
      }//if-else

  delete[] descript;
}

void ACSError::log(){  
  unsigned int i=0;
  ACSErr::ErrorTrace *c = &errorTrace;

  while (c->previousError.length()!=0){
    i++;
    log (c, depth-i);
    c = &c->previousError[0];
  }
  i++;
  log (c, depth-i);
}

char* ACSError::getDescription (ACSErr::ACSErrType et, ACSErr::ErrorCode ec)
{
    
    if (et>15)
	{
	return CORBA::string_dup("The old ACS error system contained only 15 error types - *move to the new error system*");
	}
    else
	{
	if (ACE_OS::strlen(oldDescriptions[et][0])==0)
	    {
	    return CORBA::string_dup("There was no description for this type/code - *move to the new error system*");
	    }
	else
	    {
	    return CORBA::string_dup(oldDescriptions[et][ec]);
	    }
	}

/*    this code can be used for read error description from old idl files */

/*      CORBA::String_var etStr;   // = getType name from array ??

    // create out file name wher is descriptions for that error type
    ACE_CString file = "idl/" + ACE_CString((char*)etStr.in()) +".idl";

    char  mode, dirMode, filePath[256];
    int fileSize;

    if (!acsFindFile (file.c_str(), filePath, &mode, &fileSize, &dirMode))
	{
	ACE_CString rs("DESCRIPTION CAN NOT BE READ - can not find file: ");
	rs += file;
	return CORBA::string_dup (rs.c_str());
	}
 
    ifstream etFile;
    etFile.open (filePath);
    if (!etFile.is_open ())
	{
	ACE_CString rs("DESCRIPTION CAN NOT BE READ - can not open file: ");
	rs += ACE_CString(filePath);
	return CORBA::string_dup (rs.c_str());
	}

    char line[512];
    int lineToJmp=-1;

    while (etFile)
	{
	etFile.getline (line, 512);
        if (lineToJmp!=-1)
	    {
	    char *errDesc=  strstr(line, "//ED:");
	    if (errDesc)
		{
		if (lineToJmp>0) 
		    {
		    lineToJmp--;
		    }
		else
		    {// we are in the right line
		    //		ACE_OS::printf ("Description: %s\n", errDesc+5);
		    int i;
                    for(i=5; errDesc[i]!=0 && errDesc[i]==' '; i++);
		    etFile.close();
		    return CORBA::string_dup(errDesc+i);
		    }
		}
	    }
	else
	    {  
	    if (strstr (line, "enum"))
		lineToJmp = ec;	   
	    }
	}

    etFile.close();    
    return CORBA::string_dup ("DESCRIPTION CAN NOT BE READ - //ED: ... can not be find");
*/
}

bool ACSError::init ()
{
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	struct sigaction sigSegVAction;
#endif
    orb = CORBA::ORB::_nil();

    ACE_Utils::UUID_GENERATOR::instance ()->init ();
    
    m_oldUnexpected = std::set_unexpected(acserrUnspecifiedExHandler);
    m_oldTerminate = std::set_terminate(acserrUncaughtExHandler);

#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
    std::memset(&sigSegVAction, 0, sizeof(struct sigaction));

    sigSegVAction.sa_sigaction = acserrSigSegvHandler;
    sigSegVAction.sa_flags = SA_SIGINFO | SA_RESETHAND;

    if(sigaction(SIGSEGV, &sigSegVAction, 0) != 0)
    {
    	ACS_SHORT_LOG((LM_WARNING, "Error while installing the new SIGSEGV signal handler: !", std::strerror(errno)));
    }//if
#endif //!MAKE_VXWORKS
    initialized = true;

    return true;
}//init


bool ACSError::init (CORBA::ORB_ptr _orb)
{
#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
	struct sigaction sigSegVAction;
#endif //!MAKE_VXWORKS

    orb = CORBA::ORB::_duplicate(_orb);
    m_oldUnexpected = std::set_unexpected(acserrUnspecifiedExHandler);
    m_oldTerminate = std::set_terminate(acserrUncaughtExHandler);

#if !defined (MAKE_VXWORKS) && !defined(__CYGWIN__)
    std::memset(&sigSegVAction, 0, sizeof(struct sigaction));

    sigSegVAction.sa_sigaction = acserrSigSegvHandler;
    sigSegVAction.sa_flags = SA_SIGINFO | SA_RESETHAND;

    if(sigaction(SIGSEGV, &sigSegVAction, 0) != 0)
    {
    	ACS_SHORT_LOG((LM_WARNING, "Error while installing the new SIGSEGV signal handler: !", std::strerror(errno)));
    }//if

#endif //!MAKE_VXWORKS
    initialized = true;

    return true;
}//init

bool ACSError::init (int argc, char *argv[])
{    
    try
	{ 
	ACS_DEBUG("ACSError::init", "Initialising ORB ... "); 
	CORBA::ORB_var orb = CORBA::ORB_init (argc, argv, 0); 
	
	ACS_DEBUG ("ACSError::init", "ORB initialised !"); 
	} 
    catch( CORBA::Exception &ex )
	{ 
	ex._tao_print_exception("Failed to initalise ORB"); 
	return -1; 
	} 

    return init (orb.in());
}

bool ACSError::initLog (CORBA::ORB_ptr _orb)
{
    if (LoggingProxy::isInit())
	return true;
    
    LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
    LoggingProxy::init (m_logger); 
    ACS_SHORT_LOG((LM_WARNING, "Logging proxy not initialysed. Create a default one !"));
    ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !"));
    
    return true;
}//initLog

void ACSError::done(){
    //LoggingProxy::done(); 
    if (!CORBA::is_nil(ACSError::orb.in()))
	CORBA::release(ACSError::orb._retn());

	std::set_unexpected(m_oldUnexpected);
	std::set_terminate(m_oldTerminate);
}
/*___oOo___*/





