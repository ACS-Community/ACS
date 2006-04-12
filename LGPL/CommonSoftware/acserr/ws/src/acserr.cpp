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
* "@(#) $Id: acserr.cpp,v 1.73 2006/04/12 15:08:45 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/

#include <acsutilTimeStamp.h>
#include <acserrExceptionManager.h>
#include <ErrorSystemErrType.h>
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include <time.h>
#include <stdio.h>
#include <iomanip>

static char *rcsId="@(#) $Id: acserr.cpp,v 1.73 2006/04/12 15:08:45 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

/******************************************************************************

ErrorTraceHelper

*******************************************************************************/

char ErrorTraceHelper::m_hostName[64] = "";
char ErrorTraceHelper::m_processName[64] = "";
const unsigned int ErrorTraceHelper::m_maxDepth = 20;

ACS::Time ErrorTraceHelper::getTime()
{
    return ::getTimeStamp();
} 

ErrorTraceHelper::ErrorTraceHelper(ACSErr::ErrorTrace &et, int depth) :
    m_errorTraceRef(et) , m_depth(depth)
{  
    const ACSErr::ErrorTrace *c = &m_errorTraceRef;
    if (m_depth) 
	{ 
	m_depth=1;
	while (c->previousError.length()!=0)
	    {
	    m_depth ++;
	    c = &c->previousError[0];
	    }
	m_current = &m_errorTraceRef;
	}
    else 
	{    
    	m_current = 0/*NULL*/;
	}  
}

ErrorTraceHelper::ErrorTraceHelper(ACSErr::ErrorTrace &et) :
    m_errorTraceRef(et)
{ 
    const ACSErr::ErrorTrace *c = &m_errorTraceRef;
    if (c) 
	{ 
	m_depth=1;
	while (c->previousError.length()!=0)
	    {
	    m_depth ++;
	    c = &c->previousError[0];
	    }
	m_current = &m_errorTraceRef;
	}
    else 
	{    
    	m_current = 0;
  	m_depth = 0;	
	}
}

ErrorTraceHelper::ErrorTraceHelper (ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
				    const char* file, int line, const char* routine, const char* sd, 
				    ACSErr::Severity severity,
				    ACSErr::ErrorTrace &errortrace ) :
	m_errorTraceRef(errortrace)
{ 
    m_depth=1;
    fill(et, ec, severity, file, line, routine, sd); 
}

ErrorTraceHelper::ErrorTraceHelper (const ACSErr::ErrorTrace &pet,
				    ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
				    const char* file, int line, const char* routine, const char* sd, 
				    ACSErr::Severity severity,
				    ACSErr::ErrorTrace &errortrace ) :
    m_errorTraceRef(errortrace)
{ 
    m_depth=1;
    const ACSErr::ErrorTrace *c = &pet;
    while (c->previousError.length()!=0)
	{
	m_depth ++;
	c = &c->previousError[0];
	}

    
    if( m_depth < m_maxDepth ) 
    {
      fill(et, ec, severity, file, line, routine, sd); 

      m_errorTraceRef.previousError.length(1);
      m_errorTraceRef.previousError[0] = pet; 

      m_depth++;
    }
    else if( m_depth > m_maxDepth )
    { 
      m_errorTraceRef = pet;
      m_current = &m_errorTraceRef;
    }
    else 
    { 
      fill(ACSErr::ErrorSystemErrType, ErrorSystemErrType::ErrorTraceOverFlow, 
           ACSErr::Alert, "", 0,  "", 
            ErrorSystemErrType::ErrorTraceOverFlowExImpl::getShortDescription()); 
      
      m_errorTraceRef.previousError.length(1);
      m_errorTraceRef.previousError[0] = pet; 
      
      m_depth++;
      m_current = &m_errorTraceRef;
    }

    
//	ACSErr::ErrorTrace::_tao_seq_ACSErr_ErrorTrace__1 (1, const_cast<ACSErr::ErrorTrace*>(&pet), 0);
}

ErrorTraceHelper& ErrorTraceHelper::operator=(ACSErr::ErrorTrace& et)
{
    m_errorTraceRef = et;

    const ACSErr::ErrorTrace *c = &m_errorTraceRef;
    if (c) 
	{ 
	m_depth=1;
	while (c->previousError.length()!=0)
	    {
	    m_depth ++;
	    c = &c->previousError[0];
	    }//while
	m_current = &m_errorTraceRef;
	}
    else 
	{    
    	m_current = 0;
  	m_depth = 0;	
	}//if-else
    
    return *this;
}// operator=

void ErrorTraceHelper::fill (ACSErr::ACSErrType et, ACSErr::ErrorCode ec, ACSErr::Severity severity,
		   const char* file, int line, const char* routine, const char* sd) 
{
//  errorTrace.description = CORBA::string_dup (ed);
    m_errorTraceRef.timeStamp = getTime();
    m_errorTraceRef.file = CORBA::string_dup (file);
    m_errorTraceRef.lineNum = line;
    m_errorTraceRef.routine = CORBA::string_dup (routine);

    char tid[64];
    ACE_OS::sprintf( tid, "ID: %lu", (long)ACE_Thread_Manager::instance()->thr_self() );
    m_errorTraceRef.thread = CORBA::string_dup (tid);
    
    // setting process name
    if (m_processName[0]==0)
	{
	unsigned long pid = (unsigned long)ACE_OS::getpid();
	ACE_OS::sprintf (m_processName, "PID: %lu", pid);
	}
    m_errorTraceRef.process = CORBA::string_dup (m_processName);
   
    //setting host name
    if (m_hostName[0]==0)
	{
	ACE_OS::hostname (m_hostName, 64);
	}
    m_errorTraceRef.host = CORBA::string_dup (m_hostName);

    m_errorTraceRef.severity = severity;  

    m_errorTraceRef.shortDescription = CORBA::string_dup (sd);
  
    m_errorTraceRef.errorCode = ec; 
    m_errorTraceRef.errorType = et;
    m_current = &m_errorTraceRef;
}

void ErrorTraceHelper::log (ACSErr::ErrorTrace * c, int level){
  unsigned int j;
 
  ACE_Time_Value tv = ACE_OS::gettimeofday();
  char timeBuf[25];
  sprintf(timeBuf, "%ld", tv.sec());
  LoggingProxy::StackId (timeBuf); // unique stack ID = current time
  LoggingProxy::StackLevel (level);

  for (j=0; j<c->data.length(); j++) 
      LoggingProxy::AddData (c->data[j].name.in(), c->data[j].value.in());

  // set runtime context
  ACE_Log_Msg::instance()->local_host(c->host.in()); 
  LoggingProxy::ProcessName (c->process.in());
  LoggingProxy::ThreadName (c->thread.in());

  //create the message
  std::ostringstream ostr;
  ostr << c->shortDescription.in() << " (type=" << c->errorType << ", code=" << c->errorCode << ")" << std::ends;
  //set the logging proxy flags
  LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT);
  ACS_CHECK_LOGGER;
  //just delegate
  LOG_RECORD(Logging::BaseLog::LM_ERROR, 
	     ostr.str(), 
	     c->file.in(), 
	     c->lineNum, 
	     c->routine.in(), 
	     c->timeStamp,
	     getLogger()->getName());
}

void ErrorTraceHelper::log()
{  
  unsigned int i=0;
  ACSErr::ErrorTrace *c = &m_errorTraceRef;

  if (!m_depth) return;

  while (c->previousError.length()!=0){
    i++;
    log (c, m_depth-i);
    c = &c->previousError[0];
  }
  i++;
  log (c, m_depth-i);
}

void ErrorTraceHelper::toString (ACSErr::ErrorTrace * c, int level, std::ostringstream &oss){
   
  oss << "Error Trace Level : " << level << std::endl;

  char ctp[21];
  long sec_ =  ACE_static_cast(CORBA::ULongLong, c->timeStamp) / ACE_static_cast(ACE_UINT32, 10000000u) - ACE_UINT64_LITERAL(0x2D8539C80); 
  long usec_ = (c->timeStamp % 10000000u) / 10;
  time_t tt(sec_);
  struct tm *utc_p = ACE_OS::gmtime(&tt);
  ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H:%M:%S.", utc_p);

  oss << "  " << ctp << std::setfill('0') << std::setw(3) << usec_/1000;
  oss << " in " << c->routine << " of file " << c->file <<  " at line " << c->lineNum << std::endl;
  oss << "  HostName:" << c->host << ", process:" << c->process << " Thread " << c->thread << "," << std::endl;
  oss << "  Severity=" << c->severity;
  oss << " type=" << c->errorType << " code=" << c->errorCode << " description:" << c->shortDescription << std::endl;

  for (unsigned int j=0; j<c->data.length(); j++) 
      oss << "    data[" << j << "] : " <<  c->data[j].name << " = " << c->data[j].value << std::endl;

  return;
}

std::string ErrorTraceHelper::toString()
{  
  std::ostringstream oss;
  
  if (!m_depth) 
    return oss.str();

  ACSErr::ErrorTrace *c = &m_errorTraceRef;
  unsigned int i=0;
  while (c->previousError.length()!=0){
    i++;
    toString(c, m_depth-i,oss);
    c = &c->previousError[0];
  }
  i++;
  toString (c, m_depth-i, oss);
  return oss.str();
}

void ErrorTraceHelper::addData (const char* name, const char* value)
{
    ACSErr::NameValue nv;
    int length = m_current->data.length();

    if (name==NULL || value==NULL)
	return;

    if (strlen(name)<ADD_DATA_VALUE_MAX)
	{
	nv.name = CORBA::string_dup (name);
	}
    else
	{
 // it is dirty trick which allows as to copy just ADD_DATA_VALUE_MAX to CORBA string w/o making local copy of the string.
	char tc=name[ADD_DATA_VALUE_MAX-1]; // store temporary character
	const_cast<char*>(name)[ADD_DATA_VALUE_MAX-1]=0; // temporary set end at ADD_DATA_VALUE_MAX-1
	nv.name = CORBA::string_dup (name); //copy just first ADD_DATA_VALUE_MAX
	const_cast<char*>(name)[ADD_DATA_VALUE_MAX-1]=tc; // set back the character
	}//if-else

// now we do the same with value!
    if (strlen(value)<ADD_DATA_VALUE_MAX)
        {
        nv.value = CORBA::string_dup (value);
        }
    else
        {
// it is dirty trick which allows as to copy just ADD_DATA_VALUE_MAX to CORBA string
        char tc=value[ADD_DATA_VALUE_MAX-1]; // store temporary character
        const_cast<char*>(value)[ADD_DATA_VALUE_MAX-1]=0; // temporary set end at ADD_DATA_VALUE_MAX-1
    nv.value = CORBA::string_dup (value); //copy just first ADD_DATA_VALUE_MAX
    const_cast<char*>(value)[ADD_DATA_VALUE_MAX-1]=tc; // set back the character
        }//if-else

    m_current->data.length (length+1);
    m_current->data[length] = nv;
}//addData


void ErrorTraceHelper::setMemberValue (const char* name, ACE_CString& value)
{ 
    setMemberValue(name, value.c_str());
}

void ErrorTraceHelper::setMemberValue (const char* name, const char* value)
{   
    int i=0,
	length = m_current->data.length();
    if (name == NULL || value == NULL) return;

// let's try to find the data with the same name
    while (i<length && ACE_OS::strcmp(name, m_current->data[i].name) != 0)
	i++;
  
    if (i<length) // have we reached the end ?
	{
	// previous value should be deleted by TAO_String_Manager (=type of value in NameValue structure)
	m_current->data[i].value  = CORBA::string_dup (value);
	}
    else
	{
	addData(name, value);
	}
}//setMemberValue

ACE_CString ErrorTraceHelper::getData (const char* name)
{
    if (name==NULL) return ACE_CString();

    int i=0,
	length = m_current->data.length();
    while (i<length && ACE_OS::strcmp(name, m_current->data[i].name) != 0)
	i++;
    if (i<length) // have we reached the end ?
	return ACE_CString( m_current->data[i].value);
    else
	return ACE_CString();
}//getData

template<>
char * ErrorTraceHelper::getMemberValue<char*> (const char* name)
{
  return CORBA::string_dup(getData(name).c_str());
}

template<>
ACE_CString ErrorTraceHelper::getMemberValue<ACE_CString> (const char* name)
{
  return getData(name);
}

ACSErr::ErrorTrace* ErrorTraceHelper::getNext(){ 
  if (m_depth && m_current->previousError.length()!=0) {
    m_current = &m_current->previousError[0];
    return m_current; 
  }else{
    return NULL;
  }
}

char* ErrorTraceHelper::getDescription(){ 
    return CORBA::string_dup(m_current->shortDescription);
}

void ErrorTraceHelper::setHostName (const char* hn){
  strncpy (m_hostName, hn, 64);
}

void ErrorTraceHelper::setProcessName (const char *pn){
  strncpy (m_processName, pn, 64);
}

/****************************************************************************************
 *
 *               CompletionImpl
 *
 ****************************************************************************************/
CompletionImpl::CompletionImpl() :
        CompletionInit(ACSErr::ErrorSystemErrType, ErrorSystemErrType::EmptyError),
        m_errorTraceHelper(ACSErr::ErrorSystemErrType, ErrorSystemErrType::EmptyError,
                           "",0,"",ErrorSystemErrType::EmptyErrorExImpl::getShortDescription(),
                           DEFAULT_SEVERITY, previousError[0] )
{}

void CompletionImpl::log()
{
    if (!isErrorFree())
	{
	getErrorTraceHelper()->log();
	}
    else
	{
	ACS_LOG_TIME (0, timeStamp, Logging::BaseLog::FIELD_UNAVAILABLE,
		      (LM_NOTICE, "Error-Free Completion (type=%d, code=%d)", getType(), getCode()));
	}
}

CompletionImpl & 
CompletionImpl::operator=(CompletionImpl& c) 
{
  if (this != &c )
	{
	type = c.getType();
	code = c.getCode();
	timeStamp = c.getTimeStamp();

	previousError = c.previousError;
	m_errorTraceHelper = c.previousError[0];

	}
  return *this;
}

CompletionImpl&
CompletionImpl::operator=(Completion* c) 
{
    if (c != NULL )
	{
	type = c->type;;
	code = c->code;
	timeStamp = c->timeStamp;

	previousError = c->previousError;
	m_errorTraceHelper = c->previousError[0];
	}
    else
	{
	// TBD: improved
	printf("WARNING a NULL ACSErr completion can not be assigned to CompletionImpl: CompletionImpl::operator=(Completion* c)!!\n");
	}

  return *this;
}


/*
 * Exception Manager
 *
 */
ACSErr::ExceptionManager ACSErr::ExceptionManager::exmgr;
/*___oOo___*/

