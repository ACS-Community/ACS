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
* "@(#) $Id: acserr.cpp,v 1.92 2010/05/31 09:36:51 bjeram Exp $"
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
#include "ace/UUID.h"

static char *rcsId="@(#) $Id: acserr.cpp,v 1.92 2010/05/31 09:36:51 bjeram Exp $";
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

void ErrorTraceHelper::setErrorTrace(ACSErr::ErrorTrace &et, int depth)
{
    m_errorTracePtr= &et;
    m_depth = depth;

    const ACSErr::ErrorTrace *c = m_errorTracePtr;
    if (m_depth)
	{
	m_depth=1;
	while (c->previousError.length()!=0)
	    {
	    m_depth ++;
	    c = &c->previousError[0];
	    }
	m_current = m_errorTracePtr;
	}
    else
	{
    	m_current = 0;//NULL
	}
}


ErrorTraceHelper::ErrorTraceHelper(ACSErr::ErrorTrace &et) :
    m_errorTracePtr(&et)
{
    const ACSErr::ErrorTrace *c = m_errorTracePtr;
    if (c)
	{
	m_depth=1;
	while (c->previousError.length()!=0)
	    {
	    m_depth ++;
	    c = &c->previousError[0];
	    }
	m_current = m_errorTracePtr;
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
	m_errorTracePtr(&errortrace)
{
    m_depth=1;
    fill(et, ec, severity, file, line, routine, sd);
}

ErrorTraceHelper::ErrorTraceHelper (const ACSErr::ErrorTrace &pet,
				    ACSErr::ACSErrType et, ACSErr::ErrorCode ec,
				    const char* file, int line, const char* routine, const char* sd,
				    ACSErr::Severity severity,
				    ACSErr::ErrorTrace &errortrace ) :
    m_errorTracePtr(&errortrace)
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
      m_errorTracePtr->previousError.length(1);
      m_errorTracePtr->previousError[0] = pet;
      m_depth++;
//      m_current = &m_errorTraceRef;
    }
    else if( m_depth > m_maxDepth )
	{
        // here we have to deep copy previous error trace !!!
	fill(pet.errorType, pet.errorCode, pet.severity, pet.file, pet.lineNum, pet.routine, pet.shortDescription);
	m_errorTracePtr->previousError.length(1);
	m_errorTracePtr->previousError[0] = pet.previousError[0];
	m_current = m_errorTracePtr;
	}
    else
	{
	fill(ACSErr::ErrorSystemErrType, ::ErrorSystemErrType::ErrorTraceOverFlow,
	     ACSErr::Alert, "acserr.cpp", __LINE__,  "ErrorTraceHelper::ErrorTraceHelper",
	     ::ErrorSystemErrType::ErrorTraceOverFlowExImpl::getShortDescription());

	m_errorTracePtr->previousError.length(1);
	m_errorTracePtr->previousError[0] = pet;

	m_depth++;
	m_current = m_errorTracePtr;
	}

}//ErrorTraceHelper::ErrorTraceHelper

ErrorTraceHelper& ErrorTraceHelper::operator=(ACSErr::ErrorTrace& et)
{
    m_errorTracePtr = &et;
    const ACSErr::ErrorTrace *c = m_errorTracePtr;
    if (c)
	{
	m_depth=1;
	while (c->previousError.length()!=0)
	    {
	    m_depth ++;
	    c = &c->previousError[0];
	    }//while
	m_current = m_errorTracePtr;
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
   char tid[64];

   m_errorTracePtr->timeStamp = getTime();
   m_errorTracePtr->file = CORBA::string_dup (file);
   m_errorTracePtr->lineNum = line;
   m_errorTracePtr->routine = CORBA::string_dup (routine);

   // setting thread name
   if (LoggingProxy::ThreadName()!=0 &&
       strlen(LoggingProxy::ThreadName())>0)
       {
       m_errorTracePtr->thread = CORBA::string_dup (LoggingProxy::ThreadName());
       }
   else
       {
       ACE_OS::sprintf( tid, "ID: %lu", (long)ACE_Thread_Manager::instance()->thr_self() );
       m_errorTracePtr->thread = CORBA::string_dup (tid);
       }//if-else

    // setting process name
    if (m_processName[0]!=0)
	{
	m_errorTracePtr->process = CORBA::string_dup (m_processName);
	}
    else
	if (LoggingProxy::ProcessName()!=0
	    && strlen(LoggingProxy::ProcessName())>0)
	    {
	    m_errorTracePtr->process = CORBA::string_dup (LoggingProxy::ProcessName());
	    }
	else
	    {
	    unsigned long pid = (unsigned long)ACE_OS::getpid();
	    ACE_OS::sprintf (m_processName, "PID: %lu", pid);
	    m_errorTracePtr->process = CORBA::string_dup (m_processName);
	    }//if-else

    //setting host name
    if (m_hostName[0]==0)
	{
	ACE_OS::hostname (m_hostName, 64);
	}
    m_errorTracePtr->host = CORBA::string_dup (m_hostName);

    m_errorTracePtr->severity = severity;

    m_errorTracePtr->shortDescription = CORBA::string_dup (sd);

    m_errorTracePtr->errorCode = ec;
    m_errorTracePtr->errorType = et;
    m_current = m_errorTracePtr;
}

void ErrorTraceHelper::log (ACSErr::ErrorTrace * c,
			    int level, char* stackId,
			    ACE_Log_Priority priorty)
{
    unsigned int j;
    ACE_CString oldProcessName, oldThreadName, oldHost;
    Logging::BaseLog::Priority prio;

    LoggingProxy::StackLevel (level);
    LoggingProxy::StackId (stackId);

    for (j=0; j<c->data.length(); j++)
      LoggingProxy::AddData (c->data[j].name.in(), c->data[j].value.in());

  // set runtime context
//  ACE_Log_Msg::instance()->local_host(c->host.in());

  // here we save old process, host  and thread names
  oldHost = LoggingProxy::host();
  oldProcessName = LoggingProxy::ProcessName();
  oldThreadName = LoggingProxy::ThreadName();

  LoggingProxy::ProcessName (c->process.in());
  LoggingProxy::ThreadName (c->thread.in());

  //create the message
  std::ostringstream ostr;
  ostr << c->shortDescription.in() << " (type=" << c->errorType << ", code=" << c->errorCode << ")" << std::ends;
  //set the logging proxy flags
  LoggingProxy::Flags(LM_SOURCE_INFO | LM_RUNTIME_CONTEXT);

  prio = (priorty!=LM_ERROR) ? Logging::ace2acsPriority(priorty) :
      Logging::BaseLog::Priority(1 << (c->severity+7));

  ACS_CHECK_LOGGER;
  if (strlen(c->sourceObject.in())>0)
      {
      //just delegate
      LOG_RECORD(prio,
		 ostr.str(),
		 c->file.in(),
		 c->lineNum,
		 c->routine.in(),
		 c->timeStamp,
		 c->sourceObject.in());
      }
  else
      {
      //just delegate
      LOG_GLOBAL_RECORD(prio,
		 ostr.str(),
		 c->file.in(),
		 c->lineNum,
		 c->routine.in(),
		 c->timeStamp);
      }//if-else

  // reset process and thread name
  LoggingProxy::ProcessName (oldProcessName.c_str());
  LoggingProxy::ThreadName (oldThreadName.c_str());
  LoggingProxy::host (oldHost.c_str());
}//log

void ErrorTraceHelper::log(ACE_Log_Priority priorty)
{
  unsigned int i=0;
  ACSErr::ErrorTrace *c = m_errorTracePtr;
  char uuidBuf[40];
  ACE_Utils::UUID* uuid = ACE_Utils::UUID_GENERATOR::instance ()->generate_UUID ();

  snprintf(uuidBuf, 40, "%s", uuid->to_string()->c_str());
  delete uuid;

  if (!m_depth) return;

  while (c->previousError.length()!=0){
    i++;
    log (c, m_depth-i, uuidBuf, priorty);
    c = &c->previousError[0];
  }
  i++;
  log (c, m_depth-i, uuidBuf, priorty);
}//log

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

  ACSErr::ErrorTrace *c = m_errorTracePtr;
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

    if (strlen(name)<ADD_DATA_NAME_MAX)
	{
	nv.name = CORBA::string_dup (name);
	}
    else
	{
 // it is dirty trick which allows as to copy just ADD_DATA_NAME_MAX to CORBA string w/o making local copy of the string.
	char tc=name[ADD_DATA_NAME_MAX-1]; // store temporary character
	const_cast<char*>(name)[ADD_DATA_NAME_MAX-1]=0; // temporary set end at ADD_DATA_VALUE_MAX-1
	nv.name = CORBA::string_dup (name); //copy just first ADD_DATA_NAME_MAX
	const_cast<char*>(name)[ADD_DATA_NAME_MAX-1]=tc; // set back the character
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

char* ErrorTraceHelper::getShortDescription(){
    return CORBA::string_dup(m_current->shortDescription);
}

void ErrorTraceHelper::setHostName (const char* hn){
  strncpy (m_hostName, hn, 64);
}

void ErrorTraceHelper::setProcessName (const char *pn){
  strncpy (m_processName, pn, 64);
}

/******************************************************************************
 *
 *               CompletionInit
 *
 ******************************************************************************/
CompletionInit::CompletionInit(const ACSErr::Completion &c) :
    ACSErr::Completion(c) {
}

CompletionInit::CompletionInit(ACSErr::CompletionType t, ACSErr::CompletionCode c, bool initTrace)
{
    type = t;
    code = c;
    timeStamp = ErrorTraceHelper::getTime();
    if (initTrace)
	{
	previousError.length(1);
	previousError[0] = ACSErr::ErrorTrace();
	}//if
}//CompletionInit


/****************************************************************************************
 *
 *               CompletionImpl
 *
 ****************************************************************************************/
CompletionImpl::CompletionImpl() :
    CompletionInit(ACSErr::ErrorSystemErrType, ::ErrorSystemErrType::EmptyError),
    m_errorTraceHelper(ACSErr::ErrorSystemErrType, ::ErrorSystemErrType::EmptyError,
		       "",0,"", ::ErrorSystemErrType::EmptyErrorExImpl::getShortDescription(),
		       DEFAULT_SEVERITY, previousError[0] )
{}

CompletionImpl::CompletionImpl (const ACSErr::Completion &c) :
    CompletionInit(c)//, m_errorTraceHelper(previousError[0], previousError.length())
{
    if (previousError.length() > 0 )
	m_errorTraceHelper.setErrorTrace(previousError[0], previousError.length());
}

CompletionImpl::CompletionImpl(ACSErr::Completion* c, bool del) :
    CompletionInit(*c)//, m_errorTraceHelper(previousError[0], previousError.length())
{
    if (previousError.length() > 0 )
	m_errorTraceHelper.setErrorTrace(previousError[0], previousError.length());

    if (del)
	{
	delete c;
	c = 0;
	}
}

CompletionImpl::CompletionImpl(ACSErr::Completion_var& c) :
    CompletionInit(*c.ptr()),
    m_errorTraceHelper()//previousError[0], previousError.length())
{
 if (previousError.length() > 0)
	m_errorTraceHelper.setErrorTrace(previousError[0], previousError.length());
}

void CompletionImpl::log(ACE_Log_Priority priorty)
{
    if (!isErrorFree())
	{
	getErrorTraceHelper()->log(priorty);
	}
    else
	{
	ACS_LOG_TIME (0, timeStamp, Logging::BaseLog::FIELD_UNAVAILABLE,
		      (LM_INFO, "Completion (type=%d, code=%d) does not contain any error",
		       getType(), getCode()));
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
	if (previousError.length()>0)
	    {
	    m_errorTraceHelper = previousError[0];//c.previousError[0];
	    }//if
	}//if
  return *this;
}

CompletionImpl::CompletionImpl (const CompletionImpl &c)
    : CompletionInit(c), m_errorTraceHelper()
{
    if (previousError.length() > 0)
	m_errorTraceHelper.setErrorTrace(previousError[0], previousError.length());
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
	if (previousError.length()>0)
	    {
	    m_errorTraceHelper = previousError[0];
	    }//if
	}
    else
	{
	// TBD: improved
	printf("WARNING a NULL ACSErr completion can not be assigned to CompletionImpl: CompletionImpl::operator=(Completion* c)!!\n");
	}
    delete c;
    c = 0;
    return *this;
}//operator=(Completion* c)

CompletionImpl&
CompletionImpl::operator=(Completion_var& c)
{
    if (c.ptr() != NULL )
	{
	type = c->type;;
	code = c->code;
	timeStamp = c->timeStamp;
	previousError = c->previousError;
	if (previousError.length()>0)
	    {
	    m_errorTraceHelper = previousError[0];
	    }//if
	}
    else
	{
	// TBD: improved
	printf("WARNING a Completion_var that contains NULL ACSErr completion can not be assigned to CompletionImpl: CompletionImpl::operator=(Completion_var &c)!!\n");
	}
    return *this;
}//operator=(Completion_var& c)

/*
 * Exception Manager
 *
 */
ACSErr::ExceptionManager ACSErr::ExceptionManager::exmgr;
/*___oOo___*/

