/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
*
* "@(#) $Id: loggingLoggingProxy.cpp,v 1.28 2007/03/04 17:40:31 msekoran Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* acaproni  2004-11-25  Into log(), replaced < and > with { and } of the msg to avoid parsing errors
* mschilli  2004-01-05  added stdout-flush after the printf's in method log()
* bjeram    2002-03-06  changed ACE_CString AAAA(x, y) to   ACE_CString AAAA((const char*)x, y); needed because by ACE x.3
* msekoran  2002-03-18  Using getTempFileName() helper method.
* msekoran  2002-02-13  Unregistred as ACE logging callback in destructor
* bjeram    2002-01-17  Added stdout flush in flush & converting  output of ACE_OS::getpid()
*                       to unsigned long to work good on Linux and Sun
* msekoran  2001-12-28  Logs with Archive XML entry type do not got to the STDOUT
* msekoran  2001-12-17  Added CL failure detection, logging to syslog, file, ...
* bjeram    2001-11-12  added initialized flag and isInit()
* bjeram    2001-07-12  ACS_LOG_STDIO -> ACS_LOG_STDOUT
* msekoran  2001-07-12  Output to a file if CL is not available
* msekoran  2001-06-08  Implementation according new specifications
* almamgr   2000-12-03  Removed and changed to char* from filename and oldfilename
* almamgr   2000-10-19  Looging of INFO on stdout more compact than XML output
* almamgr   2000-10-19  created 
*/
#include <string>
#include <loggingLoggingProxy.h>

#include <loggingXMLParser.h>

#include <loggingLocalFile.h>
#include <loggingLocalSyslog.h>
#include <loggingRemoteSyslog.h>

#include <acsutilTempFile.h>

 using namespace loggingXMLParser;

#define LOG_NAME "Log"
#define DEFAULT_LOG_FILE_NAME "acs_local_log"

ACE_RCSID(logging, logging, "$Id: loggingLoggingProxy.cpp,v 1.28 2007/03/04 17:40:31 msekoran Exp $");

ACE_TCHAR* LoggingProxy::m_LogEntryTypeName[] =
{
    ACE_TEXT ("Unknown"),		// not in specs
    ACE_TEXT ("Shutdown"), 	// not in specs
    ACE_TEXT ("Trace"),
    ACE_TEXT ("Debug"),
    ACE_TEXT ("Info"),
    ACE_TEXT ("Notice"),
    ACE_TEXT ("Warning"),
    ACE_TEXT ("Startup"),		// not in specs
    ACE_TEXT ("Error"),
    ACE_TEXT ("Critical"),
    ACE_TEXT ("Alert"),
    ACE_TEXT ("Emergency")
};

unsigned int LoggingProxy::setClrCount_m = 0;
bool LoggingProxy::initialized = false;
int LoggingProxy::instances = 0;
int LoggingProxy::m_failureLimit = 3;
int LoggingProxy::m_minReconnectionTime = 15;      // sec

ACE_TSS<LoggingTSSStorage> * LoggingProxy::tss = 0;
//ACE_CString LoggingProxy::m_process=""; // for some reason does not work in cases wherr logs come from ditructor of objects.
char LoggingProxy::m_process[256];


void
LoggingProxy::log(ACE_Log_Record &log_record)
{
    unsigned int flags = (*tss)->flags();
    unsigned long priority = getPriority(log_record);
    
    int privateFlags = (*tss)->privateFlags();
    bool prohibitLocal  = privateFlags & 1; 
    bool prohibitRemote = privateFlags & 2;
   
    ACE_TCHAR timestamp[24];
    formatISO8601inUTC(log_record.time_stamp(), timestamp);
    
    const ACE_TCHAR * entryType = (*tss)->logEntryType();
    if (!entryType)
	{
	if (log_record.priority() <= ACE::log2(LM_MAX))
	    {
	    entryType = m_LogEntryTypeName[log_record.priority()+1];
	    }
	else
	    {
	    entryType = m_LogEntryTypeName[0];      // invalid priority ("programmer exception")
	    }
	}

    LoggingTSSStorage::HASH_MAP_ENTRY *entry;
    LoggingTSSStorage::HASH_MAP_ITER hash_iter = (*tss)->getData();

    if (!prohibitLocal && ACE_OS::strcmp(entryType, "Archive")!=0)      // do not print archive logs
	{
	// to make print outs nice
	ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_printMutex);
	
	if (m_stdio<0)
	    {
	    // log only LM_INFO and higher
	    if (log_record.priority()>=ACE::log2(LM_INFO)) 
		{
                // GCH: if (log_record.priority()>=6)   // LM_WARNING+
                // GCH:     ACE_OS::printf( "ERROR!! " );

		if ((*tss)->sourceObject()==0)
		    {
		    ACE_OS::printf ("%s %s", timestamp, log_record.msg_data());
		    }
		else
		    {
		    ACE_OS::printf ("%s [%s] %s", timestamp, (*tss)->sourceObject(), log_record.msg_data());
		    }

                if (log_record.priority()>=ACE::log2(LM_WARNING))   // LM_WARNING+
                    {
                    for (; hash_iter.next(entry) != 0; hash_iter.advance() )
	               {
                          ACE_OS::printf(",%s=%s", entry->ext_id_.c_str(), entry->int_id_.c_str() );
                       }
                    }
                ACE_OS::printf ("\n");
		ACE_OS::fflush (stdout); //(2004-01-05)msc: added
		}
	    }
	else if (log_record.priority()+1>=(unsigned int)m_stdio)
	    {
            // GCH: if (log_record.priority()>=6)   // LM_WARNING+
            // GCH:    ACE_OS::printf( "ERROR!! " );

	    ACE_OS::printf ("%s ", timestamp);
	    
	    //print out the source object
	    const ACE_TCHAR *so = (*tss)->sourceObject();
	    
	    if(so!=0)
		{
		ACE_OS::printf ("[%s - ", so);
		}
	    
	    // print out routine if set 
	    const ACE_TCHAR * r = (*tss)->routine();
	    //if routine and source object both exist 
	    if (r!=0 && so!=0)
		{
		ACE_OS::printf ("%s] ", r);    
		}
	    //if source object exists but routine does not
	    else if(r==0 && so!=0)
		{
		ACE_OS::printf ("] ");
		}
	    else if(r!=0 && so==0)
		{
		ACE_OS::printf ("[%s] ", r);
		}
	    
	    ACE_OS::printf ("%s", log_record.msg_data());
            if (log_record.priority()>=ACE::log2(LM_WARNING))   // LM_WARNING+
                {
                for (; hash_iter.next(entry) != 0; hash_iter.advance() )
	           {
                   ACE_OS::printf(",%s=%s", entry->ext_id_.c_str(), entry->int_id_.c_str() );
                   }
                }
            ACE_OS::printf ("\n");
	    ACE_OS::fflush (stdout); //(2004-01-05)msc: added
	    }//if-else
	}//if

    // if priority less tha minCachePriority do not cache or log
    if (prohibitRemote || priority < m_minCachePriority) 
	{
	return;
	}
    
    //
    // format XML
    //
    
    ACE_TCHAR line[64];
    ACE_OS::sprintf(line, "<%s TimeStamp=\"%s\"", 
		    entryType, 
		    timestamp); 
    
    ACE_CString xml((size_t)512);    // create buffer of 512 chars to improove performace (avoid reallocating)
    xml = line;
    
    // source info
    if (flags & LM_SOURCE_INFO ||
	(log_record.priority()==ACE::log2(LM_DEBUG)))		// LM_DEBUG
	{
	ACE_Log_Msg *log_msg = ACE_Log_Msg::instance ();
	if (log_msg)
	    {
	    xml += " File=\"";
	    xml += (*tss)->file();
	    ACE_OS::sprintf(line, "\" Line=\"%d\"", (*tss)->line());
	    xml += line;
	    }
	}
    
    // routine (REQUIRED for LM_TRACE and LM_DEBUG)
    const ACE_TCHAR * r = (*tss)->routine();
    if (r || 
	(log_record.priority()==ACE::log2(LM_TRACE)) ||		// LM_TRACE
	(log_record.priority()==ACE::log2(LM_DEBUG)))		// LM_DEBUG
	{
	if (r)
	    {
	    xml += " Routine=\"";
	    int before=xml.length();
	    xml+= ACE_CString(r) + "\"";
	    for (unsigned int t=before; t<xml.length(); t++)
		{
		/*
		 * We do not want to manipulate the strings (too slow) so we try to fix 
		 * some of the problems that can cause a parse error while reading the logs
		 */
		if (xml[t]=='<') xml[t]='{';
		if (xml[t]=='>') xml[t]='}';
		if (xml[t]=='&') xml[t]='#';
		}
	    }
	else
	    {
	    xml += " Routine=\"\"";
	    }
	}
    
    // runtime info
    if (flags & LM_RUNTIME_CONTEXT)
	{
	ACE_Log_Msg *log_msg = ACE_Log_Msg::instance ();
	if (log_msg)
	    {
	    xml += " Host=\"";
	    xml += log_msg->local_host();
	    xml += "\" Process=\"";
	    if (m_process/*.c_str()*/)
		{
		xml += m_process;
		}
	    xml += "\" Thread=\"";
	    const ACE_TCHAR * threadName = (*tss)->threadName();
	    if (threadName)
		{
		xml+= threadName;
		}
	    xml += "\"";
	    }
	}
    
    // context
    r = (*tss)->context();
    if (r || (flags & LM_RUNTIME_CONTEXT))
	{
	if (r)
	    {
	    xml += " Context=\"" + ACE_CString(r) + "\"";
	    }
	else
	    {
	    xml += " Context=\"\"";
	    }
	}

    //source object
    if ((LM_RUNTIME_CONTEXT) && (*tss)->sourceObject()!=0)
	{
	xml += " SourceObject=\"" + ACE_CString((*tss)->sourceObject()) + "\"";
	}
    
    //audience
    if ((*tss)->audience()!=0)
	{
	xml += " Audience=\"" + ACE_CString((*tss)->audience()) + "\"";
	}

    if (log_record.priority()>=ACE::log2(LM_WARNING))		// LM_WARNING+
	{
	// stackId
	r = (*tss)->stackId();
	if (r || (flags & LM_RUNTIME_CONTEXT))
	    {
	    if (r)
		{
		xml += " StackId=\"" + ACE_CString(r) + "\"";
		}
	    else
		{
		xml += " StackId=\"\"";
		}
	    }
	
	// stackLevel
	int sl = (*tss)->stackLevel();
	if ((sl > 0) || (flags & LM_RUNTIME_CONTEXT))
	    {
	    ACE_OS::sprintf(line, " StackLevel=\"%d\"", sl);
	    xml += line;
	    }
	}
    
    // logId
    if ((*tss)->logId())
	{
	xml += " LogId=\"" + ACE_CString((*tss)->logId()) + "\"";
	}
    
    // uri
    if ((*tss)->uri())
	{
	xml += " Uri=\"" + ACE_CString((*tss)->uri()) + "\"";
	}
    
    // priority
    if (priority != log_record.priority()+1)
	{
	ACE_OS::sprintf(line, " Priority=\"%lu\"", priority); //align to ACS priorty
	xml += line;
	}
    
    // add attributes
    for (hash_iter = (*tss)->getAttributes();
	 (hash_iter.next (entry) != 0);
	 hash_iter.advance ())
	{
	xml += " " + entry->ext_id_ + "=\"" + entry->int_id_ + "\"";
	}
    
    xml += ">";
    
    for (hash_iter = (*tss)->getData();
	 (hash_iter.next (entry) != 0);
	 hash_iter.advance ())
	{
	if(entry->int_id_.length() == 0)
	    {
	    /*
	     * Gianluca, Alessandro 2006-10-16
	     * XercesJ complains if we leave an empty element.
	     * Therefore we put the N/A string
	     */
	    xml += "<Data Name=\"" + entry->ext_id_ + "\">N/A</Data>";
	    }
	else
	    {
	    xml += "<Data Name=\"" + entry->ext_id_ + "\"><![CDATA["  + entry->int_id_ + "]]></Data>";
	    }
	}
    
    if (ACE_OS::strlen(log_record.msg_data()))
	{
	xml += "<![CDATA[";
	xml+=log_record.msg_data();
	xml+="]]>";
	}
    
    // end tag
    ACE_OS::sprintf(line, "</%s>", entryType); 
    xml += line;
    
    //
    // XML created, now cache or send
    //
    
    //ACE_OS::printf ("%s", xml.c_str());
    /*
      const ACE_TCHAR * threadName = (*tss)->threadName();
      if (threadName)
      ACE_OS::printf ("Locking %s\n", threadName);
      else
      ACE_OS::printf ("Locking <unnamed>\n");
    */    
    
    ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);
    
    /*
      if (threadName)
      ACE_OS::printf ("Releasing %s\n", threadName);
      else
      ACE_OS::printf ("Releasing <unnamed>\n");
    */
    
    // sent record directly to centralized logger
    if (!m_noLogger && (m_cacheDisabled || (priority > m_maxCachePriority)))
	{
	CORBA::Any record;
	record <<= xml.c_str();
	if (!sendRecord(record))
	    {
	    m_cache.push_back(xml);
	    }
	}
    else
	{
	// cache it to local cache file
	m_cache.push_back(xml);
	}
    
    // transfer cache to centralized logger if necessary
    if (m_cache.size() >= m_cacheSize)
	{
	sendCache();
	}
    
    // clear TSS data
    (*tss)->clear();    
}

void LoggingProxy::logXML(const ACE_TCHAR *xml, bool cache)
{
   ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);

   // first xml string must be checked if it is XML and DTD
   if (cache)
    {
      CORBA::Any record;
      record <<= xml;
      if (!sendRecord(record))
	m_cache.push_back(xml);
    }
  else
    {
      // cache it to local cache file
      m_cache.push_back(xml);
    }

  // transfer cache to centralized logger if necessary
   if (m_cache.size() >= m_cacheSize)
     sendCache();
}
											
void 
LoggingProxy::LogEntryType(const ACE_TCHAR *szType)
{
  if (tss)
    (*tss)->logEntryType(szType);
}

void
LoggingProxy::Routine(const ACE_TCHAR *szRoutine)
{
  if (tss)
    (*tss)->routine(szRoutine);
}

void
LoggingProxy::File(const ACE_TCHAR *szRoutine)
{
  if (tss)
    (*tss)->file(szRoutine);
}

void
LoggingProxy::Line(long lineNumber)
{
  if (tss)
    (*tss)->line(lineNumber);
}

void
LoggingProxy::Flags(unsigned int uiFlags)
{
  if (tss)
    (*tss)->flags(uiFlags);
}

void
LoggingProxy::ThreadName(const ACE_TCHAR *szName)
{
  if (tss)
    (*tss)->threadName(szName);
}

void
LoggingProxy::SourceObject(const ACE_TCHAR *soName)
{
  if (tss)
    (*tss)->sourceObject(soName);
}

const ACE_TCHAR * LoggingProxy::SourceObject()
{
    if (tss)
	return (*tss)->sourceObject();
    else
	return 0;
}//SourceObject

void
LoggingProxy::audience(const ACE_TCHAR *aud)
{
  if (tss)
    (*tss)->audience(aud);
}

const ACE_TCHAR * LoggingProxy::audience()
{
    if (tss)
	return (*tss)->audience();
    else
	return 0;
}//audience

const ACE_TCHAR *
LoggingProxy::ThreadName()
{
  if (tss)
    return (*tss)->threadName();
  else
    return 0;
}

void
LoggingProxy::ProcessName(const ACE_TCHAR *szName)
{
  if (tss)
    strncpy(m_process, szName, 255); 
}

const ACE_TCHAR *
LoggingProxy::ProcessName()
{
  if (tss)
      return m_process;//.c_str();
  else
    return 0;
}


void 
LoggingProxy::ResetAttributes()
{
  if (tss)
    (*tss)->resetAttributes();
}

void
LoggingProxy::AddAttribute(const ACE_TCHAR *szName, const ACE_TCHAR *szValue)
{
  if (tss)
    (*tss)->addAttribute(szName, szValue);
}

void
LoggingProxy::LogId(const ACE_TCHAR *szName)
{
  if (tss)
    (*tss)->logId(szName);
}

void
LoggingProxy::URI(const ACE_TCHAR *szName)
{
  if (tss)
    (*tss)->uri(szName);
}

void
LoggingProxy::StackId(const ACE_TCHAR *szId)
{
  if (tss)
    (*tss)->stackId(szId);
}

const ACE_TCHAR *
LoggingProxy::StackId()
{
  if (tss)
    return (*tss)->stackId();
  else
    return 0;
}

void
LoggingProxy::PrivateFlags(int privateFlags)
{
  if (tss)
    (*tss)->privateFlags(privateFlags);
}

const int
LoggingProxy::PrivateFlags()
{
  if (tss)
    return (*tss)->privateFlags();
  else
    return 0;
}

void
LoggingProxy::StackLevel(int nLevel)
{
  if (tss)
    (*tss)->stackLevel(nLevel);
}

int
LoggingProxy::StackLevel()
{
  if (tss)
    return (*tss)->stackLevel();
  else
    return 0;
}

	
void
LoggingProxy::Context(const ACE_TCHAR *szName)
{
  if (tss)
    (*tss)->context(szName);
}

const ACE_TCHAR*
LoggingProxy::Context()
{
  if (tss)
    return (*tss)->context();
  else 
    return 0;
}


void
LoggingProxy::AddData(const ACE_TCHAR *szName, const ACE_TCHAR *szFormat, ...)
{
  if (!tss)
    return;

  va_list argp;

  va_start (argp, szFormat);

  ACE_TCHAR data[ADD_DATA_VALUE_MAX];
  vsnprintf (data, ADD_DATA_VALUE_MAX, szFormat, argp);
  (*tss)->addData(szName, data);
  va_end (argp);

}


/************************************************************************/

LoggingProxy::LoggingProxy(const unsigned long cacheSize,
			   const unsigned long minCachePriority,
			   const unsigned long maxCachePriority,
			   DsLogAdmin::Log_ptr centralizedLogger,
			   CosNaming::NamingContext_ptr namingContext) :
  m_cacheSize(cacheSize),
  m_minCachePriority(minCachePriority), 
  m_maxCachePriority(maxCachePriority),
  m_logger(DsLogAdmin::Log::_duplicate(centralizedLogger)),
  m_noLogger(false),
  m_namingContext(CosNaming::NamingContext::_duplicate(namingContext)),
  m_failureCount(0),
  m_disconnectionTime(0),
  m_cacheDisabled(!cacheSize || maxCachePriority<minCachePriority),
  m_alreadyInformed(false),
  m_stdio(-1)
{
  if (m_logger.ptr() == DsLogAdmin::Log::_nil())
    m_noLogger = true;

  if (!tss)
      tss = new ACE_TSS<LoggingTSSStorage>;
  instances++;


  // set logger filename
  m_filename = getTempFileName("ACS_LOG_FILE", DEFAULT_LOG_FILE_NAME);

  char *acsSTDIO = getenv("ACS_LOG_STDOUT");
  if (acsSTDIO && *acsSTDIO)
    {
      m_stdio = atoi(acsSTDIO);
    }

  char *acsSyslog = getenv("ACS_LOG_SYSLOG");
  if (acsSyslog && *acsSyslog)
      m_syslog = acsSyslog;

}

LoggingProxy::~LoggingProxy()
{
  flush();  //sendCache();

  // unregister ACE callback
  done();

  instances--;
  if (tss && !instances)
      {
      delete tss;
      tss = 0;
      }
}

void LoggingProxy::flush()
{
  sendCache();
  ACE_OS::fflush (stdout);
}

void
LoggingProxy::formatISO8601inUTC(const ACE_Time_Value &timestamp, ACE_TCHAR str[])
{
  ACE_TCHAR ctp[20];
  time_t ut(timestamp.sec());
  struct tm *utc = ACE_OS::gmtime(&ut);
  ACE_OS::strftime(ctp, sizeof(ctp), "%Y-%m-%dT%H:%M:%S", utc);
  
  ACE_OS::sprintf (str, ACE_TEXT ("%s.%03ld"),
		   ctp, timestamp.usec () / 1000);
}


void
LoggingProxy::failedToSend()
{
    m_failureCount++;

    if (m_failureCount > m_failureLimit)
    {

        // just try to reconnect (update reference)
        // if it is successful do not disable logging to CL yet
        // this only happens if the failure limit has just been reached
        if ((m_failureCount==(m_failureLimit+1)) && reconnectToLogger())
	    return;

	// disable logging to centralized logger, but leave m_logger reference and retry to reconnect
	m_noLogger = true;

	// save disconnect time
	m_disconnectionTime = ACE_OS::gettimeofday();

	ACE_TCHAR timestamp[24];
	formatISO8601inUTC(ACE_OS::gettimeofday(), timestamp);

	ACE_OS::printf ("%s LoggingProxy: Disconnected from the Centralized Logger, using local logging cache.\n", timestamp);
	m_alreadyInformed = false;
    }
}

void
LoggingProxy::successfullySent()
{
    if (m_failureCount > m_failureLimit)
    {
	ACE_TCHAR timestamp[24];
	formatISO8601inUTC(ACE_OS::gettimeofday(), timestamp);
	
	ACE_OS::printf ("%s LoggingProxy: Connection to the Centralized Logger reestablished.\n", timestamp);
    }

    m_failureCount = 0;
}

bool
LoggingProxy::reconnectToLogger()
{
    // reconnect only if m_logger once had reference
    if (m_logger.ptr() != DsLogAdmin::Log::_nil())
    {
	ACE_Time_Value dt = ACE_OS::gettimeofday();
	dt-=m_disconnectionTime;
	if (dt.sec() >= m_minReconnectionTime)
	{
	    
	    // update reference
	    if (m_namingContext.ptr()!=CosNaming::NamingContext::_nil())
	    {
		
	    try
		{

		    CosNaming::Name name;
		    name.length(1);
		    name[0].id = CORBA::string_dup(LOG_NAME);
		    //name[0].kind = ; 
  
		    CORBA::Object_var obj = m_namingContext->resolve(name);
		    

		    if (!CORBA::is_nil(obj.in()))
		    {
			DsLogAdmin::Log_var logger = DsLogAdmin::Log::_narrow(obj.in());
			
			
			if (logger.ptr() != DsLogAdmin::Log::_nil())
			{
			    m_logger = logger;
			    // retry
			    m_noLogger = false;

			}
		    }
		    
		}
	    catch(...)
		{
		    // No-op.
		}
	    }

	    // retry anyway
	    //m_noLogger = false;

	    m_disconnectionTime = ACE_OS::gettimeofday();

	    return !m_noLogger;
	}
    }

    // failed
    return false;
}


/// NO LOGGING IN THIS METHOD !!!
void
LoggingProxy::sendCache()
{
    ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);
    
    // no centralized logger => output to other logging end-point
    if (m_noLogger && (reconnectToLogger()==false))
	{
	ACE_TCHAR timestamp[24];
	formatISO8601inUTC(ACE_OS::gettimeofday(), timestamp);
	
	CacheLogger * logger = 0;
	ACE_CString key;
	
	// use syslog
	if (m_syslog.length()>0)
	    {
	    int facility = -1;
	    ACE_CString host;
	    
	    // parse m_syslog variable
	    // format: [<facility>@]<host>
	    
	    int pos = m_syslog.find("@");
	    // no facility specified, take default
	    if (pos==ACE_CString::npos)
		host = m_syslog;
	    else
		{
		host = m_syslog.substr(pos+1);
		
		ACE_CString fac = m_syslog.substr(0, pos);
		
		if (fac.length()>0)
		    if (isdigit(fac[0]))
			{
			// extract facility number
			sscanf(fac.c_str(), "%d", &facility);
			}
		// parse 'facility string'
		    else
			{
			int n = 1;
			for (; RemoteSyslogLogger::m_facilityNames[n].value != -1; n++)
			    if (ACE_OS::strcmp(RemoteSyslogLogger::m_facilityNames[n].name, fac.c_str())==0)
				{ facility = RemoteSyslogLogger::m_facilityNames[n].value; break; } 
			
			if (facility==-1)
			    ACE_OS::printf ("%s LoggingProxy: Invalid syslog facility name '%s'. Using defaults.\n",
					    timestamp, fac.c_str());
			}
		}
	    
	    
#ifdef ACS_HAS_LOCAL_SYSLOG_CALLS
	    if (ACE_OS::strcmp(host.c_str(), "localhost")==0)
		{
		if (facility!=-1)
		    logger = new LocalSyslogLogger(facility);  // set facility
		else
		    logger = new LocalSyslogLogger();          // default facility
		
		// add pid to syslog ident (process name)
		// watch if LoggingProxy::ProcessName() is unitialized and is 0
		const char * processName = "unknown";
		if (LoggingProxy::ProcessName())
		    processName = LoggingProxy::ProcessName();
		
		ACE_TCHAR buf[200];
		ACE_OS::sprintf(buf, "%s(%lu)", processName, (unsigned long)ACE_OS::getpid());
		key = buf;
		}
	    
#endif
	    if (!logger)
		{
		if (facility!=-1)
		    logger = new RemoteSyslogLogger(facility);  // set facility
		else
		    logger = new RemoteSyslogLogger();          // default facility
		key = host;
		}
	    
	    }
	// use local file
	else
	    {
	    logger = new LocalFileLogger();
	    key = m_filename.c_str();
	    }
	
	//
	// log cache here
	//
	
	if (logger && (m_logger.ptr() == DsLogAdmin::Log::_nil()))
	    {
	    if (logger->open(key.c_str())==0)
		{
		
		XMLElement * xmlElement = 0;
		ACE_CString str(size_t(32));
		int prio;
		
		for (LogDeque::iterator iter = m_cache.begin();
		     iter != m_cache.end(); 
		     iter++)
		    {
		    
		    prio = ACE::log2 (LM_ERROR);         // default (LM_ERROR)
		    
		    // parse "Priority" out
		    xmlElement = XMLParser::parseString(iter->c_str(), true);
		    if (xmlElement!=0)
			{
			if (xmlElement->getAttribute ("Priority", str)==0)
			    {
			    int tprio = atoi(str.c_str());
			    if (tprio)                        // valid value
				prio = tprio;
			    }
			else
			    {                                     // extract from entry type
			    const ACE_TCHAR * type = xmlElement->name();
			    const int len = sizeof(m_LogEntryTypeName) / sizeof(m_LogEntryTypeName[0]);
			    for (int i = 1; i < len; i++)
				if (ACE_OS::strcmp(m_LogEntryTypeName[i], type)==0)
				    { prio = i; break; } 
			    
			    }
			
			delete xmlElement;
			}
		    
		    
		    logger->log(prio, iter->c_str());        // what if error occurs!!!
		    }
		
		if (!m_alreadyInformed)
		    {
		    ACE_OS::printf ("%s %s logger: Cache saved to '%s'.\n", timestamp, logger->getIdentification(), logger->getDestination());
		    m_alreadyInformed=true;
		    }
		
		logger->close();
		m_cache.clear();
		
		delete logger;
		
		return;
		
		}
	    else
		{
		ACE_OS::printf ("%s %s logger: Failed to save logging cache. Cache is lost!\n", timestamp, logger->getIdentification());
		m_cache.clear();
		
		delete logger;
		
		return;
		}
	    }
	

	// should I retain cache or not?!!!
	ACE_OS::printf ("%s Failed to create cache logginger. Logging cache is lost!\n", timestamp);
	m_cache.clear();
	
	return;
	
	}
    
    
    //
    // centralized logger is present, log cache to CL
    //
    
    
    try
	{
	
	// fill anys
	DsLogAdmin::Anys anys(m_cache.size());
	anys.length(m_cache.size());
	int i = 0;
	for (LogDeque::iterator iter = m_cache.begin();
	     iter != m_cache.end(); 
	     iter++)
	    anys[i++] <<= iter->c_str();
	
	m_logger->write_records(anys);
	
	
	// successfully sent
	successfullySent();
	
	// successfully sent, clear cache
	m_cache.clear();
	}
    catch(...)
	{
	failedToSend();
	// this can cause dead-loop (when new log record causes sent action...)
	//ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "(LoggingProxy::sendCache) Unexpected exception occured while sending to the Centralized Logger");
	}
    
}

/// NO LOGGING IN THIS METHOD !!!
bool
LoggingProxy::sendRecord(CORBA::Any &record)
{
  ACE_GUARD_RETURN (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex, false);

  if (m_noLogger && !reconnectToLogger())
    return false;

  
  try
    {

      // fill anys
      DsLogAdmin::Anys anys(1);
      anys.length(1);
      anys[0] = record;

      m_logger->write_records(anys);
      

      // successfully sent
      successfullySent();

      return true;
    }
  catch(...)
    {
      failedToSend();
      // this can cause dead-loop
      //ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "(LoggingProxy::sendRecord) Unexpected exception occured while sending to the Centralized Logger");
    }

  return false;

}

void
LoggingProxy::init(LoggingProxy *loggingProxy) {

 //check if callback has been already set for this thread
 // for some reson  it's problem (at thread shutdown) if we set the callback two times 
    if (ACE_LOG_MSG->msg_callback () == 0) 
	{
	ACE_LOG_MSG->clr_flags (ACE_Log_Msg::STDERR);
	ACE_LOG_MSG->set_flags (ACE_Log_Msg::MSG_CALLBACK);
	ACE_LOG_MSG->msg_callback (loggingProxy); 
	setClrCount_m ++;
	}
    initialized = true;
}

bool
LoggingProxy::isInitThread() {

  return (ACE_LOG_MSG->msg_callback () == 0);
}
    
void
LoggingProxy::done() {
    setClrCount_m --;
    // global flags can be set cleared just when all LoggingProxy have been deleted
    if ( setClrCount_m == 0 )
	{
	ACE_LOG_MSG->set_flags (ACE_Log_Msg::STDERR);
	ACE_LOG_MSG->clr_flags (ACE_Log_Msg::MSG_CALLBACK);
	initialized = false;
	}
}


unsigned long
LoggingProxy::getPriority(ACE_Log_Record &log_record)
{
    // ACE default
// here we have to add 1 to align ACE and ACS priorities. In past it was OK due to a bug in ACE
    unsigned long priority = log_record.priority()+1; 
    unsigned long flag_prio = (*tss)->flags() & 0x0F;
    if (flag_prio)
	priority = flag_prio;

    return priority;
}

