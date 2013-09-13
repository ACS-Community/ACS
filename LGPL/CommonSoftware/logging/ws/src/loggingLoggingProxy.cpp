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
* "@(#) $Id: loggingLoggingProxy.cpp,v 1.88 2012/05/22 13:13:19 bjeram Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* acaproni  2004-11-25  Into log(), replaced < and > with { and } of the msg to avoid parsing errors
* mschilli  2004-01-05  added stdout-flush after theACE_OS::printf's in method log()
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

//#include <acsutilAnyAide.h>
#include <loggingLocalFile.h>
#include <loggingLocalSyslog.h>
#include <loggingRemoteSyslog.h>
#include <loggingLogLevelDefinition.h>

#include <acsutilTempFile.h>
#include <logging_idlS.h>

 using namespace loggingXMLParser;

#define LOG_NAME "Log"
#define DEFAULT_LOG_FILE_NAME "acs_local_log"

ACE_RCSID(logging, logging, "$Id: loggingLoggingProxy.cpp,v 1.88 2012/05/22 13:13:19 bjeram Exp $");
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
    unsigned long priority = getPriority(log_record);
    int privateFlags = (*tss)->privateFlags();
    // 1 - default/priority local prohibit
    // 2 - default/dynamic priority Remote prohibit
    bool prohibitLocal  = privateFlags & 1;
    bool prohibitRemote = privateFlags & 2;
    int localLogLevelPrecedence = (*tss)->logLevelLocalType();
    int remoteLogLevelPrecedence = (*tss)->logLevelRemoteType();
    ACE_TCHAR timestamp[24];
    formatISO8601inUTC(log_record.time_stamp(), timestamp);

    const ACE_TCHAR * entryType = (*tss)->logEntryType();
    std::string s_entryType;
    if (!entryType)
	{
        //DELOUSE Case
        if(log_record.priority()+1 == 13)
            s_entryType = LogLevelDefinition::fromInteger(AcsLogLevels::DELOUSE_VAL).getName();
        //Also TRACE level was moved
        else if (log_record.priority()+1 == 2)
            s_entryType = LogLevelDefinition::fromInteger(AcsLogLevels::TRACE_VAL).getName();
        else
            s_entryType = LogLevelDefinition::fromInteger(log_record.priority()+1).getName();
	entryType = s_entryType.c_str();
	}
    else
	{
	s_entryType = (const char*)entryType;
	}

	if (!prohibitLocal || m_envStdioPriority >= 0){
		if(localLogLevelPrecedence >= CDB_LOG_LEVEL){
			if(m_envStdioPriority >= 0) {
				localLogLevelPrecedence=ENV_LOG_LEVEL;
				prohibitLocal = priority>=(unsigned int)m_envStdioPriority? false:true;
			}else{
				prohibitLocal = priority >= m_minCachePriority? false:true;
			}
		}else if(localLogLevelPrecedence == ENV_LOG_LEVEL)
			prohibitLocal = priority>=(unsigned int)m_envStdioPriority? false:true;
	}
    LoggingTSSStorage::HASH_MAP_ENTRY *entry;
    LoggingTSSStorage::HASH_MAP_ITER hash_iter = (*tss)->getData();

    if (!prohibitLocal && ACE_OS::strcmp(entryType, "Archive")!=0)      // do not print archive logs
	{
    	// that we do not have to allocate string each time we get it from TSS
    	ACE_CString &tmpStr=(*tss)->getTmpStr();
        bool printed = false;

	 if (localLogLevelPrecedence == DEFAULT_LOG_LEVEL)
	    {
	    // log only LM_INFO and higher && less than LM_DELOUSE
	    if (log_record.priority()>=ACE::log2(LM_INFO) && log_record.priority()<12)
		{
		if ((*tss)->sourceObject()==0)
		    {
			tmpStr = timestamp;
			tmpStr += " ";
			tmpStr += log_record.msg_data();
		    //ACE_OS::printf ("%s %s", timestamp, log_record.msg_data());
		    }
		else
		    {
			tmpStr = timestamp;
			tmpStr += " [";
			tmpStr += (*tss)->sourceObject();
			tmpStr += "] ";
			tmpStr += log_record.msg_data();
		    //ACE_OS::printf ("%s [%s] %s", timestamp, (*tss)->sourceObject(), log_record.msg_data());
		    }
            printed = true;
		}
	    }
      else{
    	  tmpStr = timestamp;
    	  tmpStr += " ";
	    //ACE_OS::printf ("%s ", timestamp);

	    //print out the source object
	    const ACE_TCHAR *so = (*tss)->sourceObject();

	    if(so!=0)
		{
		//ACE_OS::printf ("[%s - ", so);
	    	tmpStr += "[";
	    	tmpStr += so;
	    	tmpStr += " - ";
		}

	    // print out routine if set
	    const ACE_TCHAR * r = (*tss)->routine();
	    //if routine and source object both exist
	    if (r!=0 && so!=0)
		{
	    	tmpStr+=r;
	    	tmpStr+="] ";
		//ACE_OS::printf ("%s] ", r);
		}
	    //if source object exists but routine does not
	    else if(r==0 && so!=0)
		{
	    	tmpStr+="] ";
		//ACE_OS::printf ("] ");
		}
	    else if(r!=0 && so==0)
		{
	    	tmpStr+="[";
	    	tmpStr+=r;
	    	tmpStr+="] ";
		//ACE_OS::printf ("[%s] ", r);
		}

	    tmpStr+=log_record.msg_data();
	    //ACE_OS::printf ("%s", log_record.msg_data());
        printed = true;
	}

    //Here we will print the log parameters (attributes) and Data ALWAYS
    if(printed){

        for (hash_iter = (*tss)->getAttributes();
         (hash_iter.next (entry) != 0);
         hash_iter.advance ())
        {
				tmpStr+=" ";
				tmpStr+=entry->ext_id_.c_str();
				tmpStr+="=\"";
				tmpStr+=entry->int_id_.c_str();
				tmpStr+="\"";
         //   ACE_OS::printf(" %s=\"%s\"",entry->ext_id_.c_str(),entry->int_id_.c_str());
	    }

        for (hash_iter = (*tss)->getData();
         (hash_iter.next (entry) != 0);
         hash_iter.advance ())
        {
        if(entry->int_id_.length() != 0)
            {
        	tmpStr+=" ";
        	tmpStr+=entry->ext_id_.c_str();
        	tmpStr+="=\"";
			tmpStr+=entry->int_id_.c_str();
        	tmpStr+="\"";
            //ACE_OS::printf(" %s=\"%s\"",entry->ext_id_.c_str(),entry->int_id_.c_str());
            }
        }

        ACE_OS::printf ("%s\n", tmpStr.c_str());
	    ACE_OS::fflush (stdout); //(2004-01-05)msc: added
    }//if printed

    }//else//if


    // this is the case of the proxy created not by maci
    if(remoteLogLevelPrecedence >= CDB_LOG_LEVEL){
        if(m_envCentralizePriority >= 0) {
            remoteLogLevelPrecedence=ENV_LOG_LEVEL;
            prohibitRemote = priority>=(unsigned int)m_envCentralizePriority? false:true;
        }else{
            prohibitRemote = priority>=m_minCachePriority? false:true;
        }
    }else if(remoteLogLevelPrecedence == ENV_LOG_LEVEL)
        prohibitRemote = priority>=(unsigned int)m_envCentralizePriority? false:true;

    if (prohibitRemote)
	{
	// anyway we have to clear TSS data
	(*tss)->clear();
	return;
	}

    if(!m_logBin){
	    sendXmlLogs(log_record, timestamp, s_entryType.c_str());
    }else{
        sendBinLogs(log_record, timestamp, s_entryType.c_str());
    }

    // clear TSS data
   // (*tss)->clear();
  }//log

  void LoggingProxy::sendBinLogs(ACE_Log_Record &log_record, const ACE_TCHAR * timestamp, const ACE_TCHAR * entryType){
	ACSLoggingLog::LogBinaryRecord *s_log = new ACSLoggingLog::LogBinaryRecord();
    unsigned int flags = (*tss)->flags();
    unsigned long priority = getPriority(log_record);

	s_log->type = LogLevelDefinition::fromInteger(log_record.priority()+1).getValue();
    //m_LogBinEntryTypeName[log_record.priority()+1];

	s_log->TimeStamp = timestamp;
    if (flags & LM_SOURCE_INFO ||
		(log_record.priority()==ACE::log2(LM_DEBUG)))		// LM_DEBUG
	{
		ACE_Log_Msg *log_msg = ACE_Log_Msg::instance ();
		if (log_msg)
	    {
	        s_log->File = (*tss)->file();
	    	s_log->Line = (*tss)->line();
	    }
	}
    // routine (REQUIRED for LM_TRACE and LM_DEBUG)
    const ACE_TCHAR * r = (*tss)->routine();
    if (r ||
	(log_record.priority()==1)  ||               		// LM_TRACE
	(log_record.priority()==12) ||               		// LM_DELOUSE
	(log_record.priority()==ACE::log2(LM_DEBUG)))		// LM_DEBUG
	{
	    if (r){
            s_log->Routine = (*tss)->routine();
        }else{
	        s_log->Routine= "";
	    }
	}

    // runtime info
    if (flags & LM_RUNTIME_CONTEXT)
	{
	    ACE_Log_Msg *log_msg = ACE_Log_Msg::instance ();
        if (log_msg)
	    {
	    //BJE: Here before was not CORBA::string_dup ???
	    s_log->Host = CORBA::string_dup( (host()==0) ? log_msg->local_host() : host());
            if (m_process){
                s_log->Process =CORBA::string_dup( m_process);
            }

            const ACE_TCHAR * threadName = (*tss)->threadName();
            if (threadName){
                s_log->Thread = threadName;
            }
        }
	}
    // context
    r = (*tss)->context();
    if (r || (flags & LM_RUNTIME_CONTEXT))
	{
	if (r)
	    {
	    s_log->LogContext = (*tss)->context();
	    }
	else
	    {
	    s_log->LogContext = "";
	    }
	}
    //source object
    if ((LM_RUNTIME_CONTEXT) && (*tss)->sourceObject()!=0)
	{
	s_log->SourceObject = (*tss)->sourceObject();
	}

    s_log->StackLevel=-1;
  //  s_log->StackId = "";
    if (log_record.priority()>=ACE::log2(LM_WARNING))		// LM_WARNING+
	{
	    // stackId -- log id
	    r = (*tss)->stackId();
	    if (r || (flags & LM_RUNTIME_CONTEXT)){
	        if (r){
                s_log->StackId = (*tss)->stackId();
		    }else{
		        s_log->StackId = "-";
		    }
        }

        // stackLevel
        int sl = (*tss)->stackLevel();
        if ((sl > 0) || (flags & LM_RUNTIME_CONTEXT)){
            s_log->StackLevel = (*tss)->stackLevel();
        }
	}

    // logId
    if ((*tss)->logId())
	{
	s_log->LogId = (*tss)->logId();
	}

    // uri
    if ((*tss)->uri())
	{
	    s_log->Uri = (*tss)->uri();
	}

	s_log->Priority = priority;
    if((*tss)->audience()!=0)
        s_log->Audience = (*tss)->audience();

    if((*tss)->array()!=0)
        s_log->Array = (*tss)->array();

    if((*tss)->antenna()!=0)
        s_log->Antenna = (*tss)->antenna();

    LoggingTSSStorage::HASH_MAP_ITER hash_iter = (*tss)->getAttributes();
    LoggingTSSStorage::HASH_MAP_ENTRY *entry;
    // add attributes
    int max = 128;
    ACSLoggingLog::NameValue aux [max];
    int i,j,length;
    for (i=0; (hash_iter.next (entry) != 0) && i<max; hash_iter.advance ()){
        aux[i].name = entry->ext_id_.c_str();
        aux[i].value = entry->int_id_.c_str();
        i++;
    }
    s_log->attributes = ACSLoggingLog::NameValueSeq(i);
    for (j=0;j<i;j++){
        length = s_log->attributes.length();
        s_log->attributes.length(length+1);
        s_log->attributes[length] = aux[j];
    }

     ACSLoggingLog::NameValue aux2[max];
    i=0;
    for (hash_iter = (*tss)->getData(); (hash_iter.next (entry) != 0) && i<max; hash_iter.advance ()){
        aux2[i].name = entry->ext_id_.c_str();
        if(entry->int_id_.length() == 0){
            aux2[i].value = "";
        }else{
            aux2[i].value = entry->int_id_.c_str();
	    }
        i++;
	}
    s_log->log_data = ACSLoggingLog::NameValueSeq(i);
    for (j=0;j<i;j++){
        length = s_log->log_data.length();
        s_log->log_data.length(length+1);
        s_log->log_data[length] = aux2[j];
    }

    if (ACE_OS::strlen(log_record.msg_data())){
	    s_log->MsgData = log_record.msg_data();
	}

    (*tss)->clear();
    ACE_GUARD_REACTION (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex, printf("problem acquring mutex in loggingProxy::log () errno: %d\n", errno);return);
    if (!m_noLogger && (m_cacheDisabled || (priority > m_maxCachePriority))){
        CORBA::Any record;
        record <<= *s_log;
        if (!sendRecord(record)){
            m_bin_cache.push_back(s_log);
        }else delete s_log;
	}
    else{
	    // cache it to local cache file
	    m_bin_cache.push_back(s_log);
    }

    // transfer cache to centralized logger if necessary
    if (m_bin_cache.size() >= m_cacheSize){
		// avoid excessive signaling
	    if(m_bin_cache.size() > 0)
            sendCache();
    }

  }

  void LoggingProxy::sendXmlLogs(ACE_Log_Record &log_record,  const ACE_TCHAR * timestamp, const ACE_TCHAR * entryType){
    //
    // format XML
    //

    unsigned int flags = (*tss)->flags();
    unsigned long priority = getPriority(log_record);
    ACE_TCHAR line[64];
    ACE_OS::sprintf(line, "<%s TimeStamp=\"%s\"",
    //ACE_OS::sprintf(line, "<LogEntry Type=\"%d\" TimeStamp=\"%s\"",
		    entryType,
		    timestamp);

    // that we do not have to allocate string each time we get it from TSS
    ACE_CString &xml=(*tss)->getTmpStr();
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
	(log_record.priority()== 1) ||		                // LM_TRACE
    (log_record.priority()== 12) ||                      // LM_DELOUSE
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
	    xml += ((host()==0) ? log_msg->local_host() : host());
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
    //array
    if ((*tss)->array()!=0)
    {
    xml += " Array=\"" + ACE_CString((*tss)->array()) + "\"";
    }
    //antenna
    if ((*tss)->antenna()!=0)
    {
    xml += " Antenna=\"" + ACE_CString((*tss)->antenna()) + "\"";
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

    // priority, discard the cases when TRACE and DELOUSE levels are used
    if (priority != log_record.priority()+1 && priority != 1 && priority != 2)
	{
	ACE_OS::sprintf(line, " Priority=\"%lu\"", priority); //align to ACS priorty
	xml += line;
	}


    LoggingTSSStorage::HASH_MAP_ITER hash_iter = (*tss)->getData();
    LoggingTSSStorage::HASH_MAP_ENTRY *entry;
    // add attributes
    for (hash_iter = (*tss)->getAttributes();
	 (hash_iter.next (entry) != 0);
	 hash_iter.advance ())
	{
	xml += " " + entry->ext_id_ + "=\"" + entry->int_id_ + "\"";
	}

    xml += ">";

    if (ACE_OS::strlen(log_record.msg_data()))
	{
	xml += "<![CDATA[";
	xml+=log_record.msg_data();
	xml+="]]>";
	}

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

    // we can clear TSS data here, because we do not need them anymore
    (*tss)->clear();
    ACE_GUARD_REACTION (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex, printf("problem acquring mutex in loggingProxy::log () errno: %d\n", errno);return);
    //ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);

    // sent record directly to centralized logger
    if (!m_noLogger && (m_cacheDisabled || (priority > m_maxCachePriority)))
	{
	 if (oldLog == NULL){
	    CORBA::Any record;
	    record <<= xml.c_str();
	    if (!sendRecord(record))
	    {
	       m_cache.push_back(xml);
	    }
	 }
	 else{
	    Logging::XmlLogRecordSeq reclist;
	    reclist.length(1);
	    reclist[0].xml = xml.c_str();
	    if (!sendRecord(reclist))
	    {
	       m_cache.push_back(xml);
	    }
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
		// avoid excessive signaling
		if (m_cache.size() > 0){
			struct timespec ts;
			ts.tv_sec=0;
			ts.tv_nsec=100;
			sendCache();
			ace_mon.release();
			//FIXME: This nanosleep provide stability to the logging system. It shouldn't exist :(
			nanosleep(&ts,NULL);
		}
	}
}

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

void
LoggingProxy::array(const ACE_TCHAR *aud)
{
    if (tss)
        (*tss)->array(aud);
}


const ACE_TCHAR * LoggingProxy::array()
{
    if (tss)
	return (*tss)->array();
    else
	return 0;
}//array

void
LoggingProxy::antenna(const ACE_TCHAR *aud)
{
    if (tss)
        (*tss)->antenna(aud);
}


const ACE_TCHAR * LoggingProxy::antenna()
{
    if (tss)
	return (*tss)->antenna();
    else
	return 0;
}//antenna

void
LoggingProxy::host(const ACE_TCHAR *h)
{
    if (tss)
        (*tss)->host(h);
}//host


const ACE_TCHAR * LoggingProxy::host()
{
    if (tss)
	return (*tss)->host();
    else
	return 0;
}//host


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
LoggingProxy::LogLevelRemoteType(int logLevelRemoteType)
{
  if (tss)
    (*tss)->logLevelRemoteType(logLevelRemoteType);
}

const int
LoggingProxy::LogLevelRemoteType()
{
  if (tss)
    return (*tss)->logLevelRemoteType();
  else
    return 0;
}
void
LoggingProxy::LogLevelLocalType(int logLevelLocalType)
{
  if (tss)
    (*tss)->logLevelLocalType(logLevelLocalType);
}

const int
LoggingProxy::LogLevelLocalType()
{
  if (tss)
    return (*tss)->logLevelLocalType();
  else
    return 0;
}

void LoggingProxy::logXML(const ACE_TCHAR *xml, bool cache)
{
   ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);

   // first xml string must be checked if it is XML and DTD
   if(!m_logBin){
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
    }else{
        //TODO: transform to bin??
    }
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
			   Logging::AcsLogService_ptr centralizedLogger,
			   CosNaming::NamingContext_ptr namingContext,
			   const unsigned int autoFlushTimeoutSec,
			   const int maxLogsPerSecond) :
  m_cacheSize(cacheSize),
  m_minCachePriority(minCachePriority),
  m_maxCachePriority(maxCachePriority),
  m_autoFlushTimeoutSec(autoFlushTimeoutSec),
  m_maxLogsPerSecond(maxLogsPerSecond),
  logThrottleAlarm_p(NULL),
  m_logger(Logging::AcsLogService::_duplicate(centralizedLogger)),
  m_noLogger(false),
  m_namingContext(CosNaming::NamingContext::_duplicate(namingContext)),
  m_failureCount(0),
  m_disconnectionTime(0),
  m_cacheDisabled(!cacheSize || maxCachePriority<minCachePriority),
  m_alreadyInformed(false),
  m_envStdioPriority(-1),
  m_envCentralizePriority(-1),
  m_doWorkCond(m_doWorkMutex),
  m_sendingPending(false),
  m_threadCreated(false),
  //m_threadStart(2),
  m_threadShutdown(2),
  m_shutdown(false)
{
  logThrottle = new ::logging::LogThrottle(maxLogsPerSecond);
  
  if (CORBA::is_nil(m_logger.ptr()))
	  m_noLogger = true;

  if (!tss)
      tss = new ACE_TSS<LoggingTSSStorage>;
  instances++;

  char *acsSTDIO = getenv("ACS_LOG_STDOUT");
  if (acsSTDIO && *acsSTDIO)
    {
      m_envStdioPriority = atoi(acsSTDIO);
    }

  char *acsSyslog = getenv("ACS_LOG_SYSLOG");
  if (acsSyslog && *acsSyslog)
      m_syslog = acsSyslog;

  m_logBin = false;
  char *acsLogType = getenv("ACS_LOG_BIN");
  if (acsLogType && *acsLogType){
    if(strcmp("true", acsLogType) == 0)
        m_logBin = true;
  }
  char *acsCentralizeLogger = getenv("ACS_LOG_CENTRAL");
  if (acsCentralizeLogger && *acsCentralizeLogger)
    {
      m_envCentralizePriority = atoi(acsCentralizeLogger);
    }

  oldLog = ACE_OS::getenv("LOG_SERVICE_USE_EXTENSIONS");
  /*if (oldLog!= NULL){
			 std::cout << "Using Logging Service Extensions" << std::endl;
  }*/
}

LoggingProxy::~LoggingProxy()
{
  flush();  //sendCacheInternal();

  // unregister ACE callback
  done();

  instances--;
  if (tss && !instances)
      {
      delete tss;
      tss = 0;
      }

  // signal work thread to exit
  m_shutdown = true;
  m_doWorkCond.signal();
  if (m_threadCreated)
    m_threadShutdown.wait();

  delete logThrottle;
}

void LoggingProxy::flush()
{
  sendCacheInternal();
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
    if (!CORBA::is_nil(m_logger.ptr()))
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
			Logging::AcsLogService_var logger = Logging::AcsLogService::_narrow(obj.in());


			if (!CORBA::is_nil(logger.ptr()))
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

void*
LoggingProxy::worker(void* arg)
{
	static_cast<LoggingProxy*>(arg)->svc();
	return 0;
}

int
LoggingProxy::svc()
{
    // start barrier
    //m_threadStart.wait();

    // started on demand, so flush immediately
    sendCacheInternal();

    while (!m_shutdown)
	{
	ACE_Time_Value timeout = ACE_OS::gettimeofday() + ACE_Time_Value(m_autoFlushTimeoutSec, 0);
	m_doWorkMutex.acquire(); //we have to acqure mutex bfore we are going to wait !!!
	m_doWorkCond.wait(&timeout);
	m_doWorkMutex.release();  // and after we have to release the mutex !!!
	if (!m_shutdown)
	    sendCacheInternal();
	}

    // shutdown barrier
    m_threadShutdown.wait();

    return 0;
}

void
LoggingProxy::sendCache()
{
    if (!m_threadCreated)
	{
	  // start one worker thread
	  // note this method is called when under m_mutex lock, so this is safe
#ifndef MAKE_VXWORKS
	  if (ACE_Thread::spawn(static_cast<ACE_THR_FUNC>(LoggingProxy::worker), this) != -1)
#else
	  if (ACE_Thread::spawn((ACE_THR_FUNC)(LoggingProxy::worker), this) != -1)
#endif
	  {
	  	m_threadCreated = true;
  	    //m_threadStart.wait();
	  }
	  else
	  {
	  	// not thread available, do it in current thread
	  	sendCacheInternal();
	    return;
	  }
	}

	// signal work thread to get working...
	m_doWorkCond.signal();
}

/// NO LOGGING IN THIS METHOD !!!
void
LoggingProxy::sendCacheInternal()
{
	// TBD: I think that we do not need m_sendingPending anymore, because thee is just one thread that sends.
	if (m_sendingPending)
		return;

    ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);

 	// nothing to send check
	if (instances == 0)
		return;
    if(!m_logBin && m_cache.size() == 0) return;
    if(m_logBin && m_bin_cache.size() == 0) return;

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
	    if (pos==(int)ACE_CString::npos)
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
		if(m_filename.length() == 0)
		{
			// set logger filename and eventually creates missing directories
			m_filename = getTempFileNameAndCreateMissingDirs(timestamp);
		}
		if(m_filename.length() == 0)
		{
			ACE_OS::printf ("%s Failed to save logging cache. Cache is lost!\n", timestamp);
		}
		if ((ACE_OS::strcmp(m_filename.c_str(), "/dev/null")==0) || (m_filename.length() == 0))
		{
			if (!m_logBin)
				m_cache.clear();
			else
				m_bin_cache.clear();
			return;
		}
		logger = new LocalFileLogger();
		key = m_filename.c_str();
	}

	//
	// log cache here
	//

	if (logger && (CORBA::is_nil(m_logger.ptr())))
	{
		if(logNonCentralizedLoggerCache(logger,key, timestamp)!= 0)
		{
			if (m_syslog.length()<=0)
			{
				// It looks like the directory was removed or the permissions changed
				// Retry to create the directory and save the logs immediately
				ACE_OS::printf ("%s %s logger: Failed to save logging cache in %s! (Directory removed or permissions changed?)\n", timestamp, logger->getIdentification(),key.c_str());
				m_filename = getTempFileNameAndCreateMissingDirs(timestamp);
				if (m_filename.length() != 0)
				{
					m_alreadyInformed=false;
					if(logNonCentralizedLoggerCache(logger,m_filename, timestamp)!= 0)
					{
						ACE_OS::printf ("%s %s logger: Failed to save logging cache. Cache is lost!\n", timestamp, logger->getIdentification());
					}
					else
					{
						return;
					}
				}
			}
			else
			{
				ACE_OS::printf ("%s %s logger: Failed to save logging cache. Cache is lost!\n", timestamp, logger->getIdentification());
			}
			if (!m_logBin)
				m_cache.clear();
			else
				m_bin_cache.clear();
			delete logger;
			return;
		} // if(logNonCentralizedLoggerCache(logger,key, timestamp)!= 0)
		else
		{
			return;
		}
	}
    // should I retain cache or not?!!!
    ACE_OS::printf ("%s Failed to create cache logger. Logging cache is lost!\n", timestamp);
    if (!m_logBin) m_cache.clear();
    else m_bin_cache.clear();
    return;
   }
    //
    // centralized logger is present, log cache to CL
    //

	// TAO uses leader/follower concurrency pattern that might cause
	// that this thread will do other things and this can case deadlock
	// this is avoided by using this flag
	m_sendingPending = true;

    try
	{

    if(!m_logBin ){
		// fill anys
		DsLogAdmin::Anys anys(m_cache.size());
		Logging::XmlLogRecordSeq reclist;
		if (oldLog == NULL)
			anys.length(m_cache.size());
		else
			reclist.length(m_cache.size());
       
	  	int i = 0;
        for (LogDeque::iterator iter = m_cache.begin();
             iter != m_cache.end();
             iter++){
					if(oldLog ==NULL)
						anys[i++] <<= iter->c_str();
					else
						reclist[i++].xml = iter->c_str();
	}
        // we have to unlock before we do a remote call to prevent a deadlock
        // this is just temporary solution. We have to solve the problem in case if we can not send logs to logging system
        // (to delete cache afterwards or to put the logs back and how to handle    successfullySent/failedToSend
        // successfully sent, clear cache
        m_cache.clear();
        ace_mon.release();
		  if (oldLog == NULL)
           m_logger->write_records(anys);
		  else
		     m_logger->writeRecords(reclist);

        // here we have to acquire the mutex again. Should be done in better way.
        ace_mon.acquire();
        // successfully sent
        successfullySent();

	}else{
	    // fill anys
        DsLogAdmin::Anys anys(m_bin_cache.size());
        anys.length(m_bin_cache.size());
        int i = 0;
        for (LogBinDeque::iterator iter = m_bin_cache.begin();
             iter != m_bin_cache.end();
             iter++){
            CORBA::Any record;
            record <<= *(*iter);
            anys[i++] = record;
            delete *iter;
        }
        // we have to unlock before we do a remote call to prevent a deadlock
        // this is just temporary solution. We have to solve in better way the problem in case if we can not send
        // logs to logging system
        // (to delete cache afterwards or to put the logs back and how to handle    successfullySent/failedToSend
        // successfully sent, clear cache
        m_cache.clear();
        ace_mon.release();
        m_logger->write_records(anys);

        // here we have to acquire the mutex again to protect successfullySent. Should be done in better way.
        ace_mon.acquire();
        // successfully sent
        successfullySent();
	}//if-else
    }
    catch(...)
	{
    	//TBD: this is now protected!!!
    	ace_mon.acquire();
	    failedToSend();
	// this can cause dead-loop (when new log record causes sent action...)
	//ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "(LoggingProxy::sendCache) Unexpected exception occured while sending to the Centralized Logger");
	}
	 m_sendingPending = false;
}

void LoggingProxy::setAlarmSender(LogThrottleAlarm* alarmSender) {
	ACE_Guard<ACE_Recursive_Thread_Mutex> guard(m_mutex);
	logThrottleAlarm_p=alarmSender;
}


/// NO LOGGING IN THIS METHOD !!!
bool LoggingProxy::sendRecord(const Logging::XmlLogRecordSeq &reclist)
{
	ACE_GUARD_RETURN (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex, false);

	if (m_noLogger && !reconnectToLogger())
		return false;
	try
	{
		bool canSend = false;
		unsigned int logsAvail = logThrottle->checkPublishLogRecord();
		if(logsAvail > 0){
			canSend=true;
			if(reclist.length() > logsAvail)
				const_cast<Logging::XmlLogRecordSeq*>(&reclist)->length(logsAvail);
			logThrottle->updateLogCounter(reclist.length());
		}

		ace_mon.release();
		if(canSend) {
			m_logger->writeRecords(reclist);
			if (logThrottleAlarm_p!=NULL && m_maxLogsPerSecond!=-1) {
				logThrottleAlarm_p->clearLogThrottleAlarm();
			}
		} else {
			if (logThrottleAlarm_p!=NULL && m_maxLogsPerSecond!=-1) {
				logThrottleAlarm_p->raiseLogThrottleAlarm();
			}
		}

		// here we have to acquire the mutex again. Should be done in better way.
		ace_mon.acquire();
		// successfully sent
		successfullySent();

		return true;
	}
	catch(...)
	{
		//TBD: here we have to lock again, but it has to be done in better way
		ace_mon.acquire();
		failedToSend();
		// this can cause dead-loop
		//ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "(LoggingProxy::sendRecord) Unexpected exception occured while sending to the Centralized Logger");
	}

	return false;

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

      // we have to unlock before we do a remote call to prevent a deadlock
      // this is just temporary solution. We have to solve in better way the problem in case if we can not send
      // logs to logging system
      // (to delete cache afterwards or to put the logs back and how to handle    successfullySent/failedToSend
      // successfully sent


      ace_mon.release();
      m_logger->write_records(anys);

      // here we have to acquire the mutex again. Should be done in better way.
      ace_mon.acquire();
      // successfully sent
      successfullySent();

      return true;
    }
  catch(...)
    {
	  //TBD: here we have to lock again, but it has to be done in better way
	  ace_mon.acquire();
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
    ACE_LOG_MSG->msg_callback (NULL);
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
    //DELOUSE case
    if(priority == 13)
        priority = 2;
    //TRACE case
    else if (priority == 2)
        priority = 1;
    if (flag_prio)
 	priority = flag_prio;

    return priority;
}

std::string LoggingProxy::BinToXml(ACSLoggingLog::LogBinaryRecord* record){
    ACE_TCHAR line[64];
    ACE_CString xml((size_t)512);    // create buffer of 512 chars to improove performace (avoid reallocating)

    ACE_OS::sprintf(line, "<%s TimeStamp=\"%s\"",
        //    m_LogEntryTypeName[record->type],
            LogLevelDefinition::fromInteger(record->type).getName().c_str(),
            record->TimeStamp.in());

    xml = line;

    // source info
    xml += " File=\"";
    xml += record->File;
    ACE_OS::sprintf(line, "\" Line=\"%d\"", record->Line);
    xml += line;

    xml += " Routine=\"";
    xml+= record->Routine.in();
    xml +=  "\"";

    xml += " Host=\"";
    xml += record->Host.in();
    xml += "\" Process=\"";
    xml += record->Process.in();
    xml += "\" Thread=\"";
    xml+= record->Thread.in();
    xml += "\"";

    xml += " Context=\"";
    xml += record->LogContext.in();
    xml += "\"";

    xml += " SourceObject=\"";
    xml += record->SourceObject.in();
    xml += "\"";
    if(strlen(record->StackId) > 0){
        xml += " StackId=\"";
        xml += record->StackId.in();
        xml += "\"";
    }
    if(record->StackLevel != -1){
        ACE_OS::sprintf(line, " StackLevel=\"%d\"", record->StackLevel);
        xml += line;
    }
    if(strcmp("", record->LogId.in())!= 0){
        xml += " LogId=\"";
        xml += record->LogId.in();
        xml += "\"";
    }
    if(strcmp("", record->Uri.in())!= 0){
        xml += " Uri=\"";
        xml += record->Uri.in();
        xml += "\"";
    }
    if(record->Priority != record->type){
        ACE_OS::sprintf(line, " Priority=\"%lu\"", record->Priority); //align to ACS priorty
        xml += line;
    }
    /*xml += " Audience=\"";
    xml += record->Audience.in();
    xml += "\"";
*/
    for (unsigned int i = 0; i< record->attributes.length() ; i++)
    {
        xml += " ";
        xml += record->attributes[i].name.in();
        xml += "=\"";
        xml += record->attributes[i].value.in();
        xml += "\"";
    }

    xml += ">";

    for (unsigned int i = 0; i< record->log_data.length() ; i++)
	{
        xml += "<Data Name=\"";
        xml += record->log_data[i].name.in();
        xml += "\">";
        if(strlen(record->log_data[i].value.in()) > 0){
            xml += "<![CDATA[";
            xml += record->log_data[i].value.in();
            xml += "]]>";
        }else   xml += "N/A";
        xml += "</Data>";
	}

	xml += "<![CDATA[";
	xml+=record->MsgData.in();
	xml+="]]>";

    // end tag
//    ACE_OS::sprintf(line, "</%s>", m_LogEntryTypeName[record->type]);
    ACE_OS::sprintf(line, "</%s>", LogLevelDefinition::fromInteger(record->type).getName().c_str());
    xml += line;
    return xml.c_str();
}

void LoggingProxy::setCentralizedLogger(Logging::AcsLogService_ptr centralizedLogger)
{
    ACE_GUARD (ACE_Recursive_Thread_Mutex, ace_mon, m_mutex);
    m_logger = Logging::AcsLogService::_duplicate(centralizedLogger);
    if (CORBA::is_nil(m_logger.ptr()))
	m_noLogger = true;
    else
	m_noLogger = false;
}//LoggingProxy::setCentralizedLogger

ACE_TSS<LoggingTSSStorage> *LoggingProxy::getTSS(){
	return tss;
}

ACE_CString LoggingProxy::getTempFileNameAndCreateMissingDirs(const ACE_TCHAR * timestamp)
{
	ACE_CString result;
	char * acs_log_file_env = ACE_OS::getenv("ACS_LOG_FILE");
	ACE_CString filename = DEFAULT_LOG_FILE_NAME;
	if(acs_log_file_env != NULL)
	{
		// ACS_LOG_FILE environment variable is defined
		if (ACE_OS::strcmp(acs_log_file_env, "/dev/null")==0)
		{
			result = acs_log_file_env;
			return result;
		}
		// Check whether the base directory exists,
		// and that we have write permission
		// Tries to create the base directory and its parents
		// if they do not exist yet
		ACE_CString baseDir = getParentFolder(acs_log_file_env);
		if (createMissingDirectories(baseDir.c_str(), timestamp) != 0)
		{
			// Fallback to alternative directory
			filename = ACE::basename(acs_log_file_env);
			ACE_OS::printf ("%s Logging cache cannot be written in %s directory (from ACS_LOG_FILE environment variable).\nFalling back to alternate solution.\n",
					timestamp,baseDir.c_str());
		}
		else
		{
			// The missing directories have been created
			result = acs_log_file_env;
			return result;
		}
	}
	result = getTempFileName(0, filename.c_str());
	ACE_CString baseDir = getParentFolder(result.c_str());
	if (createMissingDirectories(baseDir.c_str(), timestamp) != 0)
	{
		ACE_OS::printf ("%s Logging cache cannot be written in %s directory.\n",
							timestamp,baseDir.c_str());
		return "";
	}
	// The missing directories have been created
	return result;
}

/*
 * Check that the file having the name given as parameter
 * can be created in the specified path and tries to create the missing
 * parent folders if they do not exists
 */
int LoggingProxy::createMissingDirectories(const ACE_TCHAR * dirname, const ACE_TCHAR * timestamp)
{
	if (makeDirectory(dirname, timestamp) != 0)
	{
		return -1;
	}
	// Check if the directory is writable and has executable permissions
	if (ACE_OS::access(dirname,W_OK|X_OK)!=0)
	{
		ACE_OS::printf ("%s %s directory does not have write or exec permissions!\n",
									timestamp,dirname);
		return -1;
	}
	return 0;
}

/*
 * Creates the given directory and its parent folders
 */
int LoggingProxy::makeDirectory(const ACE_TCHAR * dirname, const ACE_TCHAR * timestamp)
{
	if (ACE_OS::access(dirname,F_OK)==0)
	{
		// The directory already exists: nothing to do
		return 0;
	}
	// The directory does not exist yet
	ACE_CString baseDirName = getParentFolder(dirname);
	if(makeDirectory(baseDirName.c_str(), timestamp) == 0)
	{
		// The base directory exists
		// Creates the directory with the right permissions (O777)
		mode_t prev_umask = ACE_OS::umask(0);
		if (ACE_OS::mkdir(dirname, (S_IRWXU | S_IRWXG | S_IRWXO))!=0)
		{
			ACE_OS::printf ("%s Failed to create %s directory\n", timestamp, dirname);
			return -1;
		}
		else
		{
			umask(prev_umask); // restore previous umask
			// The directory could be created
			return 0;
		}
	}
	else
	{
		// The base directory could not be created
		return -1;
	}
}

int LoggingProxy::logNonCentralizedLoggerCache(CacheLogger * logger,const ACE_CString & key, const ACE_TCHAR * timestamp)
{
	if (logger->open(key.c_str())==0)
	{
		if(!m_logBin)
		{
			XMLElement * xmlElement = 0;
			ACE_CString str(size_t(32));
			int prio;
			for (LogDeque::iterator iter = m_cache.begin(); iter != m_cache.end(); iter++)
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
						/*const int len = sizeof(m_LogEntryTypeName) / sizeof(m_LogEntryTypeName[0]);
	                            for (int i = 1; i < len; i++)
	                            if (ACE_OS::strcmp(m_LogEntryTypeName[i], type)==0)
	                                { prio = i; break; }
						 */
						prio = LogLevelDefinition::fromName(type).getValue();
					}
					delete xmlElement;
				}
				logger->log(prio, iter->c_str());        // what if error occurs!!!
			}
		} // if(!m_logBin)
		else
		{
			ACE_CString str(size_t(32));
			int prio;

			for (LogBinDeque::iterator iter = m_bin_cache.begin(); iter != m_bin_cache.end(); iter++)
			{
				prio = ACE::log2 (LM_ERROR);         // default (LM_ERROR)
				// parse "Priority" out
				int tprio = (*iter)->Priority;
				if (tprio)                        // valid value
					prio = tprio;
				else
				{                                     // extract from entry type
					AcsLogLevels::logLevelValue type = (*iter)->type;
					std::string name = LogLevelDefinition::fromInteger(type).getName();
					if(name != AcsLogLevels::OFF_NAME)
						prio = type;

				}
				logger->log(prio,BinToXml(*iter).c_str());
				delete *iter;
			}
		}
		if (!m_alreadyInformed)
		{
			ACE_OS::printf ("%s %s logger: Cache saved to '%s'.\n", timestamp, logger->getIdentification(), logger->getDestination());
			ACE_OS::fflush (stdout); //(2004-01-05)msc: added
			m_alreadyInformed=true;
		}

		logger->close();
		if (!m_logBin)
			m_cache.clear();
		else
			m_bin_cache.clear();
		delete logger;

		return 0;
	}
	else
	{
		// logger->open(key.c_str()) failed
		return -1;
	}
}

/*
 * Method to get the parent directory name of the file given as parameter
 * This method is preferred over ACE:dirname() which is not reentrant
 */
ACE_CString LoggingProxy::getParentFolder(const ACE_TCHAR * filename)
{
	ACE_CString dir = filename;
	ACE_CString dirName="";
	size_t lastSepPos = dir.rfind(ACE_DIRECTORY_SEPARATOR_CHAR);
	if ((lastSepPos!=ACE_CString::npos) && (lastSepPos > 0))
		dirName = dir.substr(0,lastSepPos);
	else
		dirName = ".";
	return dirName;
}
