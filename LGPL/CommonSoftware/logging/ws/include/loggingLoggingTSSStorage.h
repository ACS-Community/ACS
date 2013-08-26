#ifndef logging_logging_tssstorage_H
#define logging_logging_tssstorage_H

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
* "@(#) $Id: loggingLoggingTSSStorage.h,v 1.9 2008/08/05 15:46:07 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* almamgr   2000-12-03  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ace/Hash_Map_Manager.h>
#include <ace/Task.h>

class LoggingTSSStorage
{
  public:
    LoggingTSSStorage() : m_threadName(0),
    tmpStr((size_t)512)
	{
	    m_ldata.open(3);
	    m_attributes.open(3);
	    clear();
	}

    ~LoggingTSSStorage()
	{
	    if (m_threadName)
		{
		delete[] m_threadName;
		m_threadName = 0;
		}
	    m_ldata.close();
	    m_attributes.close();
	}

    void clear()
	{
	    m_logEntryType = 0;
	    m_routine = 0;
	    m_file = 0;
	    m_line = 0UL;
	    sourceObject_m = 0;
	    audience_m = 0;
            array_m = 0;
            antenna_m = 0;
	    m_lflags = 0;
	    m_stackId = 0;
	    m_stackLevel = 0;
	    m_context = 0;
	    m_uri = 0;
	    m_logId = 0;
	    m_host = 0;
	    m_privateFlags = 0;
	    logLevelLocalType_m = 6;
	    logLevelRemoteType_m = 6;
	    m_ldata.unbind_all();
	    m_attributes.unbind_all();
	}

    void resetAttributes() { m_attributes.unbind_all(); };

    const ACE_TCHAR * routine() { return m_routine; };
    void routine(const ACE_TCHAR * routine) { m_routine=routine; };

    const ACE_TCHAR * file() { return m_file; };
    void file(const ACE_TCHAR * fileName) { m_file=fileName; };

    const unsigned long line() { return m_line; };
    void line(unsigned long lineNumber) { m_line=lineNumber; };

    const ACE_TCHAR *
    sourceObject() { return sourceObject_m; };

    void
    sourceObject(const ACE_TCHAR * soName) { sourceObject_m = soName; };

    const ACE_TCHAR *
    audience() { return audience_m; };

    void
    audience(const ACE_TCHAR * soName) { audience_m = soName; };

    const ACE_TCHAR *
    array() { return array_m; };

    void
    array(const ACE_TCHAR * soName) { array_m = soName; };

    const ACE_TCHAR *
    antenna() { return antenna_m; };

    void
    antenna(const ACE_TCHAR * soName) { antenna_m = soName; };

    unsigned int flags() { return m_lflags; };
    void flags(unsigned int flags) { m_lflags=flags; };

    const ACE_TCHAR * threadName() { return m_threadName; };
    void threadName(const ACE_TCHAR * name)
	{
	    if (m_threadName)
		{
		delete[] m_threadName;
		m_threadName = 0;
		}

	    if (name)
		{
		m_threadName = new ACE_TCHAR[ACE_OS::strlen(name)+1];
		ACE_OS::strcpy(m_threadName, name);
		}
	};

    const ACE_TCHAR * logEntryType() { return m_logEntryType; };
    void logEntryType(const ACE_TCHAR * logEntryType) { m_logEntryType=logEntryType; };

    const ACE_TCHAR * context() { return m_context; };
    void context(const ACE_TCHAR * context) { m_context=context; };

    int stackLevel() { return m_stackLevel; };
    void stackLevel(int stackLevel) { m_stackLevel=stackLevel; };

    const ACE_TCHAR * stackId() { return m_stackId; };
    void stackId(const ACE_TCHAR * stackId) { m_stackId=stackId; };

    const ACE_TCHAR * logId() { return m_logId; };
    void logId(const ACE_TCHAR * logId) { m_logId=logId; };

    const ACE_TCHAR * uri() { return m_uri; };
    void uri(const ACE_TCHAR * uri) { m_uri=uri; };

    const ACE_TCHAR * host() { return m_host; };
    void host(const ACE_TCHAR * host) { m_host=host; };

    int privateFlags() { return m_privateFlags; };
    void privateFlags(int privateFlags) { m_privateFlags=privateFlags; };

    int logLevelLocalType() { return logLevelLocalType_m; };
    void logLevelLocalType(int logLevelLocalType) { logLevelLocalType_m=logLevelLocalType; };

    int logLevelRemoteType() { return logLevelRemoteType_m; };
    void logLevelRemoteType(int logLevelRemoteType) { logLevelRemoteType_m=logLevelRemoteType; };
    typedef ACE_Hash_Map_Manager <ACE_CString, ACE_CString, ACE_Thread_Mutex> HASH_MAP;
    typedef ACE_Hash_Map_Iterator <ACE_CString, ACE_CString, ACE_Thread_Mutex> HASH_MAP_ITER;
    typedef ACE_Hash_Map_Entry <ACE_CString, ACE_CString> HASH_MAP_ENTRY;

    void addAttribute(const ACE_TCHAR * name,  const ACE_TCHAR * value)
	{
	    m_attributes.bind(name, value);
	};

    void addData(const ACE_TCHAR * name,  const ACE_TCHAR * value)
	{
	    m_ldata.bind(name, value);
	};

    HASH_MAP_ITER getAttributes() {
	return HASH_MAP_ITER(m_attributes);
    };

    HASH_MAP_ITER getData() {
	return HASH_MAP_ITER(m_ldata);
    };

    ACE_CString& getTmpStr(){
		return tmpStr;
    }
  private:

    ACE_TCHAR * m_threadName;		// deep copy

    const ACE_TCHAR * m_logEntryType;
    const ACE_TCHAR * m_routine;
    const ACE_TCHAR * m_file;
    unsigned long m_line;
    const ACE_TCHAR * sourceObject_m;
    const ACE_TCHAR * audience_m;
    const ACE_TCHAR * array_m;
    const ACE_TCHAR * antenna_m;
    unsigned int m_lflags;
    const ACE_TCHAR * m_stackId;
    int m_stackLevel;
    const ACE_TCHAR * m_context;
    const ACE_TCHAR * m_logId;
    const ACE_TCHAR * m_uri;
    const ACE_TCHAR * m_host;

    // used internally by logger
    // bit 0 (LSB) = prohibit stdout
    // bit 1       = prohibit remote
    int m_privateFlags;
    int logLevelLocalType_m;
    int logLevelRemoteType_m;

    LoggingTSSStorage::HASH_MAP m_ldata;
    LoggingTSSStorage::HASH_MAP m_attributes;
    // temporary string that can be used to create log message on the fly.
    ACE_CString tmpStr;
};


#endif /*!logging_logging_tssstorage_H*/

