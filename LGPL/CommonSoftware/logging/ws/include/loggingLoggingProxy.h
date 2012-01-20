#ifndef logging_logging_proxy_H
#define logging_logging_proxy_H

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
 * "@(#) $Id: loggingLoggingProxy.h,v 1.42 2012/01/20 22:07:44 tstaig Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * bjeram    2003-03-06  added #include <ace/Log_Record.h> (needed by ACE/TAO x.3)
 * bjeram    2002-04-10  added setStdio
 * msekoran  2001-12-17  added CL failure detection, logging to syslog, file, ...
 * bjeram    2001-09-13  added logXML(...) method
 * bjeram    2001-08     added ACS_LOG_TIME macro
 * msekoran  2001-07-12  renamed m_data and m_flags variables in LoggingTSSStorage class
 * msekoran  2001-06-08  Implementation according new specifications
 * almamgr   2000-12-03  Removed static and changed to char* from filename and oldfilename
 * almamgr   2000-12-03  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ace/Log_Msg_Callback.h>

#include <deque>

#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include <orbsvcs/orbsvcs/CosNamingC.h>

#include <ace/Synch.h>
#include "logging_idlC.h"

#include "loggingExport.h"
#include "loggingLoggingTSSStorage.h"
#include "loggingLogThrottle.h"

#define DYNAMIC_LOG_LEVEL 1
#define CDB_REFRESH_LOG_LEVEL 2
#define ENV_LOG_LEVEL 3
#define CDB_LOG_LEVEL 4
#define DEFAULT_LOG_LEVEL 5
#define NOT_DEFINED_LOG_LEVEL 6

/// maximum length for addData value. It means 1023 characters + \0
#define ADD_DATA_VALUE_MAX 1024
/// maximum length for addData name. It means 2055 characters + \0
#define ADD_DATA_NAME_MAX 256

/// If OR-ed with log entries� flags, the runtime context (host name, process name,
/// thread name, context, stack ID and stack level) will also be output.
#define LM_RUNTIME_CONTEXT 0x00000200

/// If OR-ed with log entries� flags, the source code information (file name,
/// line number) will also be output.
#define LM_SOURCE_INFO 0x00000100

/// This profides in one macro both LM_RUNTIME_CONTEXT and LM_SOURCE_INFO
#define LM_FULL_INFO (LM_RUNTIME_CONTEXT | LM_SOURCE_INFO)

/**
 * The Log Message Callback.
 * All ACE logging requests (ACE_DEBUG, ACE_ERROR, or ACS_LOG calls) are to be redirected through this LoggingProxy.
 * One of the first things what an ACS application should do is to initialize LoggingProxy.
 * An example of initialization:
 * <pre>
 * // initialize ACE logger instance
 * ACE_TCHAR hostname[33];
 * ACE_OS::hostname (hostname, sizeof(hostname));
 * ACE_Log_Msg::instance()->local_host(hostname);
 *
 * LoggingProxy * logger = new LoggingProxy(cacheSize, minCachePriority, maxCachePriority, telecomLogger.in(), namingContext.in());
 * if (logger)
 *  {
 *     LoggingProxy::init(logger);
 *     LoggingProxy::ProcessName(argv[0]);
 *     LoggingProxy::ThreadName("main");
 *  }
 * </pre>
 *
 * WARNING:
 * When LoggingProxy is configured to log to the centralized logger, this means remote logging using CORBA,
 * user of the LoggingProxy is obligated to disable remote logging before shutting down CORBA,
 * i.e. calling CORBA::ORB::shutdown() or CORBA::ORB::destroy() metods.
 * This is simply done by setting centralized logger refernce to nil. An example:
 * if (logger)
 *  {
 *    logger->flush();
 *    logger->setCentralizedLogger(Logging::AcsLogService::_nil());
 *  }
 *
 * LoggingProxy is also configurable via next enivronment variables:
 * <OL>
 * <LI>ACS_LOG_STDOUT - variable which controls STDOUT output of logs (default is LM_INFO - 4). All priorities with >= priority are outputed. For instance, to have all logs outputted set variable to 0.</LI>
 * <LI>ACS_LOG_SYSLOG - if variable is set, syslog is used to save logging cache (in case of unavailability of centralized logger). The syntax is [facility@]hostname[:port] (default facility is LOG_USER and default port is 514). To take advantage of local syslog calls (on systems supporting it), use "localhost" as hostname. If variable is not set, local file is used to save logs.</LI>
 * <LI>ACS_LOG_FILE - the name of the file where logger cache is saved (default is /tmp/acs_local_log).</LI>
 * </OL>
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: loggingLoggingProxy.h,v 1.42 2012/01/20 22:07:44 tstaig Exp $"
 */
class logging_EXPORT LoggingProxy : public ACE_Log_Msg_Callback
{

  public:
    /// Receives all log entries submited
    /// within the process. Thread safe!
    void log(ACE_Log_Record &log_record);

    /// Specifies the log entry type, if the output representation is different
    /// than the one implied with ACE�s log entry type. Applies for the next
    /// log entry only. Pointer to the string must be stored in the thread-specific
    /// storage!
    static void LogEntryType(const ACE_TCHAR *szType);

    /// Specifies the name of the routine (function) where the following log entry
    /// will be generated. Pointer to the string must be stored in the
    /// thread-specific storage!
    static void Routine(const ACE_TCHAR *szRoutine);

    /// Specifies the name of the file where the following log entry
    /// will be generated. Pointer to the string must be stored in the
    /// thread-specific storage!
    static void File(const ACE_TCHAR *fileName);

    /// Specifies the name of the file where the following log entry
    /// will be generated. Pointer to the string must be stored in the
    /// thread-specific storage!
    static void Line(long lineNumber);

    /// Set the flags that will apply to the log entry that will be submitted next.
    /// Flags must be stored in thread-specific storage! Flags are obtained by OR-ing
    /// appropriate LM_* values above. If priority is 0, the default priority
    /// associated with ACE's log entry type (LM_INFO, LM_ERROR, ...) is implied.
    static void Flags(unsigned int uiFlags);

    /// Specifies the name of the thread. Pointer to the name must be stored in
    /// the thread-specific storage!
    static void ThreadName(const ACE_TCHAR *szName);

    /// Returns the name of the thread.
    static const ACE_TCHAR *ThreadName();

    /// Specifies the name of the process. Must be stored in a process-wide global
    /// variable!
    static void ProcessName(const ACE_TCHAR *szName);

    /// Returns the name of the process.
    static const ACE_TCHAR *ProcessName();

    /// Reset the list of custom attributes. The attributes are applicable to the
    /// next log entry only.
    static void ResetAttributes();

    /// Add an attribute to the list of next log entries� attributes.
    static void AddAttribute(const ACE_TCHAR *szName, const ACE_TCHAR *szValue);

    /// Specify the LogId attribute of the log entry that follows. Can be 0 (default)
    /// in which case no LogId attribute is output.
    static void LogId(const ACE_TCHAR *szName);

    /// Specify the URI attribute of the log entry that follows. Can be 0 (default)
    /// in which case no URI attribute is output.
    static void URI(const ACE_TCHAR *szName);

    /// Specifies the stack ID of the current logical thread. Pointer to the name
    /// must be stored in the thread-specific storage! Can be set to NULL if
    /// the logical thread ID is unknown.
    static void StackId(const ACE_TCHAR *szId);

    /// Returns the the logical thread ID. Must have been set previously using
    /// StackId.
    static const ACE_TCHAR *StackId();

    /// Private flags (to be used only internally).
    static void PrivateFlags(int privateFlags);

    /// Private flags (to be used only internally).
    static const int PrivateFlags();

    static void LogLevelLocalType(int logLevelLocalType);

    static const int LogLevelLocalType();

    static void LogLevelRemoteType(int logLevelRemoteType);

    static const int LogLevelRemoteType();

    /// Set the stack level in the current logical thread. The value must be stored
    /// in the thread-specific storage!
    static void StackLevel(int nLevel);

    /// Retrieve the stack level in the current logical thread.
    static int StackLevel();

    /// Set the context in which the code is operating. Pointer to the name must
    /// be stored in the thread-specific storage!
    static void Context(const ACE_TCHAR *szName);

    /// Retrieve the context in which the code is operating.
    static const ACE_TCHAR *Context();

    /// Set the name of the object which is publishing the log.
    /// Pointer to the name must
    /// be stored in the thread-specific storage!
    static void SourceObject(const ACE_TCHAR *soName);

    /// Return the name of the source object set by SourceObject
    /// The name is global per thread (stored in thread-specific storage TSS)
    static const ACE_TCHAR * SourceObject();

    /// Set the audience intended for the log message.
    static void audience(const ACE_TCHAR *aud);

    /// Return the audience.
    /// The name is global per thread (stored in thread-specific storage TSS)
    static const ACE_TCHAR * audience();

    /// Set the array where the log message was generated.
    static void array(const ACE_TCHAR *aud);

    /// Return the array.
    /// The name is global per thread (stored in thread-specific storage TSS)
    static const ACE_TCHAR * array();

    /// Set the antenna where the log message was generated.
    static void antenna(const ACE_TCHAR *aud);

    /// Return the antenna.
    /// The name is global per thread (stored in thread-specific storage TSS)
    static const ACE_TCHAR * antenna();

    /// Set the host where the log message was generated.
    static void host(const ACE_TCHAR *aud);

    /// Return the host
    /// The name is global per thread (stored in thread-specific storage TSS)
    static const ACE_TCHAR * host();

    /// Supply data with the log entry that follows.
    /// The maximum lenghth for AddData value is ADD_DATA_VALUE_MAX (255+\0). If it is too long it will be truncated.
    static void AddData(const ACE_TCHAR *szName, const ACE_TCHAR *szFormat, ...);

    static ACE_TSS<LoggingTSSStorage> *getTSS();

  public:

    /// Constructor
    /// WARNING: LoggingProxy MUST be created before calling any of its static methods!
    LoggingProxy(const unsigned long cacheSize,
		 const unsigned long minCachePriority,
		 const unsigned long maxCachePriority,
		 Logging::AcsLogService_ptr centralizedLogger = Logging::AcsLogService::_nil(),
		 CosNaming::NamingContext_ptr namingContext = CosNaming::NamingContext::_nil(),
		 const unsigned int autoFlushTimeoutSec = 5,
		 const int maxLogsPerSecond = -1);

    /// Destructor
    ~LoggingProxy();

    /// Set Centralized Logger.
    void setCentralizedLogger(Logging::AcsLogService_ptr centralizedLogger);

    /// Set Naming Context.
    void setNamingContext(CosNaming::NamingContext_ptr namingContext)
	{
	    m_namingContext = CosNaming::NamingContext::_duplicate(namingContext);
	}

    /// Forces logger to flush cache. Thread safe!
    void flush();

    /// Sets ACS_LOG_STDOUT runtime
    void setStdio(int stdio){ m_envStdioPriority = stdio; }

    /// ACE log type int-to-string mapping
    //static ACE_TCHAR* m_LogEntryTypeName[];
    //static ACSLoggingLog::LogType m_LogBinEntryTypeName[];

    /// initializes ACE_LOG_MSG (must be done per each thread)
    static void init(LoggingProxy *loggingProxy);

    /// returns ACE_LOG_MSG to default state
    static void done();
    static std::string BinToXml(ACSLoggingLog::LogBinaryRecord* record);

    /// Returns priority of the record (default or overriden).
    /// It returns ACS priority which is ACE priority increased by 1.
    /// in the past ACE and ACS priorities were aligned, due a bug in ACS's log2!
    static unsigned long getPriority(ACE_Log_Record &log_record);

    /// Checks xml if it is XML and "DTD" string and send it to centralized logger. Thread safe!
    void logXML(const ACE_TCHAR *xml, bool cache=true);

    /// Returns true if loggin is initialized otherwise false
    static bool isInit()
	{
	    return initialized;
	}

   /// Returns true if logging is initialized in this thread
    static bool isInitThread();

    /// For testing only (not tread-safe)
    int getCacheLogCount() const { if(!m_logBin) return m_cache.size(); else return m_bin_cache.size();}

  protected:
	 /** If the environment variable LOG_SERVICE_USE_EXTENSIONS is set the new log way will be used*/
	 char *oldLog;

  private:
    /// Get time in ISO8601 format in UTC (eg. 2000-12-31T08:12:21.322) - 24 chars (incl. \0)
    static void formatISO8601inUTC(const ACE_Time_Value &timestamp, ACE_TCHAR str[]);

    /// Method to be called when CORBA invocation to the centralized logger failed.
    void failedToSend();

    /// Method to be called when CORBA invocation to the centralized logger was successful.
    void successfullySent();

    /// Tries to reconnect to the centralized logger (after some amount of time) and return <true> if successful.
    bool reconnectToLogger();

    /// Send local cache file to the centralized logger (Telecom log service)
    void sendCache();

    /// Send local cache file to the centralized logger (Telecom log service)
    void sendCacheInternal();

    /// Send given record to the centralized logger (Telecom log service)
    /// Return: true if successful, false on failure
    bool sendRecord(CORBA::Any &record);
    
    /// Send given record to the centralized logger (Telecom log service)
    /// Return: true if successful, false on failure
    bool sendRecord(const Logging::XmlLogRecordSeq &reclist);

    void sendXmlLogs(ACE_Log_Record &log_record,  const ACE_TCHAR * timestamp, const ACE_TCHAR * entryType);
    void sendBinLogs(ACE_Log_Record &log_record,  const ACE_TCHAR * timestamp, const ACE_TCHAR * entryType);

    ///
    /// The number of log entries that can be kept in the local cache.
    /// When this number is reached, all log entries are transferred to the centralized
    /// logging. If network connection is not available, the local cache continues to grow, and
    /// every submitting of a log entry will attempt to flush the cache to the centralized logging.
    ///
    unsigned long m_cacheSize;

    ///
    /// Log entries whose priority is below (smaller than) the one specified with this property will be ignored
    /// (neither cached nor submitted to the centralized logging). In release version of the system,
    /// this will be set to LM_INFO (3), ignoring LM_TRACE and LM_DEBUG log entries. Debug version of the
    /// system will set this to LM_DEBUG (2). During development, it will be set to LM_TRACE (1).
    ///
    unsigned long m_minCachePriority;

    ///
    /// Log entries whose priority exceeds (is greater than) the one specified with this property
    /// will be directly transmitted to the centralized logging, bypassing the local cache.
    /// If this is set to MinCachePriority 1, the local cache feature is disabled.
    ///
    unsigned long m_maxCachePriority;

    ///
    /// Timeout in seconds to auto-flush (send cache).
    ///
    unsigned int m_autoFlushTimeoutSec;

    ///
    /// Throttling, max Log/second allowed to be sent to the logging system.
    /// -1 turn off the throttling
    ///
    int m_maxLogsPerSecond;
    logging::LogThrottle *logThrottle;
    ///
    /// Reference to the persistent object which implements the Telecom Log
    /// Service�s Log interface, in particular the write_records method.
    ///
    Logging::AcsLogService_var m_logger;
    bool m_noLogger;

    ///
    /// Reference to the naming service.
    /// Used to get new reference of the Centralized Logger ("Log") in reconnectToLogger() method.
    ///
    CosNaming::NamingContext_var m_namingContext;

    /// Failure counter for centralized logger (CL).
    /// If the failure counter reaches the limit, CL is disabled.
    int m_failureCount;

    /// Failure count limit.
    static int logging_EXPORT m_failureLimit;

    /// The time CL was disabled.
    /// LoggingProxy will try to reconnect after some (defined) time.
    ACE_Time_Value m_disconnectionTime;

    /// Reconnection retry (minimum time in sec)
    static int logging_EXPORT m_minReconnectionTime;

    ///
    /// Local cache
    ///
    typedef std::deque<ACE_CString> LogDeque;
    LogDeque m_cache;


    typedef std::deque<ACSLoggingLog::LogBinaryRecord *> LogBinDeque;
    //typedef std::deque<ACSLoggingLog::LogBinaryRecord > LogBinDeque;
    LogBinDeque m_bin_cache;

   // ACSLoggingLog::LogBinaryRecord * m_binaryRecord;

    /// Cache state
    bool m_cacheDisabled;

    ///
    /// Process
    ///
    static char /*ACE_CString*/ logging_EXPORT m_process[256];

    ///
    /// Thread mutex to make log() method thread-safe
    ///
    ACE_Recursive_Thread_Mutex m_mutex;

    bool m_logBin;

    ///
    /// Thread specific storage
    ///
    static ACE_TSS<LoggingTSSStorage> * logging_EXPORT tss;
    static int logging_EXPORT instances;

    /// Local output file, if Centralized Logger is unavailable (set by ACE_LOG_FILE env. var)
    ACE_CString m_filename;

    /// Switch to inform client only once about logging to the file
    bool m_alreadyInformed;

    /// Priority of logs to be sent to STDIO (set by ACE_LOG_STDIO env. var.):
    /// # < 0 - log only LM_INFO (4) and higher - default
    /// # otherwise all >= log all
    int m_envStdioPriority;
    int m_envCentralizePriority;

    /// Syslog setting (set by ACE_LOG_SYSLOG env. var.):
    /// not defined - local file cache is used
    /// 'localhost' - local syslog is used (if available)
    /// hostname - remote syslog is used (by default port 514/udp is used).
    ACE_CString m_syslog;

    /// Initialization flag.
    static bool logging_EXPORT initialized;

    static ACE_CString logging_EXPORT sourceObject_m;

    /// Set /clear flag counter - number of timest that
    /// global flags were set cleared
    static unsigned int logging_EXPORT setClrCount_m;

  /// The mutual exclusion mechanism which is required to use the <m_workWorkCond>.
  ACE_SYNCH_MUTEX m_doWorkMutex;

  /// Work condition
  ACE_SYNCH_CONDITION m_doWorkCond;

  /// Sending pending...
  volatile bool m_sendingPending;

  /// Thread entry point (thread worker)
  virtual int svc();

  /// Static thread worker (calls svc)
  static void* worker(void*);

  /// Thread creation flag.
  volatile bool m_threadCreated;
  /// Thread start barrier
  //ACE_Barrier m_threadStart;
  /// Thread shutdown barrier
  ACE_Barrier m_threadShutdown;
  volatile bool m_shutdown;
};

#endif /*!logging_logging_proxy_H*/

