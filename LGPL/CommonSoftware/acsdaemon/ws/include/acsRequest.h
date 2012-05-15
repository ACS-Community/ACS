#ifndef _ACS_REQUEST_H_
#define _ACS_REQUEST_H_

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
* "@(#) $Id: acsRequest.h,v 1.15 2012/05/15 09:06:34 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsdaemonS.h"
#include <acsThread.h>
#include <acsutilPorts.h>
#include <queue>
#include <memory>

#include <ace/Task.h>
#include <ace/Activation_Queue.h>
#include <ace/Method_Request.h>
#include <ace/Guard_T.h>

#include <logging.h>

#include <set>

// callback call timeout
#define CORBA_TIMEOUT 5000

/*** exit codes of startup/shutdown scripts ***/
// all fine
#define EC_OK 0
// can't create requested acs instance: already exists
#define EC_CANNOTCREATE 40
// can't use requested acs instance: doesn't exist or is owned by somebody else
#define EC_CANNOTUSE 41
// general failure, no details known
#define EC_FAILURE 42
// command line arguments wrong or missing
#define EC_BADARGS 43
// can't run requested servant: no port available
#define EC_NOPORT 44
// can't run requested servant: launch attempt timed out
#define EC_TIMEOUT 45

// start-up (dependency) order
enum ACSServiceType {
    NAMING_SERVICE = 0,
    INTERFACE_REPOSITORY,
    CDB,
    RDB_CDB,
    NOTIFICATION_SERVICE,
    LOGGING_SERVICE,
    ACS_LOG_SERVICE,
    ALARM_SERVICE,
    MANAGER,
    UNKNOWN
};

struct ACSService {
    const char *xmltag;
    const char *script;
    const char *impname;   // Name of services handler (used for logging purposes)
    const char *imptype;   // CORBA-type for this services handler
    const char *impport;
    const char *impexec;
    const char *svccorbaurl;
    std::string (*svcport)(int);
    std::string (*namedsvcport)(int, const char *);
    bool autorestart;      // Should ACS service automatically restart
    bool async;            // Should service be started asynchronously
    const ACSServiceType* depententService;     // What async started service to wait for
};

#define ACS_SERVICE_TYPES UNKNOWN
#define ACS_SERVICE_INSTANCES 11

const ACSServiceType noDependency[] = { UNKNOWN };
const ACSServiceType namingServiceDependency[] = { NAMING_SERVICE, UNKNOWN };
const ACSServiceType cdbDependency[] = { CDB, RDB_CDB, UNKNOWN };


const ACSService acsServices[] = {
		{
				"naming_service",
				"acsNamingService",
				"Naming Service Imp",
				"NamingServiceImp",
				"2981",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonNamingServiceImp",
				"corbaloc::%s:%s/NameService",
				&ACSPorts::getNamingServicePort,
				NULL,
				false,
				false,
				noDependency
		}, {
				"interface_repository",
				"acsInterfaceRepository",
				"Interface Repository Imp",
				"InterfaceRepositoryImp",
				"2987",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonInterfaceRepositoryImp",
				"corbaloc::%s:%s/InterfaceRepository",
				&ACSPorts::getIRPort,
				NULL,
				false,
				true,
				namingServiceDependency
		}, {
				"cdb",
				"acsConfigurationDatabase",
				"CDB Imp",
				"ConfigurationDatabaseImp",
				"2983",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonConfigurationDatabaseImp",
				"corbaloc::%s:%s/CDB",
				&ACSPorts::getCDBPort,
				NULL,
				false,
				true,
				namingServiceDependency
		}, {
				"rdb_cdb",
				"acsRDBConfigurationDatabase",
				"CDB Imp",
				"ConfigurationDatabaseImp",
				"2983",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonConfigurationDatabaseImp",
				"corbaloc::%s:%s/CDB",
				&ACSPorts::getCDBPort,
				NULL,
				false,
				true,
				namingServiceDependency
		}, {
				"notification_service",
				"acsNotifyService",
				"Notification Service Imp",
				"NotificationServiceImp",
				"2982",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonNotificationServiceImp",
				"corbaloc::%s:%s/%s",
				NULL,
				&ACSPorts::getNotifyServicePort,
				true,
				false,
				namingServiceDependency
		}, {
				"logging_service",
				"acsLoggingService",
				"Logging Service Imp",
				"LoggingServiceImp",
				"2986",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonLoggingServiceImp",
				"corbaloc::%s:%s/Log",
				&ACSPorts::getLoggingServicePort,
				NULL,
				true,
				false,
				namingServiceDependency
		}, {
				"acs_log",
				"acsACSLogService",
				"ACS Log Service Imp",
				"ACSLogServiceImp",
				"2985",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonACSLogServiceImp",
				"corbaloc::%s:%s/ACSLogSvc",
				&ACSPorts::getLogPort,
				NULL,
				false,
				false,
				namingServiceDependency
		}, {
				"alarm_service",
				"acsAlarmService",
				"Alarm Service Imp",
				"AlarmServiceImp",
				"2988",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonAlarmServiceImp",
				"corbaloc::%s:%s/AcsAlarmService",
				&ACSPorts::getAlarmServicePort,
				NULL,
				false,
				true,
				cdbDependency
		}, {
				"manager",
				"acsManager",
				"Manager Imp",
				"ManagerImp",
				"2984",
				"acsutilBlock -t 15 -s -k -b \"Imp is up and running...\" acsdaemonManagerImp",
				"corbaloc::%s:%s/Manager",
				&ACSPorts::getManagerPort,
				NULL,
				false,
				false,
				cdbDependency
		}, { NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL, false, false, noDependency }
};

enum ACSServiceRequestType {
    START_SERVICE,
    STOP_SERVICE
};

enum ACSServiceRequestTarget {
    LOCAL,
    DAEMON,
    IMP
};

ACSServiceType acsServiceXMLTagToEnum(const char *service);

class RequestProcessorThread;

class Request {
  public:
    virtual ~Request() {};
    virtual void abort() = 0;
    virtual bool execute() = 0;
};

class RequestProcessorThread : public ACS::Thread {
  private:
    ACE_Thread_Mutex *m_mutex;
    ACE_Condition<ACE_Thread_Mutex> *m_wait;
    std::queue<Request*> pending;
    volatile bool running;
  public:
    RequestProcessorThread(const ACE_CString &name,
           const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
           const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime);
    ~RequestProcessorThread();
    void onStart();
    void stop();
    void exit();
    void runLoop() ACE_THROW_SPEC ((CORBA::SystemException, ::ACSErrTypeCommon::BadParameterEx));
    void process(Request* r);
};

template <class R> class RequestChainContext;

template <class R> class ChainedRequest : public Request {
  private:
    RequestChainContext<R> *context;
    friend class RequestChainContext<R>;
  protected:
    virtual void complete();
  public:
    ChainedRequest() : context(NULL) {}
    void process(RequestChainContext<R> *icontext);
    virtual bool isAsync() { return false; }
};

template <class R> class RequestChainContext {
  private:
    RequestProcessorThread *rpt;
    typedef std::deque<R*> Queue;
    Queue requests;
    R *curreq;
    bool inprocess;

    bool hasAsync;
    bool failed;
    int asyncToComplete;
    std::set<ACSServiceType> asyncStartInProgress;
    bool syncPending;

    ACE_Thread_Mutex mutex;

  protected:
    virtual bool requestDone(R *request) = 0;
    virtual void chainDone() = 0;
    virtual void chainAborted() = 0;
  public:
    RequestChainContext(RequestProcessorThread *irpt) : rpt(irpt), curreq(NULL), inprocess(false),
    	hasAsync(false), failed(false), asyncToComplete(0), asyncStartInProgress(), syncPending(false) {}
    virtual ~RequestChainContext() {
        while (!requests.empty()) {
            delete requests.front();
            requests.pop_front();
        }
    }
    RequestProcessorThread *getRequestProcessor() { return rpt; }
    void appendRequest(R *request) { requests.push_back(request); }
    void appendRequestOrdered(R *request) {
        ACSServiceType thisType = request->getDescription()->getACSService();
        typename Queue::iterator it = requests.begin();
        while (it != requests.end() && (*it)->getDescription()->getACSService() <= thisType)
        	it++;
        requests.insert(it, request);
    }
    void prependRequest(R *request) { requests.push_front(request); }
    void prependRequestOrdered(R *request) {
        ACSServiceType thisType = request->getDescription()->getACSService();
        typename Queue::iterator it = requests.begin();
        while (it != requests.end() && (*it)->getDescription()->getACSService() > thisType)
                it++;
        requests.insert(it, request);
    }
    void proceed(R *lastreq = NULL);
};

/*********************** ACS SERVICES SPECIFIC REQUESTS ***********************/

class ACSServiceRequestChainContext;
class ACSDaemonContext;

class ACSServiceRequestDescription {
  private:
    ACSServiceType service;
    int instance_number;
    const char *host, *name, *corbalocName, *domain, *cdbxmldir;
    bool loadir, wait, recovery, async;
    ACE_CString prepareCommand(ACSDaemonContext *context, ACSServiceRequestType request_type, bool log);
  public:
    ACSServiceRequestDescription(ACSServiceType iservice, int iinstance_number);
    ACSServiceRequestDescription(const ACSServiceRequestDescription &desc);
    ~ACSServiceRequestDescription();
    ACSErr::Completion_var executeLocal(ACSDaemonContext *context, ACSServiceRequestType request_type);
    ACSErr::Completion_var executeRemote(ACSDaemonContext *context, ACSServiceRequestType request_type, CORBA::ORB_ptr orb, acsdaemon::DaemonCallback_ptr cbptr, const char *corbaloc);
    void setFromXMLAttributes(const char **atts);
    void setName(const char *iname) { name = iname == NULL ? NULL : strdup(iname); }
    void setCorbalocName(const char *iname) { corbalocName = iname == NULL ? NULL : strdup(iname); }
    void setDomain(const char *idomain) { domain = idomain == NULL ? NULL : strdup(idomain); }
    void setLoadIR(bool iloadir) { loadir = iloadir; }
    void setWaitLoadIR(bool iwait) { wait = iwait; }
    void setRecovery(bool irecovery) { recovery = irecovery; }
    void setCdbXMLDir(const char *icdbxmldir) { cdbxmldir = icdbxmldir == NULL ? NULL : strdup(icdbxmldir); }
    int getInstanceNumber() { return instance_number; }
    const char *getName() { return name; }
    const char *getCorbalocName() { return corbalocName; } 
    const char *getHost() { return host == NULL ? ACSPorts::getIP() : host; }
    ACSServiceType getACSService() { return service; }
    const char *getACSServiceName() { return acsServices[service].xmltag; }
    bool isAsync() { return async; }
    const ACSServiceType* getDependentService() { return acsServices[service].depententService; }
};


class ACSServiceRequest : public ChainedRequest<ACSServiceRequest>, POA_acsdaemon::DaemonCallback {
  private:
    ACSDaemonContext *context;
    ACSServiceRequestTarget target;
    ACSServiceRequestType request_type;
    ACSServiceRequestDescription *desc;
    acsdaemon::DaemonCallback_var callback;
    const ACSErr::Completion *completion;
    acsdaemon::DaemonCallback_var cbvar;
    acsdaemon::DaemonCallback_ptr cbptr();
    void release();
  protected:
    void complete();
    void abort();
    bool execute();
  public:
    ACSServiceRequest(ACSDaemonContext *icontext, ACSServiceRequestTarget itarget, ACSServiceRequestType itype, ACSServiceRequestDescription *idesc, acsdaemon::DaemonCallback_ptr icallback = NULL);
    ~ACSServiceRequest();
    void done(const ::ACSErr::Completion &comp);
    void working(const ::ACSErr::Completion &comp);
    const ACSErr::Completion *getCompletion() { return completion; }
    bool isErrorFree() { return completion == NULL || completion->previousError.length() == 0; }
    ACSServiceRequestTarget getRequestTarget() { return target; }
    ACSServiceRequestType getRequestType() { return request_type; }
    ACSServiceRequestDescription *getDescription() { return desc; }
    const char *getACSServiceName() { return desc->getACSServiceName(); }
    int getInstanceNumber() { return desc->getInstanceNumber(); }
    const char *getHost() { return desc->getHost(); }
    virtual bool isAsync() { return desc->isAsync(); }
};

class ACSServiceRequestChainContext : public RequestChainContext<ACSServiceRequest> {
  private:
    ACSDaemonContext *context;
    ACSServiceRequestType request_type;
    bool reuse_services;
    acsdaemon::DaemonSequenceCallback_var callback;
    int instance_number;
  protected:
    bool requestDone(ACSServiceRequest *request);
    void chainDone();
    void chainAborted();
  public:
    ACSServiceRequestChainContext(ACSDaemonContext *icontext, ACSServiceRequestType itype, bool ireuse_services, acsdaemon::DaemonSequenceCallback_ptr icallback);
    ~ACSServiceRequestChainContext();
    void addRequest(const char *iservice, const char **atts);
    void startProcessing() { proceed(); }
};


class AsyncRequestThreadPool : public ACE_Task_Base, public Logging::Loggable
{
public:

	static void configure(const char* processName, LoggingProxy *log, int threads)
	{
		ACE_Guard<ACE_Thread_Mutex> guard(mutex_);
		process_name = processName;
		logger = log;
		conf_threads = threads;
	}

	// not thread-safe
	static AsyncRequestThreadPool* getInstance()
	{
		ACE_Guard<ACE_Thread_Mutex> guard(mutex_);
		if (!instance_)
			instance_ = new AsyncRequestThreadPool(conf_threads);
		return instance_;
	}

	static void destroy()
	{
		ACE_Guard<ACE_Thread_Mutex> guard(mutex_);
		if (instance_)
		{
			instance_->shutdown();
			delete instance_;
			instance_ = 0;
		}
	}

    virtual int svc (void);

    int enqueue (ACE_Method_Request *request);

    void shutdown();

private:

    AsyncRequestThreadPool (int n_threads = 1);

    static ACE_Thread_Mutex mutex_;
    static int conf_threads;
    static ACE_CString process_name;
    static LoggingProxy *logger;

    static AsyncRequestThreadPool* instance_;

    ACE_Activation_Queue activation_queue_;

	int m_threads;
};

#endif

