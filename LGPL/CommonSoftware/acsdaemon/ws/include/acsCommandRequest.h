#ifndef _ACS_COMMAND_REQUEST_H_
#define _ACS_COMMAND_REQUEST_H_

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
* "@(#) $Id: acsCommandRequest.h,v 1.4 2008/10/27 21:11:23 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsdaemonS.h"
#include <acsThread.h>
#include <queue>
#include <memory>
#include <acsQoS.h>

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

enum ACSService {
    NAMING_SERVICE = 0,
    NOTIFICATION_SERVICE,
    CDB,
    MANAGER,
    ACS_LOG,
    LOGGING_SERVICE,
    INTERFACE_REPOSITORY,
    UNKNOWN
};

static const char *acsServiceNames[] = {
    "naming_service",
    "notification_service",
    "cdb",
    "manager",
    "acs_log",
    "logging_service",
    "interface_repository",
    NULL
};

class Request
{
  public:
    virtual ~Request() {};
    virtual void abort() = 0;
    virtual void execute() = 0;
};

class RequestProcessorThread : public ACS::Thread
{
  public:
    RequestProcessorThread(const ACE_CString &name,
	       const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	       const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime, false, THR_DETACHED) 
	{
	    ACS_TRACE("RequestProcessorThread::RequestProcessorThread"); 
	    m_mutex = new ACE_Thread_Mutex();
	    m_wait = new ACE_Condition<ACE_Thread_Mutex>(*m_mutex);
	}
    
    ~RequestProcessorThread() 
	{ 
	    ACS_TRACE("RequestProcessorThread::~RequestProcessorThread"); 
	}
    
    void onStart();

    void stop();

    void exit() { ACS::Thread::exit(); stop(); }

    void runLoop()
        ACE_THROW_SPEC ((
            CORBA::SystemException,
	    ::ACSErrTypeCommon::BadParameterEx
        ));

    void addRequest(Request* r);

  private:
    ACE_Thread_Mutex *m_mutex;
    ACE_Condition<ACE_Thread_Mutex> *m_wait;
    std::queue<Request*> pending;
    volatile bool running;
};

char *prepareCommand(const char *command, short instance_number, bool wait, const char *name, const char *additional_command_line, bool log);

void execCommand(char *commandline, acsdaemon::DaemonCallback_ptr callback, RequestProcessorThread *reqproc);

class LocalCommandRequest : public Request
{
    acsdaemon::DaemonCallback_var callback;
    char *cmd;
  public:
    LocalCommandRequest(acsdaemon::DaemonCallback_ptr icallback, char *icmd) : cmd(icmd) {
        callback = icallback == NULL ? NULL : acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, icallback);
    }
    ~LocalCommandRequest() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING LocalCommandRequest '%s'!", cmd));
        delete[] cmd; // cmd was created by ACE_CString::rep()
    }
    void abort();
    void execute();
};

class RequestChainBuilder;
class DaemonRequestCallback;
class DaemonRequest;

class CommandRequestDescription
{
    ACSService service;
    char *host, *name, *domain, *xmlcdbdir;
    bool loadir, wait, recovery;
    int instance_number;
    bool start;
    acsdaemon::DaemonSequenceCallback_ptr callback;
    CommandRequestDescription(ACSService iservice, char *ihost, char *iname = NULL) : service(iservice), host(ihost), name(iname), domain(NULL), xmlcdbdir(NULL) {}
    CommandRequestDescription(ACSService iservice, char *ihost, char *idomain, bool irecovery) : service(iservice), host(ihost), domain(idomain), recovery(irecovery) {}
    CommandRequestDescription(ACSService iservice, char *ihost, bool irecovery, char *ixmlcdbdir) : service(iservice), host(ihost), xmlcdbdir(ixmlcdbdir), recovery(irecovery) {}
    CommandRequestDescription(ACSService iservice, char *ihost, bool iloadir, bool iwait) : service(iservice), host(ihost), loadir(iloadir), wait(iwait) {}
    friend class DaemonRequest;
    friend class DaemonRequestCallback;
  public:
    CommandRequestDescription(const char *iservice, const char **atts, int iinstance_number, bool istart, acsdaemon::DaemonSequenceCallback_ptr icallback) : host(NULL), name(NULL), domain(NULL), instance_number(iinstance_number), start(istart), callback(icallback) {
        int i = 0;
        while (acsServiceNames[i] != NULL) {
            if (strcasecmp(iservice, acsServiceNames[i]) == 0) break;
            i++;
        }
        service = (ACSService)i;
        if (service == UNKNOWN) {
            ACS_SHORT_LOG((LM_ERROR, "Unknown service '%s'!", iservice));
        }
        i = 0;
        while (atts[i] != NULL) {
            if (strcasecmp(atts[i], "host") == 0) host = strdup(atts[i+1]);
            else if (strcasecmp(atts[i], "name") == 0) name = strdup(atts[i+1]);
            else if (strcasecmp(atts[i], "domain") == 0) domain = strdup(atts[i+1]);
            else if (strcasecmp(atts[i], "xml_cdb_dir") == 0) xmlcdbdir = strdup(atts[i+1]);
            else if (strcasecmp(atts[i], "load") == 0) loadir = strcasecmp(atts[i+1], "true") == 0;
            else if (strcasecmp(atts[i], "wait_load") == 0) wait = strcasecmp(atts[i+1], "true") == 0;
            else if (strcasecmp(atts[i], "recovery") == 0) recovery = strcasecmp(atts[i+1], "true") == 0;
            i += 2;
        }
    };
    ~CommandRequestDescription() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING CommandRequestDescription '%s'!", acsServiceNames[service]));
        if (host != NULL) free(host);
        if (name != NULL) free(name);
        if (domain != NULL) free(domain);
        if (xmlcdbdir != NULL) free(xmlcdbdir);
    }
    const char *getServiceName() { return acsServiceNames[service]; }
    static CommandRequestDescription *createNamingServiceDescription(char *ihost) { return new CommandRequestDescription(NAMING_SERVICE, ihost); }
};

class DaemonRequest : public Request
{
    RequestChainBuilder *builder;
    ACSService service;
    auto_ptr<CommandRequestDescription> description;
    DaemonRequest *next;
    friend class RequestChainBuilder;
  public:
    DaemonRequest(RequestChainBuilder *ibuilder, CommandRequestDescription *idescription) : builder(ibuilder), service(idescription->service), description(idescription) {}
    ~DaemonRequest() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING DaemonRequest '%s'!", acsServiceNames[service]));
    }
    void abort();
    void execute();
};

class RequestChainBuilder
{
    CORBA::ORB_ptr orb;
    bool start; // true to start and false to stop
    bool reuse_services;
    RequestProcessorThread *reqproc;
    acsdaemon::DaemonSequenceCallback_var callback;
    DaemonRequest *head, *tail;
    short instance_number;
//    friend class DaemonRequest;
//    friend class DaemonRequestCallback;
  public:
    RequestChainBuilder(CORBA::ORB_ptr iorb, bool istart, RequestProcessorThread *ireqproc, bool ireuse_services, acsdaemon::DaemonSequenceCallback_ptr icallback) : orb(iorb), start(istart), reuse_services(ireuse_services), reqproc(ireqproc), head(NULL), tail(NULL), instance_number(-1) {
    callback = icallback == NULL ? NULL : acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, icallback);
}
    virtual ~RequestChainBuilder() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING RequestChainBuilder!"));
    }
    void addRequest(const char *name, const char **atts);
    void startProcessing();
};

class DaemonRequestCallback : public POA_acsdaemon::DaemonCallback
{
    RequestChainBuilder *builder;
    DaemonRequest *next;
    auto_ptr<CommandRequestDescription> description;
    ::acsdaemon::DaemonCallback_var cobj;
  public:
    DaemonRequestCallback(RequestChainBuilder *ibuilder, DaemonRequest *inext, auto_ptr<CommandRequestDescription> idescription) : builder(ibuilder), next(inext), description(idescription), cobj(NULL) {}
    virtual ~DaemonRequestCallback() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING DaemonRequestCallback '%s'!", acsServiceNames[description.get()->service]));
    }
    ::acsdaemon::DaemonCallback_ptr ptr() {
        if (cobj == NULL)
            cobj = this->_this();
        return cobj.in();
    }
    void done (const ::ACSErr::Completion & comp);
    void working (const ::ACSErr::Completion & comp);
};



#endif
