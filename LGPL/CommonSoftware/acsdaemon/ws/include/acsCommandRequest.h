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
* "@(#) $Id: acsCommandRequest.h,v 1.1 2008/08/29 13:58:28 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsdaemonS.h"
#include <acsThread.h>
#include <queue>
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

class Request
{
  public:
    virtual ~Request() {};
    virtual void abort() = 0;
    virtual void execute() = 0;
};

class LocalRequest : public Request
{
    acsdaemon::DaemonCallback_var callback;
    char *cmd;
  public:
    LocalRequest(acsdaemon::DaemonCallback_ptr icallback, char *icmd) : cmd(icmd) {
        callback = icallback == NULL ? NULL : acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, icallback);
    }
    ~LocalRequest() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING LocalRequest '%s'!", cmd));
        delete[] cmd; // cmd was created by ACE_CString::rep()
    }
    void abort();
    void execute();
};

class CommandProcessorThread : public ACS::Thread
{
  public:
    CommandProcessorThread(const ACE_CString &name,
	       const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
	       const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
	ACS::Thread(name, responseTime, sleepTime, false, THR_DETACHED) 
	{
	    ACS_TRACE("CommandProcessorThread::CommandProcessorThread"); 
	    m_mutex = new ACE_Thread_Mutex();
	    m_wait = new ACE_Condition<ACE_Thread_Mutex>(*m_mutex);
	}
    
    ~CommandProcessorThread() 
	{ 
	    ACS_TRACE("CommandProcessorThread::~CommandProcessorThread"); 
	}
    
    void onStart();

    void onStop();

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
    bool running;
};

class RequestChainBuilder;
class RemoteRequestDaemonCallback;

class RemoteRequest : public Request
{
    RequestChainBuilder *builder;
    char *service;
    char *host, *name, *domain;
    bool loadir, wait;
    RemoteRequest *next;
    friend class RequestChainBuilder;
    friend class RemoteRequestDaemonCallback;
  public:
    RemoteRequest(RequestChainBuilder *ibuilder, char *iservice) : builder(ibuilder), service(iservice), host(NULL), name(NULL), domain(NULL), next(NULL) {};
    ~RemoteRequest() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING RemoteRequest '%s'!", service));
        if (service != NULL) free(service);
        // host, name and domain get released by the callback
    }
    void abort();
    void execute();
};

class RequestChainBuilder
{
    CORBA::ORB_ptr orb;
    bool start; // true to start and false to stop
    bool reuse_services;
    CommandProcessorThread *cmdproc;
    acsdaemon::DaemonSequenceCallback_var callback;
    RemoteRequest *head, *tail;
    short instance_number;
    friend class RemoteRequest;
    friend class RemoteRequestDaemonCallback;
  public:
    RequestChainBuilder(CORBA::ORB_ptr iorb, bool istart, CommandProcessorThread *icmdproc, bool ireuse_services, acsdaemon::DaemonSequenceCallback_ptr icallback) : orb(iorb), start(istart), reuse_services(ireuse_services), cmdproc(icmdproc), head(NULL), tail(NULL), instance_number(-1) {
    callback = icallback == NULL ? NULL : acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, icallback);
}
    virtual ~RequestChainBuilder() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING RequestChainBuilder!"));
    }
    void addRequest(const char *name, const char **atts);
    void startProcessing();
};

class RemoteRequestDaemonCallback : public POA_acsdaemon::DaemonCallback
{
    RequestChainBuilder *builder;
    RemoteRequest *next;
    char *service;
    char *host, *name, *domain;
    ::acsdaemon::DaemonCallback_var cobj;
  public:
    RemoteRequestDaemonCallback(RequestChainBuilder *ibuilder, RemoteRequest *irequest) : builder(ibuilder), next(irequest->next), service(strdup(irequest->service)), host(irequest->host), name(irequest->name), domain(irequest->domain), cobj(NULL) {}
    virtual ~RemoteRequestDaemonCallback() {
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING RemoteRequestDaemonCallback '%s'!", service));
        if (service != NULL) free(service);
        if (host != NULL) free(host);
        if (name != NULL) free(name);
        if (domain != NULL) free(domain);
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
