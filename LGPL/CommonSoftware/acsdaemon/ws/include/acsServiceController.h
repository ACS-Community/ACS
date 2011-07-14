#ifndef _ACS_SERVICE_CONTROLLER_H_
#define _ACS_SERVICE_CONTROLLER_H_

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
* "@(#) $Id: acsServiceController.h,v 1.10 2011/07/14 06:53:42 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-10-21 created
*/

#include "acsRequest.h"
#include <acsThreadManager.h>
#include <map>
#include <string>
#include <AcsAlarmSystemC.h>

const ACE_Time_Value TIME_PERIOD(15);

class DetailedServiceStateProvider {
  public:
    virtual ~DetailedServiceStateProvider() {};
    virtual acsdaemon::ServiceState getDetailedServiceState(ACSServiceRequestDescription *desc, CORBA::Object_ptr obj) = 0;
};

class ControllerThread : public ACS::Thread {
  private:
    ACSDaemonContext *context;
    ACE_Thread_Mutex *m_mutex;
    ACE_Condition<ACE_Thread_Mutex> *m_wait;
    volatile bool running;
  public:
    ControllerThread(const ACE_CString &name,
           const ACS::TimeInterval& responseTime = ThreadBase::defaultResponseTime, 
           const ACS::TimeInterval& sleepTime = ThreadBase::defaultSleepTime);
    ~ControllerThread();
    void setContext(ACSDaemonContext *icontext) { context = icontext; }
    void onStart();
    void stop();
    void exit();
    void runLoop() ACE_THROW_SPEC ((CORBA::SystemException, ::ACSErrTypeCommon::BadParameterEx));
};

class ControlledServiceRequest;

class ServiceController {
  protected:
    ACSDaemonContext *context;
    bool autorestart;
    ACE_Thread_Mutex *m_mutex;
    volatile acsdaemon::ServiceState state;
    volatile bool active;
    Request *startreq, *stopreq; // last request in queue (either start or stop, not both)

    friend class ControlledServiceRequest;
    // called once the request returned by createControlledServiceRequest begins stopping the service
    void stopping();
    // called once the request returned by createControlledServiceRequest is complete
    void requestComplete(Request *request);
    virtual bool setState(acsdaemon::ServiceState istate);
  protected:
    virtual ControlledServiceRequest *createControlledServiceRequest(ACSServiceRequestType itype, acsdaemon::DaemonCallback_ptr callback = NULL) = 0;
    virtual acsdaemon::ServiceState getActualState() = 0;
    virtual void fireAlarm(acsdaemon::ServiceState state) = 0;
  public:
    virtual ACE_CString getServiceName() = 0;
    ServiceController(ACSDaemonContext *icontext, bool iautorestart);
    virtual ~ServiceController();
    ACSDaemonContext *getContext() { return context; }
    void restart();
    bool start(acsdaemon::DaemonCallback_ptr callback = NULL) ACE_THROW_SPEC ((acsdaemonErrType::ServiceAlreadyRunningEx));
    void stop(acsdaemon::DaemonCallback_ptr callback = NULL) ACE_THROW_SPEC ((acsdaemonErrType::ServiceNotRunningEx));
    acsdaemon::ServiceState getLastState() { return state; }
};

class ControlledServiceRequest : public Request {
  private:
    ServiceController *controller;
    Request *request;
    bool delreq;
    bool isstopping;
  protected:
    void abort();
    bool execute();
  public:
    ControlledServiceRequest(ServiceController *icontroller, Request *irequest, bool iisstopping);
    ~ControlledServiceRequest();
};

class ImpController;

class ImpRequest : public Request {
  private:
    ImpController *controller;
    ACE_CString command;
  protected:
    void abort() {}
    bool execute();
  public:
    ImpRequest(ImpController *icontroller, ACSServiceRequestType itype, ACSServiceType iservice);
};

class ImpController : public ServiceController {
  private:
    ACSServiceType service;
    ACE_CString corbaloc;
  protected:
    ControlledServiceRequest *createControlledServiceRequest(ACSServiceRequestType itype, acsdaemon::DaemonCallback_ptr callback = NULL);
    acsdaemon::ServiceState getActualState();
    void fireAlarm(acsdaemon::ServiceState state) {}
  public:
    ImpController(ACSDaemonContext *icontext, ACSServiceType iservice, bool iautostart = true);
    ACSServiceType getACSService() { return service; }
    virtual ACE_CString getServiceName();
    void setManagerReference(const short instance_number, const char * ref);
};

class ACSServiceController : public ServiceController {
  private:
    ACSServiceRequestDescription *desc;
    ACE_CString corbaloc;
    bool alarmSystemInitialized;
    ::alarmsystem::AlarmService_var alarmService;
 protected:
    ControlledServiceRequest *createControlledServiceRequest(ACSServiceRequestType itype, acsdaemon::DaemonCallback_ptr callback = NULL);
    acsdaemon::ServiceState getActualState();
    virtual bool setState(acsdaemon::ServiceState istate);
    void fireAlarm(acsdaemon::ServiceState state);
  public:
    ACSServiceController(ACSDaemonContext *icontext, ACSServiceRequestDescription *idesc, bool iautostart);
    virtual ACE_CString getServiceName();
    ~ACSServiceController();
};

class ACSDaemonContext {
  private:
    CORBA::ORB_ptr orb;
    ACS::ThreadManager tm;
    RequestProcessorThread *reqproc;
    ControllerThread *ctrl;
    ACE_Thread_Mutex *m_mutex;
    ServiceController **impcontrollers;
    ServiceController **acsservicecontrollers;
    std::map<std::string, ServiceController **> acsservicecontrollersmap;
    ServiceController *getImpController(ACSServiceType service);
    ServiceController *getACSServiceController(ACSServiceRequestDescription *desc);
    std::map<short, std::string> managerReferences;
    DetailedServiceStateProvider *detailedServiceStateProvider;
    void setImpControllersManagerReference(const short instance_number, const char * ref);
  public:
    ACSDaemonContext(std::string name, DetailedServiceStateProvider *dssp = NULL);
    ~ACSDaemonContext();
    void initialize(CORBA::ORB_ptr iorb);
    void dispose(CORBA::ORB_ptr iorb);
    void processRequest(ACSServiceRequestTarget target, ACSServiceRequestType type, ACSServiceRequestDescription *desc, acsdaemon::DaemonCallback_ptr callback = NULL) ACE_THROW_SPEC ((acsdaemonErrType::ServiceAlreadyRunningEx, acsdaemonErrType::ServiceNotRunningEx));
    RequestProcessorThread *getRequestProcessor() { return reqproc; }
    CORBA::ORB_ptr getORB() { return orb; }
    void checkControllers();
    acsdaemon::ServiceState getACSServiceState(int instance_number, const char *name = NULL);
    acsdaemon::ServiceState getDetailedServiceState(ACSServiceRequestDescription *desc, CORBA::Object_ptr obj) const {
	return detailedServiceStateProvider ? detailedServiceStateProvider->getDetailedServiceState(desc, obj) : acsdaemon::RUNNING; };
    void setManagerReference(const short instance_number, const char * ref) { managerReferences[instance_number] = ref; setImpControllersManagerReference(instance_number, ref); };
    const char * getManagerReference(const short instance_number) { if (managerReferences.find(instance_number) != managerReferences.end()) return managerReferences[instance_number].c_str(); else return NULL; }
};


#endif
