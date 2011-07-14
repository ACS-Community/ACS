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
* "@$Id: acsServiceController.cpp,v 1.16 2011/07/14 06:53:42 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-08-12 created
*/

#include "acsServiceController.h"
#include <acsutilPorts.h>
#include <acsQoS.h>

#include <maciC.h>
#include <Properties.h>
#include <faultStateConstants.h>

#include "acsdaemonErrType.h"

/***************************** ControllerThread *******************************/

ControllerThread::ControllerThread(const ACE_CString &name, const ACS::TimeInterval& responseTime,  const ACS::TimeInterval& sleepTime) :
       ACS::Thread(name, responseTime, sleepTime, false, THR_DETACHED) {
    ACS_TRACE("ServiceControlThread::ServiceControlThread"); 
}

ControllerThread::~ControllerThread() { 
    ACS_TRACE("ServiceControlThread::~ServiceControlThread"); 
    if (m_mutex) delete m_wait;
    if (m_wait) delete m_mutex; 
}

void ControllerThread::onStart() {
    running = true;
    m_mutex = new ACE_Thread_Mutex();
    m_wait = new ACE_Condition<ACE_Thread_Mutex>(*m_mutex);
}

void ControllerThread::stop() {
    running = false;
    if (m_wait) m_wait->signal();
}

void ControllerThread::exit() {
    ACS::Thread::exit();
    stop();
}

void ControllerThread::runLoop() ACE_THROW_SPEC ((CORBA::SystemException, ::ACSErrTypeCommon::BadParameterEx)) {
    while (running) {
        m_mutex->acquire();
        ACE_Time_Value waittime(ACE_OS::gettimeofday() + TIME_PERIOD);
        m_wait->wait(&waittime);
        if (!running) { m_mutex->release(); break; }
        context->checkControllers();
        m_mutex->release();
    }
}

/***************************** ServiceController ******************************/

ServiceController::ServiceController(ACSDaemonContext *icontext, bool iautorestart) : context(icontext), autorestart(iautorestart), state(acsdaemon::NOT_EXISTING), active(false), startreq(NULL), stopreq(NULL) {
    m_mutex = new ACE_Thread_Mutex();
}

ServiceController::~ServiceController() {
    delete m_mutex;
}

void ServiceController::stopping() {
    // make state not existing manually - setState() would otherwise fire an
    // alarm and try to restart it
    state = acsdaemon::NOT_EXISTING;
}

void ServiceController::requestComplete(Request *request) {
    m_mutex->acquire();
    if (startreq == request) startreq = NULL;
    if (stopreq == request) stopreq = NULL;
    setState(getActualState());
    m_mutex->release();
}

bool ServiceController::setState(acsdaemon::ServiceState istate) {
    bool stopped = state != acsdaemon::NOT_EXISTING && istate == acsdaemon::NOT_EXISTING;
    /* on state change... */
    if (state != istate) {
        fireAlarm(istate);
    }
    state = istate;
    return stopped;
}

void ServiceController::restart() {
    m_mutex->acquire();
    if (active && setState(getActualState()) && autorestart && startreq == NULL /*&& stopreq == NULL*/) {
        // restarts only if state has just changed from RUNNING/DEGRADED to NOT_EXISTING

        ACS_SHORT_LOG((LM_INFO, "Restarting %s.", getServiceName().c_str()));
        stopreq = NULL;
        context->getRequestProcessor()->process(startreq = createControlledServiceRequest(START_SERVICE)); // enqueue service startup request
    }
    m_mutex->release();
}

bool ServiceController::start(acsdaemon::DaemonCallback_ptr callback) ACE_THROW_SPEC ((acsdaemonErrType::ServiceAlreadyRunningEx)) {
    m_mutex->acquire();
    setState(getActualState());
    active = true;
    if (stopreq != NULL || startreq == NULL && state == acsdaemon::NOT_EXISTING) {
        stopreq = NULL;
        context->getRequestProcessor()->process(startreq = createControlledServiceRequest(START_SERVICE, callback)); // enqueue service startup request
    } else if (callback != NULL) {
        // service is already running or scheduled to start!
        m_mutex->release();
        throw acsdaemonErrType::ServiceAlreadyRunningExImpl(__FILE__, __LINE__, "ServiceController::start").getServiceAlreadyRunningEx();
    }
    bool willrun = startreq != NULL || stopreq == NULL && (state == acsdaemon::RUNNING || state == acsdaemon::DEGRADED);
    m_mutex->release();
    // returns true if service is scheduled to start or already running
    return willrun;
}

void ServiceController::stop(acsdaemon::DaemonCallback_ptr callback) ACE_THROW_SPEC ((acsdaemonErrType::ServiceNotRunningEx)) {
    m_mutex->acquire();
    setState(getActualState());
    active = false;
    if (startreq != NULL || stopreq == NULL && state != acsdaemon::NOT_EXISTING) {
        startreq = NULL;
        context->getRequestProcessor()->process(stopreq = createControlledServiceRequest(STOP_SERVICE, callback)); // enqueue service shutdown request
    } else if (callback != NULL) {
        // service is not running or already scheduled to stop!
        m_mutex->release();
        throw acsdaemonErrType::ServiceNotRunningExImpl(__FILE__, __LINE__, "ServiceController::stop").getServiceNotRunningEx();
    }
    m_mutex->release();
}

/************************* ControlledServiceRequest ***************************/

ControlledServiceRequest::ControlledServiceRequest(ServiceController *icontroller, Request *irequest, bool iisstopping) : controller(icontroller), request(irequest), isstopping(iisstopping) {
}

ControlledServiceRequest::~ControlledServiceRequest() {
    if (delreq) delete request;
}

void ControlledServiceRequest::abort() {
    request->abort();
}

bool ControlledServiceRequest::execute() {
    if (isstopping) controller->stopping();
    delreq = request->execute();
    controller->requestComplete(this);
    return true;
}

/********************************* ImpRequest *********************************/

bool ImpRequest::execute() {
    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command.c_str()));
    ACE_OS::system(command.c_str());
    for (int i = 0; i <= 9; i++)
      if (controller->getContext()->getManagerReference(i))
        controller->setManagerReference(i, controller->getContext()->getManagerReference(i));
    return true;
}

ImpRequest::ImpRequest(ImpController *icontroller, ACSServiceRequestType itype, ACSServiceType iservice) :
	controller(icontroller) {
    // for now only itype=START_SERVICE is supported
    command = ACE_CString(acsServices[iservice].impexec);
}

/******************************* ImpController ********************************/

ImpController::ImpController(ACSDaemonContext *icontext, ACSServiceType iservice, bool iautostart) : ServiceController(icontext, iautostart), service(iservice) {
    corbaloc = ACE_CString("corbaloc::") + ACSPorts::getIP() + ":" + acsServices[service].impport + "/" + acsServices[service].imptype;
}

ControlledServiceRequest *ImpController::createControlledServiceRequest(ACSServiceRequestType itype, acsdaemon::DaemonCallback_ptr callback) {
    return new ControlledServiceRequest(this, new ImpRequest(this, itype, service), itype == STOP_SERVICE);
}

ACE_CString ImpController::getServiceName()
{
    return ACE_CString("Imp '") + acsServices[service].impname + "'";
}

acsdaemon::ServiceState ImpController::getActualState() {
    try {
        ACS_SHORT_LOG((LM_DEBUG, "Evaluating state of Imp with Corba URI '%s'.", corbaloc.c_str()));
        CORBA::Object_var obj = getContext()->getORB()->string_to_object(corbaloc.c_str());
        if (CORBA::is_nil(obj.in())) {
            ACS_SHORT_LOG((LM_ERROR, "Failed to parse Corba URI '%s' for Imp '%s'!", corbaloc.c_str(), acsServices[service].impname));
            return acsdaemon::DEFUNCT;
        }

        obj = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, obj.in());

        acsdaemon::ImpBase_var imp = acsdaemon::ImpBase::_narrow(obj.in());
        if (CORBA::is_nil(imp.in())) {
            ACS_SHORT_LOG((LM_INFO, "Imp '%s' is defunct.", acsServices[service].impname));
            return acsdaemon::DEFUNCT;
        }
        imp->ping();
        ACS_SHORT_LOG((LM_DEBUG, "Imp '%s' responded.", acsServices[service].impname));
        return acsdaemon::RUNNING;
//    } catch(CORBA::OBJECT_NOT_EXIST &ex) {
    } catch(CORBA::TRANSIENT &ex) {
        ACS_SHORT_LOG((LM_INFO, "Imp '%s' doesn't exist.", acsServices[service].impname));
        return acsdaemon::NOT_EXISTING;
    } catch(CORBA::Exception &ex) {
//        ACS_SHORT_LOG((LM_ERROR, "Failed."));
//        ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
        ACS_SHORT_LOG((LM_INFO, "Imp '%s' is defunct.", acsServices[service].impname));
        return acsdaemon::DEFUNCT;
    }
}

void ImpController::setManagerReference(const short instance_number, const char * ref) {
    try {
        ACS_SHORT_LOG((LM_DEBUG, "Setting manager reference '%s' at instance %d to Imp with Corba URI '%s'.", ref, instance_number, corbaloc.c_str()));
        CORBA::Object_var obj = getContext()->getORB()->string_to_object(corbaloc.c_str());
        if (CORBA::is_nil(obj.in())) {
            ACS_SHORT_LOG((LM_ERROR, "Failed to parse Corba URI '%s' for Imp '%s'!", corbaloc.c_str(), acsServices[service].impname));
            return;
        }

        obj = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, obj.in());

        acsdaemon::ImpBase_var imp = acsdaemon::ImpBase::_narrow(obj.in());
        if (CORBA::is_nil(imp.in())) {
            ACS_SHORT_LOG((LM_INFO, "Imp reference '%s' is not valid.", acsServices[service].impname));
            return;
        }

        imp->set_manager_reference(instance_number, ref);
        ACS_SHORT_LOG((LM_DEBUG, "Manager reference at instance %d set to Imp '%s'.", instance_number, acsServices[service].impname));
        return;
    } catch(CORBA::Exception &ex) {
        ACS_SHORT_LOG((LM_ERROR, "Failed to contact Imp '%s'.", acsServices[service].impname));
        return;
    }
}

/*************************** ACSServiceController *****************************/

ACSServiceController::ACSServiceController(ACSDaemonContext *icontext, ACSServiceRequestDescription *idesc, bool iautostart) : ServiceController(icontext, iautostart), desc(idesc), alarmSystemInitialized(false), alarmService(::alarmsystem::AlarmService::_nil()) {
    char str[256];
    const ACSService *service = &acsServices[idesc->getACSService()];
    std::string port = service->svcport == NULL ? service->namedsvcport(idesc->getInstanceNumber(), idesc->getName()) : service->svcport(idesc->getInstanceNumber());
    const char * cname = idesc->getCorbalocName() == NULL ? idesc->getName() : idesc->getCorbalocName();
    snprintf(str, 256, service->svccorbaurl, ACSPorts::getIP(), port.c_str(), cname == NULL ? "" : cname);
    corbaloc = ACE_CString(str);
}

ACSServiceController::~ACSServiceController() {
    delete desc;
}

ACE_CString ACSServiceController::getServiceName()
{
    if (desc->getName()) {
        return ACE_CString("'") + desc->getACSServiceName() + "' with name '" + desc->getName() + "'";
    } else {	
        return ACE_CString("'") + desc->getACSServiceName() + "'";
    }
}

ControlledServiceRequest *ACSServiceController::createControlledServiceRequest(ACSServiceRequestType itype, acsdaemon::DaemonCallback_ptr callback) {
    if (itype == STOP_SERVICE)
      getContext()->getDetailedServiceState(desc, 0); 
    return new ControlledServiceRequest(this, new ACSServiceRequest(getContext(), LOCAL, itype, new ACSServiceRequestDescription(*desc), callback), itype == STOP_SERVICE);
}

acsdaemon::ServiceState ACSServiceController::getActualState() {
    try {
        ACS_SHORT_LOG((LM_DEBUG, "Evaluating state of ACS service with Corba URI '%s'.", corbaloc.c_str()));
        CORBA::Object_var obj = getContext()->getORB()->string_to_object(corbaloc.c_str());
        if (CORBA::is_nil(obj.in())) {
            ACS_SHORT_LOG((LM_ERROR, "Failed to parse Corba URI '%s' for ACS service '%s'!", corbaloc.c_str(), desc->getACSServiceName()));
            return acsdaemon::DEFUNCT;
        }

        obj = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, obj.in());

        if (obj->_non_existent()) {
        	if (desc->getName()) {
	            ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' with name '%s' doesn't exist.", desc->getACSServiceName(), desc->getName()));
        	} else {	
    	        ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' doesn't exist.", desc->getACSServiceName()));
    	    }
            return acsdaemon::NOT_EXISTING;
        }
    	if (desc->getName()) {
            ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' with name '%s' responded.", desc->getACSServiceName(), desc->getName()));
    	} else {
	        ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' responded.", desc->getACSServiceName()));
	    }
        return getContext()->getDetailedServiceState(desc, obj.in()); // acsdaemon::RUNNING;
//    } catch(CORBA::OBJECT_NOT_EXIST &ex) {
    } catch(CORBA::TRANSIENT &ex) {
    	if (desc->getName()) {
            ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' with name '%s' doesn't exist.", desc->getACSServiceName(), desc->getName()));
    	} else {
	        ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' doesn't exist.", desc->getACSServiceName()));
	    }
        return acsdaemon::NOT_EXISTING;
    } catch(CORBA::Exception &ex) {
//        ACS_SHORT_LOG((LM_ERROR, "Failed."));
//        ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
    	if (desc->getName()) {
            ACS_SHORT_LOG((LM_DEBUG, "ACS service '%s' with name '%s' is defunct.", desc->getACSServiceName(), desc->getName()));
    	} else {	
	        ACS_SHORT_LOG((LM_INFO, "ACS service '%s' is defunct.", desc->getACSServiceName()));
	    }
        return acsdaemon::DEFUNCT;
    }
}

bool ACSServiceController::setState(acsdaemon::ServiceState istate) {
    bool stopped = state != acsdaemon::NOT_EXISTING && istate == acsdaemon::NOT_EXISTING;
    /* on state change... and anytime when alarm system is not initialized to force inicialization and sending current state */
    if (state != istate || !alarmSystemInitialized) {
        fireAlarm(istate);
    }
    state = istate;
    return stopped;
}

void ACSServiceController::fireAlarm(acsdaemon::ServiceState state) {
    if (!alarmSystemInitialized)
    {

        // no reference yet, noop
        if (!getContext()->getManagerReference(desc->getInstanceNumber()))
	   return;

        try {
           ACE_CString managerReference = getContext()->getManagerReference(desc->getInstanceNumber());
           ACS_SHORT_LOG((LM_DEBUG, "Initializing Alarm System using manager reference '%s'.", managerReference.c_str()));
           CORBA::Object_var obj = getContext()->getORB()->string_to_object(managerReference.c_str());
           if (CORBA::is_nil(obj.in())) {
               ACS_SHORT_LOG((LM_ERROR, "Failed to parse Corba URI '%s' as manager reference!", managerReference.c_str()));
               return;
           }

           obj = acsQoS::Timeout::setObjectTimeout(CORBA_TIMEOUT, obj.in());

	   ::maci::Manager_var manager = ::maci::Manager::_narrow(obj.in());
           if (manager.ptr() == ::maci::Manager::_nil()) {
               ACS_SHORT_LOG((LM_INFO, "Manager reference '%s' is not valid.", managerReference.c_str()));
               return;
           }

	   obj = manager->get_service(0, ::alarmsystem::AlarmServiceName, false);  
           if (CORBA::is_nil(obj.in())) {
               ACS_SHORT_LOG((LM_ERROR, "Failed to get '%s' service from manager!", ::alarmsystem::AlarmServiceName));
               return;
           }

           alarmService = ::alarmsystem::AlarmService::_narrow(obj.in());
           if (alarmService.ptr() == ::alarmsystem::AlarmService::_nil()) {
               ACS_SHORT_LOG((LM_INFO, "AlarmService reference is not valid."));
               return;
           }

	   alarmSystemInitialized = true; 

           ACS_SHORT_LOG((LM_DEBUG, "Alarm System initialized."));
       } catch (...) {
           ACS_SHORT_LOG((LM_DEBUG, "Failed to initialize Alarm System."));
           return;
       }
    } 

    ::alarmsystem::Triplet triplet;
    triplet.faultFamily = "Services";
    triplet.faultMember = desc->getACSServiceName();
    triplet.faultCode = 0; //(int)state;

    ACS::Time acsTime = ::getTimeStamp();

    bool hasName = (desc->getName() != 0);

    // create a Properties object and configure it
    CosPropertyService::Properties properties;
    properties.length(hasName ? 3 : 2);
    properties[0].property_name = "host";
    properties[0].property_value <<= desc->getHost();

    properties[1].property_name = "instance";
    properties[1].property_value <<= desc->getInstanceNumber();
/*    
    std::stringstream out;
    out << desc->getInstanceNumber();
    props.setProperty("instance", out.str());
*/
    // for named services (e.g. notification services)
    if (hasName)
    {
        properties[2].property_name = "name";
        properties[2].property_value <<= desc->getName();
    }
   
    ACE_TCHAR hostname[200];
    hostname[0] = 0;
    ACE_OS::hostname(hostname, 200);

    try
    {
       alarmService->submitAlarm(triplet, state != acsdaemon::RUNNING,
   	hostname, "ALARM_SOURCE_NAME", acsTime, properties);
    } catch (...) {
       ACS_SHORT_LOG((LM_DEBUG, "Failed to send an alarm."));
       return;
    }
}

/***************************** ACSDaemonContext *******************************/

ACSDaemonContext::ACSDaemonContext(std::string name, DetailedServiceStateProvider *dssp) : detailedServiceStateProvider(dssp) {
    reqproc = tm.create<RequestProcessorThread>((name + " Request Processor").c_str());
    ctrl = tm.create<ControllerThread>((name + " Controller").c_str());
    ctrl->setContext(this);
    m_mutex = new ACE_Thread_Mutex();
    impcontrollers = NULL;
    acsservicecontrollers = NULL;
}

ACSDaemonContext::~ACSDaemonContext() {
    ACS_SHORT_LOG((LM_DEBUG, "DESTROYING ACSDaemonContext!"));
    if (impcontrollers != NULL) {
        for (int i = 0; i < ACS_SERVICE_TYPES; i++) if (impcontrollers[i] != NULL) delete impcontrollers[i];
        delete [] impcontrollers;
    }
    if (acsservicecontrollers != NULL) {
        for (int i = 0; i < ACS_SERVICE_INSTANCES; i++) if (acsservicecontrollers[i] != NULL) delete acsservicecontrollers[i];
        delete [] acsservicecontrollers;
    }
    for (std::map<std::string, ServiceController **>::const_iterator itr = acsservicecontrollersmap.begin(); itr != acsservicecontrollersmap.end(); ++itr) {
        if (itr->second != NULL) {
            for (int i = 0; i < ACS_SERVICE_INSTANCES; i++) if (itr->second[i] != NULL) delete itr->second[i];
            delete [] itr->second;
        }
    }
    tm.destroy(ctrl);
    tm.destroy(reqproc);
    delete m_mutex;
}

ServiceController *ACSDaemonContext::getImpController(ACSServiceType service) {
    m_mutex->acquire();
    if (impcontrollers == NULL) {
        impcontrollers = new ServiceController*[ACS_SERVICE_TYPES];
        memset(impcontrollers, 0, sizeof(ServiceController*) * ACS_SERVICE_TYPES);
    }
    if (impcontrollers[service] == NULL)
	impcontrollers[service] = new ImpController(this, service);
    m_mutex->release();
    return impcontrollers[service];
}

void ACSDaemonContext::setImpControllersManagerReference(const short instance_number, const char * ref) {
    //m_mutex->acquire();
    if (impcontrollers == NULL)
       return;

    for (int i = 0; i < ACS_SERVICE_TYPES; i++)
       if (impcontrollers[i])
           ((ImpController*)impcontrollers[i])->setManagerReference(instance_number, ref);

    //m_mutex->release();
}

ServiceController *ACSDaemonContext::getACSServiceController(ACSServiceRequestDescription *desc) {
    m_mutex->acquire();
    int inum = desc->getInstanceNumber();
    const char *name = desc->getName();
    ServiceController **controllers;
    bool autorestart = acsServices[desc->getACSService()].autorestart;
    if (name == NULL) {
        if (acsservicecontrollers == NULL) {
            acsservicecontrollers = new ServiceController*[ACS_SERVICE_INSTANCES];
            memset(acsservicecontrollers, 0, sizeof(ServiceController*) * ACS_SERVICE_INSTANCES);
        }
        controllers = acsservicecontrollers;
        if (controllers[inum] == NULL) controllers[inum] = new ACSServiceController(this, desc, autorestart);
    } else {
        if ((controllers = acsservicecontrollersmap[name]) == NULL) {
            controllers = new ServiceController*[ACS_SERVICE_INSTANCES];
            memset(controllers, 0, sizeof(ServiceController*) * ACS_SERVICE_INSTANCES);
            acsservicecontrollersmap[name] = controllers;
        }
        if (controllers[inum] == NULL) controllers[inum] = new ACSServiceController(this, desc, autorestart);
    }
    m_mutex->release();
    return controllers[inum];
}

void ACSDaemonContext::initialize(CORBA::ORB_ptr iorb) {
    ACS_SHORT_LOG((LM_DEBUG, "Initializing ACSDaemonContext!"));
    orb = iorb;
    acsQoS::init(orb);
    reqproc->resume();
    ctrl->resume();
}

void ACSDaemonContext::dispose(CORBA::ORB_ptr iorb) {
    ACS_SHORT_LOG((LM_DEBUG, "Disposing ACSDaemonContext!"));
    ctrl->exit();
    reqproc->exit();
    acsQoS::done();
}

void ACSDaemonContext::processRequest(ACSServiceRequestTarget target, ACSServiceRequestType type, ACSServiceRequestDescription *desc, acsdaemon::DaemonCallback_ptr callback) ACE_THROW_SPEC ((acsdaemonErrType::ServiceAlreadyRunningEx, acsdaemonErrType::ServiceNotRunningEx)) {
    switch (target) {
    case IMP:
        getImpController(desc->getACSService())->start();
    case DAEMON:
        reqproc->process(new ACSServiceRequest(this, target, type, desc, callback));
        break;
    case LOCAL:
        switch (type) {
        case START_SERVICE: getACSServiceController(desc)->start(callback); break;
        case STOP_SERVICE: getACSServiceController(desc)->stop(callback); break;
        }
        break;
    }
}

void ACSDaemonContext::checkControllers() {
    m_mutex->acquire();
    if (impcontrollers != NULL) {
        for (int i = 0; i < ACS_SERVICE_TYPES; i++) if (impcontrollers[i] != NULL) impcontrollers[i]->restart();
    }
    if (acsservicecontrollers != NULL) {
        for (int i = 0; i < ACS_SERVICE_INSTANCES; i++) if (acsservicecontrollers[i] != NULL) acsservicecontrollers[i]->restart();
    }
    for (std::map<std::string, ServiceController **>::const_iterator itr = acsservicecontrollersmap.begin(); itr != acsservicecontrollersmap.end(); ++itr) {
        if (itr->second != NULL) {
            for (int i = 0; i < ACS_SERVICE_INSTANCES; i++) if (itr->second[i] != NULL) itr->second[i]->restart();
        }
    }
    m_mutex->release();
}

acsdaemon::ServiceState ACSDaemonContext::getACSServiceState(int instance_number, const char *name) {
    m_mutex->acquire();
    ServiceController **controllers = name == NULL ? acsservicecontrollers : acsservicecontrollersmap[name];
    acsdaemon::ServiceState retval = controllers == NULL || controllers[instance_number] == NULL ? acsdaemon::NOT_EXISTING : controllers[instance_number]->getLastState();
    m_mutex->release();
    return retval;
}


