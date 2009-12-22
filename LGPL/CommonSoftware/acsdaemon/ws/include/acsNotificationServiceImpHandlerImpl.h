#ifndef _ACS_NOTIFICATION_SERVICE_IMP_HANDLER_IMPL_H_
#define _ACS_NOTIFICATION_SERVICE_IMP_HANDLER_IMPL_H_

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
* "@(#) $Id: acsNotificationServiceImpHandlerImpl.h,v 1.8 2009/12/22 10:46:43 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-09-30 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsImpBaseHandlerImpl.h"
#include "acsNotificationServiceMonitor.h"

#include <map>

typedef std::map<ACSServiceRequestDescription*, NotificationServiceMonitor*> MonitorMap;

class ACSNotificationServiceImpHandlerImpl : public ACSImpBaseHandlerImpl<ACSNotificationServiceImpHandlerImpl>, public POA_acsdaemon::NotificationServiceImp {
private:
  MonitorMap monitorMap;
  int intervalCount;
public:

    ACSNotificationServiceImpHandlerImpl() : ACSImpBaseHandlerImpl<ACSNotificationServiceImpHandlerImpl>(NOTIFICATION_SERVICE) {}
   
    virtual ~ACSNotificationServiceImpHandlerImpl()
    {  
      MonitorMap::iterator iter = monitorMap.begin();
      for (; iter != monitorMap.end(); iter++)
      {
        NotificationServiceMonitor* nsm = iter->second;
        nsm->destroy();
        delete nsm;
      }
    } 

    /*************************** CORBA interface *****************************/

    void start_notification_service (
        const char * name,
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceAlreadyRunningEx
      )) {
        if (name != NULL && strlen(name) == 0) name = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Starting '%s' Notification Service on Imp (instance %d).", name == NULL ? "default" : name, instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(NOTIFICATION_SERVICE, instance_number);
        desc->setName(name);
    #define NOTIFY_FACTORY_NAME_STRING "NotifyEventChannelFactory"
    // add NOTIFY_FACTORY_NAME_STRING postfix, is not already there
    if (name != NULL) {
        int lendiff = (int)strlen(name) - strlen(NOTIFY_FACTORY_NAME_STRING);
        if (lendiff < 0 || strcmp(name + lendiff, NOTIFY_FACTORY_NAME_STRING) != 0)
            desc->setCorbalocName((ACE_CString(name) + NOTIFY_FACTORY_NAME_STRING).c_str());

    }
        context->processRequest(LOCAL, START_SERVICE, desc, callback);
    }

    void stop_notification_service (
        const char * name,
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceNotRunningEx
      )) {
        if (name != NULL && strlen(name) == 0) name = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Stopping '%s' Notification Service on Imp (instance %d).", name == NULL ? "default" : name, instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(NOTIFICATION_SERVICE, instance_number);
        desc->setName(name);
        context->processRequest(LOCAL, STOP_SERVICE, desc, callback);
    }

    acsdaemon::ServiceState get_service_status(const char * name, CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx
      )) {
        return context->getACSServiceState(instance_number, name);
    }

    virtual acsdaemon::ServiceState getDetailedServiceState(ACSServiceRequestDescription *desc, CORBA::Object_ptr obj) {

    // destroy check 
    if (obj == 0)
    { 
      if (monitorMap.find(desc) != monitorMap.end())
      {  
        NotificationServiceMonitor* nsm = monitorMap[desc];
        nsm->destroy();
        monitorMap.erase(desc);
        delete nsm;
      }
      return acsdaemon::DEFUNCT;
    }

    bool isRightNCType = obj->_is_a("IDL:sandia.gov/NotifyMonitoringExt/EventChannelFactory:1.0");
    if (!isRightNCType) {
		ACS_SHORT_LOG((LM_ERROR, "%s does not extend required interface, reported as defunctional.", desc->getName()));
		return acsdaemon::DEFUNCT;
    }
    
    // do not monitor alarm notification service
    if (ACE_OS::strcmp(desc->getName(), "AlarmNotifyEventChannelFactory") == 0 ||
        ACE_OS::strcmp(desc->getName(), "Alarm") == 0)
      return acsdaemon::RUNNING;
    
    if (monitorMap.find(desc) == monitorMap.end())
    {
      CosNotifyChannelAdmin::EventChannelFactory_var ecf = CosNotifyChannelAdmin::EventChannelFactory::_narrow(obj);
      NotificationServiceMonitor* nsm = new NotificationServiceMonitor(ecf.in());
      nsm->init();
      monitorMap[desc] = nsm;
      nsm->issuePingEvent();
      return acsdaemon::RUNNING;
    }
    else
    {
      NotificationServiceMonitor* nsm = monitorMap[desc];
      CORBA::ULongLong rtt = nsm->getAndResetRTT();
      nsm->issuePingEvent();
      if (rtt < 0) { // no response
        ACS_SHORT_LOG((LM_DEBUG, "%s is not responsive, reported as defunctional.", desc->getName()));
        return acsdaemon::DEFUNCT;
      }
      else if (rtt > 200000) { // > 200ms
        ACS_SHORT_LOG((LM_DEBUG, "%s response time is slow, reported as degraded.", desc->getName()));
        return acsdaemon::DEGRADED;
      }
      else
        return acsdaemon::RUNNING;
    }

  }

};


#endif
