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
* "@(#) $Id: acsNotificationServiceImpHandlerImpl.h,v 1.2 2008/10/28 09:43:31 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-09-30 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsImpBaseHandlerImpl.h"


class ACSNotificationServiceImpHandlerImpl : public ACSImpBaseHandlerImpl<ACSNotificationServiceImpHandlerImpl>, public POA_acsdaemon::NotificationServiceImp {

public:

    ACSNotificationServiceImpHandlerImpl() : ACSImpBaseHandlerImpl<ACSNotificationServiceImpHandlerImpl>(NOTIFICATION_SERVICE) {}
    
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

};



#endif
