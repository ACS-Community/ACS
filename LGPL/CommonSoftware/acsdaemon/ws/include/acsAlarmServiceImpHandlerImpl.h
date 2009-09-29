#ifndef _ACS_ALARM_SERVICE_IMP_HANDLER_IMPL_H_
#define _ACS_ALARM_SERVICE_IMP_HANDLER_IMPL_H_

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
* "@(#) $Id: acsAlarmServiceImpHandlerImpl.h,v 1.2 2009/09/29 16:04:06 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-10-27 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsImpBaseHandlerImpl.h"


class ACSAlarmServiceImpHandlerImpl : public ACSImpBaseHandlerImpl<ACSAlarmServiceImpHandlerImpl>, public POA_acsdaemon::AlarmServiceImp {

public:

    ACSAlarmServiceImpHandlerImpl() : ACSImpBaseHandlerImpl<ACSAlarmServiceImpHandlerImpl>(ALARM_SERVICE) {}
    
    /*************************** CORBA interface *****************************/

    void start_alarm_service (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      )
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceAlreadyRunningEx
      )) {
        ACS_SHORT_LOG ((LM_INFO, "Starting Alarm Service on Imp (instance %d).", instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(ALARM_SERVICE, instance_number);
        context->processRequest(LOCAL, START_SERVICE, desc, callback);
    }

    void stop_alarm_service (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      )
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceNotRunningEx
      )) {
        ACS_SHORT_LOG ((LM_INFO, "Stopping Alarm Service on Imp (instance %d).", instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(ALARM_SERVICE, instance_number);
        context->processRequest(LOCAL, STOP_SERVICE, desc, callback);
    }


    acsdaemon::ServiceState get_service_status(CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx
      )) {
        return context->getACSServiceState(instance_number);
    }

};



#endif
