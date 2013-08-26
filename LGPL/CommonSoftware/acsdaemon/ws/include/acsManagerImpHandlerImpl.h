#ifndef _ACS_MANAGER_IMP_HANDLER_IMPL_H_
#define _ACS_MANAGER_IMP_HANDLER_IMPL_H_

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
* "@(#) $Id: acsManagerImpHandlerImpl.h,v 1.1 2008/10/27 21:11:23 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-10-27 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsImpBaseHandlerImpl.h"


class ACSManagerImpHandlerImpl : public ACSImpBaseHandlerImpl<ACSManagerImpHandlerImpl>, public POA_acsdaemon::ManagerImp {

public:

    ACSManagerImpHandlerImpl() : ACSImpBaseHandlerImpl<ACSManagerImpHandlerImpl>(MANAGER) {}
    
    /*************************** CORBA interface *****************************/

    void start_manager (
        const char * domain,
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number,
        CORBA::Boolean recovery)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceAlreadyRunningEx
      )) {
        if (domain != NULL && strlen(domain) == 0) domain = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Starting Manager on Imp (instance %d).", instance_number));
        if (domain != NULL) {
            ACS_SHORT_LOG ((LM_WARNING, "Domain parameter of Manager startup is not supported!"));
        }
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(MANAGER, instance_number);
//        desc->setDomain(domain);
        desc->setRecovery(recovery);
        context->processRequest(LOCAL, START_SERVICE, desc, callback);
    }

    void stop_manager (
        const char * domain,
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceNotRunningEx
      )) {
        if (domain != NULL && strlen(domain) == 0) domain = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Stopping Manager on Imp (instance %d).", instance_number));
        if (domain != NULL) {
            ACS_SHORT_LOG ((LM_WARNING, "Domain parameter of Manager shutdown is not supported!"));
        }
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(MANAGER, instance_number);
//        desc->setDomain(domain);
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
