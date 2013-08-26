#ifndef _ACS_LOGGING_SERVICE_IMP_HANDLER_IMPL_H_
#define _ACS_LOGGING_SERVICE_IMP_HANDLER_IMPL_H_

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
* "@(#) $Id: acsLoggingServiceImpHandlerImpl.h,v 1.2 2008/10/28 13:54:15 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-10-27 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsImpBaseHandlerImpl.h"


class ACSLoggingServiceImpHandlerImpl : public ACSImpBaseHandlerImpl<ACSLoggingServiceImpHandlerImpl>, public POA_acsdaemon::LoggingServiceImp {

public:

    ACSLoggingServiceImpHandlerImpl() : ACSImpBaseHandlerImpl<ACSLoggingServiceImpHandlerImpl>(LOGGING_SERVICE) {}
    
    /*************************** CORBA interface *****************************/

    void start_logging_service (
        const char * name,
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceAlreadyRunningEx
      )) {
        if (name != NULL && (strlen(name) == 0 || strcmp(name, "Log") == 0)) name = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Starting '%s' Logging Service on Imp (instance %d).", name == NULL ? "Log" : name, instance_number));
        if (name != NULL) {
            ACS_SHORT_LOG ((LM_WARNING, "Name parameter of Logging Service startup is not supported!"));
        }
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(LOGGING_SERVICE, instance_number);
//        desc->setName(name);
        context->processRequest(LOCAL, START_SERVICE, desc, callback);
    }

    void stop_logging_service (
        const char * name,
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number)
      ACE_THROW_SPEC ((
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceNotRunningEx
      )) {
        if (name != NULL && (strlen(name) == 0 || strcmp(name, "Log") == 0)) name = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Stopping '%s' Logging Service on Imp (instance %d).", name == NULL ? "Log" : name, instance_number));
        if (name != NULL) {
            ACS_SHORT_LOG ((LM_WARNING, "Name parameter of Logging Service shutdown is not supported!"));
        }
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(LOGGING_SERVICE, instance_number);
//        desc->setName(name);
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
