#ifndef _ACS_CONFIGURATION_DATABASE_IMP_HANDLER_IMPL_H_
#define _ACS_CONFIGURATION_DATABASE_IMP_HANDLER_IMPL_H_

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
* "@(#) $Id: acsConfigurationDatabaseImpHandlerImpl.h,v 1.3 2012/02/28 12:53:37 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* azagar   2008-10-27 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsImpBaseHandlerImpl.h"


class ACSConfigurationDatabaseImpHandlerImpl : public ACSImpBaseHandlerImpl<ACSConfigurationDatabaseImpHandlerImpl>, public POA_acsdaemon::CDBImp {

public:

    ACSConfigurationDatabaseImpHandlerImpl() : ACSImpBaseHandlerImpl<ACSConfigurationDatabaseImpHandlerImpl>(CDB) {}
    
    /*************************** CORBA interface *****************************/

    void start_xml_cdb (
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number,
        CORBA::Boolean recovery,
        const char * cdb_xml_dir
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx)
    {
        if (cdb_xml_dir != NULL && strlen(cdb_xml_dir) == 0) cdb_xml_dir = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Starting Configuration Database on Imp (instance %d).", instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(CDB, instance_number);
        desc->setRecovery(recovery);
        desc->setCdbXMLDir(cdb_xml_dir);
        context->processRequest(LOCAL, START_SERVICE, desc, callback);
    }

    void start_rdb_cdb (
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number,
        CORBA::Boolean recovery,
        const char * config_name
      )
      throw (
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceAlreadyRunningEx
      )
    {
        if (config_name != NULL && strlen(config_name) == 0) config_name = NULL;
        ACS_SHORT_LOG ((LM_INFO, "Starting RDB Configuration Database on Imp (instance %d).", instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(RDB_CDB, instance_number);
        desc->setRecovery(recovery);
        desc->setCdbXMLDir(config_name);	// used as config_name
        context->processRequest(LOCAL, START_SERVICE, desc, callback);
    }

    void stop_cdb (
        acsdaemon::DaemonCallback_ptr callback,
        CORBA::Short instance_number
      )
      throw(
        ACSErrTypeCommon::BadParameterEx,
        acsdaemonErrType::ServiceNotRunningEx
      )
    {
        ACS_SHORT_LOG ((LM_INFO, "Stopping Configuration Database on Imp (instance %d).", instance_number));
        ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(CDB, instance_number);
        context->processRequest(LOCAL, STOP_SERVICE, desc, callback);
    }
        
    acsdaemon::ServiceState get_service_status(CORBA::Short instance_number)
      throw(
        ACSErrTypeCommon::BadParameterEx
      ) {
        return context->getACSServiceState(instance_number);
    }

};



#endif
