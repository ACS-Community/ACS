#ifndef _ACS_SERVICES_HANDLER_IMPL_H_
#define _ACS_SERVICES_HANDLER_IMPL_H_

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
* "@(#) $Id: acsServicesHandlerImpl.h,v 1.15 2012/05/15 09:06:34 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created 
* agrimstr 2007-11-07 refactored Services interface into separate
*                     class for use in template pattern implementation
*                     of the acsdaemon
* azagar   2008-08-12 migrated to ACS 8.0
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acsDaemonImpl.h"
#include "acsRequest.h"
#include "acsServiceController.h"


class ServiceDefinitionBuilderImpl : public POA_acsdaemon::ServiceDefinitionBuilder {

    short instance;
    ACE_CString services_definition_xml;

    static int definition_builder_count;

  public:

    ServiceDefinitionBuilderImpl(short instance_number) : instance(instance_number), services_definition_xml("") {
        if (definition_builder_count >= 10) {
            throw ::ACSErrTypeCommon::NoResourcesExImpl(__FILE__, __LINE__, 
                "::ServiceDefinitionBuilderImpl::ServiceDefinitionBuilderImpl").getNoResourcesEx();
        }
        if (instance_number < 0 || instance_number > 10) {
            throw ::ACSErrTypeCommon::BadParameterExImpl(__FILE__, __LINE__, 
                "::ServiceDefinitionBuilderImpl::ServiceDefinitionBuilderImpl").getBadParameterEx();
        }
        definition_builder_count++;
        ACS_SHORT_LOG((LM_DEBUG, "CREATING ServiceDefinitionBuilderImpl. New count: '%d'!", definition_builder_count));
     };

    ~ServiceDefinitionBuilderImpl() {
        definition_builder_count--;
        ACS_SHORT_LOG((LM_DEBUG, "DESTROYING ServiceDefinitionBuilderImpl. New count: '%d'!", definition_builder_count));
    };
    
    /*************************** CORBA interface *****************************/

    ::CORBA::Short acs_instance_number (
        void);

    void add_naming_service (
        const char * host);
   
    void add_alarm_service (
        const char * host);
 
    void add_notification_service (
        const char * name,
        const char * host);
    
    void add_xml_cdb (
        const char * host,
        ::CORBA::Boolean recovery,
        const char * cdb_xml_dir);
    
    void add_rdb_cdb (
        const char * host,
        ::CORBA::Boolean recovery,
        const char * config_name);

    void add_manager (
        const char * host,
        const char * domain,
        ::CORBA::Boolean recovery);
    
    void add_acs_log (
        const char * host);
    
    void add_logging_service (
        const char * host,
        const char * name);
    
    void add_interface_repository (
        const char * host,
        ::CORBA::Boolean load,
        ::CORBA::Boolean wait_load);
    
    void add_services_definition (
        const char * definition);
    
    ::CORBA::Boolean is_valid (
        ::CORBA::String_out error_description);
    
    char * get_services_definition (
        void);
    
    void close (
        void);
    
};

class ACSServicesHandlerImpl : public POA_acsdaemon::ServicesDaemon {

  public:
    
   /**
    * Constructor
    */
    ACSServicesHandlerImpl();
  
    /**
     * Destructor
     */
    virtual ~ACSServicesHandlerImpl();

    /**
     * Sets ACS Daemon service
     */
    void setService(ACSDaemonServiceImpl<ACSServicesHandlerImpl> *service)
    {
        h_service = service;
    }

    /**
     * Initialize handler
     */
    void initialize(CORBA::ORB_ptr orb);

    /**
     * Dispose handler
     */
    void dispose(CORBA::ORB_ptr orb);

    /**
     * Get the name of this container handler
     */
    std::string getName();

    /**
     * Get the type string of this container handler
     */
    std::string getType();
    
    /**
     * Return the port where this services handler listens for connections
     */
    std::string getPort();
   
    /**
     * Returns ACS services definition (common set of servies).
     */
    std::string getServices(short instance_number, bool recovery);
 
    /*************************** CORBA interface *****************************/

    ::acsdaemon::ServiceDefinitionBuilder_ptr create_service_definition_builder (
        ::CORBA::Short instance_number);
    
    void start_services (
        const char * definition,
        ::CORBA::Boolean reuse_services,
        ::acsdaemon::DaemonSequenceCallback_ptr callback
      )throw(ACSErrTypeCommon::BadParameterEx);
    
    void stop_services (
        const char * definition,
        ::acsdaemon::DaemonSequenceCallback_ptr callback
	) throw (ACSErrTypeCommon::BadParameterEx);
    
    void start_naming_service (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
	) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void start_notification_service (
        const char * name,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void start_xml_cdb (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number,
        ::CORBA::Boolean recovery,
        const char * cdb_xml_dir
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void start_rdb_cdb (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number,
        ::CORBA::Boolean recovery,
        const char * config_name
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);

    void start_manager (
        const char * domain,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number,
        ::CORBA::Boolean recovery
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void start_acs_log (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void start_logging_service (
        const char * name,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void start_interface_repository (
        ::CORBA::Boolean load,
        ::CORBA::Boolean wait_load,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);
    
    void stop_naming_service (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
	) throw( ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void stop_notification_service (
        const char * name,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void stop_cdb (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
	) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void stop_manager (
        const char * domain,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void stop_acs_log (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void stop_logging_service (
        const char * name,
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void stop_interface_repository (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
    
    void start_alarm_service (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw (ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceAlreadyRunningEx);

    void stop_alarm_service (
        ::acsdaemon::DaemonCallback_ptr callback,
        ::CORBA::Short instance_number
      ) throw(ACSErrTypeCommon::BadParameterEx, acsdaemonErrType::ServiceNotRunningEx);
 
    void start_acs (
        acsdaemon::DaemonSequenceCallback_ptr callback,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      )  throw(CORBA::SystemException, ::ACSErrTypeCommon::BadParameterEx);
    
    void stop_acs (
        acsdaemon::DaemonSequenceCallback_ptr callback,
        ::CORBA::Short instance_number,
        const char * additional_command_line
      ) throw (CORBA::SystemException, ::ACSErrTypeCommon::BadParameterEx);

     char * status_acs ( 
         ::CORBA::Short instance_number
         ) throw (CORBA::SystemException, ::acsdaemonErrType::FailedToGetAcsStatusEx);

    void shutdown () throw (CORBA::SystemException,::maciErrType::NoPermissionEx);


    void set_configuration_reference (
      ::CORBA::Short instance_number,
      const ::acsdaemon::ServiceInfoSeq & services_info) throw(CORBA::SystemException)
   {
       if (context)
	   context->setConfigurationReference(instance_number, services_info);
   }

  private:
    std::string h_name; // Name of services handler (used for logging purposes)
    std::string h_type; // CORBA-type for this services handler
    ACSDaemonServiceImpl<ACSServicesHandlerImpl> *h_service; // ACS daemon service
    ACSDaemonContext *context;
};



#endif
