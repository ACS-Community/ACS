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
* "@$Id: acsServicesHandlerImpl.cpp,v 1.21 2012/02/28 13:03:56 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
* agrimstr 2007-11-07 extracted service interface implementation to separate
*                     class
* azagar   2008-08-12 migrated to ACS 8.0
*/

#include "acsServicesHandlerImpl.h"
#include <expat.h>

#ifndef XML_TRUE
#define XML_TRUE 1
#endif

#ifndef XML_STATUS_OK
#define XML_STATUS_OK 1
#endif

#ifndef XML_STATUS_ERROR
#define XML_STATUS_ERROR 0
#endif

/************************** ServiceDefinitionBuilderImpl ****************************/

int ServiceDefinitionBuilderImpl::definition_builder_count = 0;

/* CORBA interface */

::CORBA::Short ServiceDefinitionBuilderImpl::acs_instance_number (
    void)
{
    return instance;
}

void ServiceDefinitionBuilderImpl::add_naming_service (
    const char * host)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[NAMING_SERVICE].xmltag;
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
   
void ServiceDefinitionBuilderImpl::add_alarm_service (
    const char * host)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[ALARM_SERVICE].xmltag;
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
 
void ServiceDefinitionBuilderImpl::add_notification_service (
    const char * name,
    const char * host)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[NOTIFICATION_SERVICE].xmltag;
    if (name != NULL && name[0] != '\0') services_definition_xml = services_definition_xml + " name=\"" + name + "\"";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
    
void ServiceDefinitionBuilderImpl::add_xml_cdb (
    const char * host,
    ::CORBA::Boolean recovery,
    const char * cdb_xml_dir)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[CDB].xmltag;
    if (host != NULL) services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " recovery=\"" + (recovery ? "true" : "false") + "\"";
    services_definition_xml = services_definition_xml + " cdb_xml_dir=\"" + cdb_xml_dir + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
    
void ServiceDefinitionBuilderImpl::add_rdb_cdb (
    const char * host,
    ::CORBA::Boolean recovery,
    const char * config_name)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[CDB].xmltag;
    if (host != NULL) services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " recovery=\"" + (recovery ? "true" : "false") + "\"";
    services_definition_xml = services_definition_xml + " config_name=\"" + config_name + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}

void ServiceDefinitionBuilderImpl::add_manager (
    const char * host,
    const char * domain,
    ::CORBA::Boolean recovery)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[MANAGER].xmltag;
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    if (domain != NULL && domain[0] != '\0') services_definition_xml = services_definition_xml + " domain=\"" + domain + "\"";
    services_definition_xml = services_definition_xml + " recovery=\"" + (recovery ? "true" : "false") + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
    
void ServiceDefinitionBuilderImpl::add_acs_log (
    const char * host)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[ACS_LOG_SERVICE].xmltag;
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
    
void ServiceDefinitionBuilderImpl::add_logging_service (
    const char * host,
    const char * name)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[LOGGING_SERVICE].xmltag;
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    if (name != NULL && name[0] != '\0') services_definition_xml = services_definition_xml + " name=\"" + name + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
    
void ServiceDefinitionBuilderImpl::add_interface_repository (
    const char * host,
    ::CORBA::Boolean load,
    ::CORBA::Boolean wait_load)
{
    services_definition_xml = services_definition_xml + "<" + acsServices[INTERFACE_REPOSITORY].xmltag;
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + " host=\"" + host + "\"";
    services_definition_xml = services_definition_xml + " load=\"" + (load ? "true" : "false") + "\"";
    services_definition_xml = services_definition_xml + " wait_load=\"" + (wait_load ? "true" : "false") + "\"";
    services_definition_xml = services_definition_xml + " />\n";
}
    
void ServiceDefinitionBuilderImpl::add_services_definition (
    const char * definition)
{
    services_definition_xml = services_definition_xml + definition;
}
    
::CORBA::Boolean ServiceDefinitionBuilderImpl::is_valid (
    ::CORBA::String_out error_description)
{
    // TODO: validation!
    return true;
}
    
char * ServiceDefinitionBuilderImpl::get_services_definition (
    void)
{
    char buffer[64];
    sprintf(buffer, "<acs_services_definition instance=\"%d\">\n", instance);
    return CORBA::string_dup((buffer + services_definition_xml + "</acs_services_definition>\n").c_str());
}
    
void ServiceDefinitionBuilderImpl::close (
    void)
{
    // ServiceDefinitionBuilderImpl may despose itself now
    PortableServer::POA_var poa = this->_default_POA();
    PortableServer::ObjectId_var oid = poa->servant_to_id(this);
    poa->deactivate_object(oid.in());
    this->_remove_ref();
}

/************************** ACSServicesHandlerImpl ****************************/

ACSServicesHandlerImpl::ACSServicesHandlerImpl () : h_name("ACS Services Daemon"), h_type(::acsdaemon::servicesDaemonServiceName)
{
    context = new ACSDaemonContext(h_name);
}

ACSServicesHandlerImpl::~ACSServicesHandlerImpl (void)
{
    ACS_SHORT_LOG((LM_DEBUG, "DESTROYING ACSServicesHandlerImpl!"));
    delete context;
}

std::string ACSServicesHandlerImpl::getName ()
{
    return h_name;
}

std::string ACSServicesHandlerImpl::getType(void)
{
    return h_type;
}

std::string ACSServicesHandlerImpl::getPort(void)
{
    return ACSPorts::getServicesDaemonPort();
}

void ACSServicesHandlerImpl::initialize(CORBA::ORB_ptr orb)
{
    context->initialize(orb);
}

void ACSServicesHandlerImpl::dispose(CORBA::ORB_ptr orb)
{
    context->dispose(orb);
}

/* CORBA interface */

void definitionXMLStartElementHandler(void *userData, const XML_Char *name, const XML_Char **atts) {
    ACSServiceRequestChainContext *rcc = (ACSServiceRequestChainContext*)userData;
    rcc->addRequest(name, atts);
}

::acsdaemon::ServiceDefinitionBuilder_ptr ACSServicesHandlerImpl::create_service_definition_builder (
    ::CORBA::Short instance_number)
{
    return (new ServiceDefinitionBuilderImpl(instance_number))->_this();
}

void ACSServicesHandlerImpl::start_services (
    const char * definition,
    ::CORBA::Boolean reuse_services,
    ::acsdaemon::DaemonSequenceCallback_ptr callback
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx
  ))
{
    ACSServiceRequestChainContext *rcc = new ACSServiceRequestChainContext(context, START_SERVICE, reuse_services, callback);
    XML_Parser parser = XML_ParserCreate("UTF-8");
    XML_SetUserData(parser, (void*)rcc);
    XML_SetStartElementHandler(parser, &definitionXMLStartElementHandler);
    if (XML_Parse(parser, definition, strlen(definition), XML_TRUE) == XML_STATUS_ERROR) {
        ACS_SHORT_LOG ((LM_ERROR, "Failed to parse service definition XML!"));
    }
    XML_ParserFree(parser);
    rcc->startProcessing();
}
    
void ACSServicesHandlerImpl::stop_services (
    const char * definition,
    ::acsdaemon::DaemonSequenceCallback_ptr callback
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx
  ))
{
    ACSServiceRequestChainContext *rcc = new ACSServiceRequestChainContext(context, STOP_SERVICE, true, callback);
    XML_Parser parser = XML_ParserCreate("UTF-8");
    XML_SetUserData(parser, (void*)rcc);
    XML_SetStartElementHandler(parser, &definitionXMLStartElementHandler);
    if (XML_Parse(parser, definition, strlen(definition), XML_TRUE) == XML_STATUS_ERROR) {
        ACS_SHORT_LOG ((LM_ERROR, "Failed to parse service definition XML!"));
    }
    XML_ParserFree(parser);
    rcc->startProcessing();
}

void ACSServicesHandlerImpl::start_naming_service (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Naming Service (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(NAMING_SERVICE, instance_number);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}

void ACSServicesHandlerImpl::start_alarm_service (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Alarm Service (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(ALARM_SERVICE, instance_number);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}

void ACSServicesHandlerImpl::start_notification_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    if (name != NULL && strlen(name) == 0) name = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Starting '%s' Notification Service (instance %d).", name == NULL ? "default" : name, instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(NOTIFICATION_SERVICE, instance_number);
    desc->setName(name);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::start_xml_cdb (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    ::CORBA::Boolean recovery,
    const char * cdb_xml_dir
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    if (cdb_xml_dir != NULL && strlen(cdb_xml_dir) == 0) cdb_xml_dir = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Starting Configuration Database (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(CDB, instance_number);
    desc->setRecovery(recovery);
    desc->setCdbXMLDir(cdb_xml_dir);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::start_rdb_cdb (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    ::CORBA::Boolean recovery,
    const char * config_name
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    if (config_name != NULL && strlen(config_name) == 0) config_name = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Starting RDB Configuration Database (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(RDB_CDB, instance_number);
    desc->setRecovery(recovery);
    desc->setCdbXMLDir(config_name);	// used as config_name
    context->processRequest(IMP, START_SERVICE, desc, callback);
}

void ACSServicesHandlerImpl::start_manager (
    const char * domain,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    ::CORBA::Boolean recovery
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    if (domain != NULL && strlen(domain) == 0) domain = NULL;
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(MANAGER, instance_number);
    desc->setDomain(domain);
    desc->setRecovery(recovery);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::start_acs_log (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Starting ACS Log Service (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(ACS_LOG_SERVICE, instance_number);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::start_logging_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    if (name != NULL && (strlen(name) == 0 || strcmp(name, "Log") == 0)) name = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Starting '%s' Logging Service (instance %d).", name == NULL ? "Log" : name, instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(LOGGING_SERVICE, instance_number);
    desc->setName(name);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::start_interface_repository (
    ::CORBA::Boolean load,
    ::CORBA::Boolean wait_load,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceAlreadyRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Interface Repository (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(INTERFACE_REPOSITORY, instance_number);
    desc->setLoadIR(load);
    desc->setWaitLoadIR(wait_load);
    context->processRequest(IMP, START_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::stop_naming_service (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Naming Service (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(NAMING_SERVICE, instance_number);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}

void ACSServicesHandlerImpl::stop_alarm_service (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Alarm Service (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(ALARM_SERVICE, instance_number);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}
 
void ACSServicesHandlerImpl::stop_notification_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    if (name != NULL && strlen(name) == 0) name = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Stopping '%s' Notification Service (instance %d).", name == NULL ? "default" : name, instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(NOTIFICATION_SERVICE, instance_number);
    desc->setName(name);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::stop_cdb (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Configuration Database (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(CDB, instance_number);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::stop_manager (
    const char * domain,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    if (domain != NULL && strlen(domain) == 0) domain = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Stopping Manager (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(MANAGER, instance_number);
    desc->setDomain(domain);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::stop_acs_log (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping ACS Log Service (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(ACS_LOG_SERVICE, instance_number);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::stop_logging_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    if (name != NULL && (strlen(name) == 0 || strcmp(name, "Log") == 0)) name = NULL;
    ACS_SHORT_LOG ((LM_INFO, "Stopping '%s' Logging Service (instance %d).", name == NULL ? "Log" : name, instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(LOGGING_SERVICE, instance_number);
    desc->setName(name);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}
    
void ACSServicesHandlerImpl::stop_interface_repository (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number
  )
  ACE_THROW_SPEC ((
    ACSErrTypeCommon::BadParameterEx,
    acsdaemonErrType::ServiceNotRunningEx
  ))
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Interface Repository (instance %d).", instance_number));
    ACSServiceRequestDescription *desc = new ACSServiceRequestDescription(INTERFACE_REPOSITORY, instance_number);
    context->processRequest(IMP, STOP_SERVICE, desc, callback);
}

std::string
ACSServicesHandlerImpl::getServices(short instance_number, bool recovery)
{
    std::string host = ACSPorts::getIP();
    #define DAEMONHOST host.c_str()

    acsdaemon::ServiceDefinitionBuilder *sdb = this->create_service_definition_builder(instance_number);
    sdb->add_naming_service(DAEMONHOST);
    sdb->add_interface_repository(DAEMONHOST, true, false);
    sdb->add_notification_service("NotifyEventChannelFactory", DAEMONHOST);
    sdb->add_notification_service("LoggingNotifyEventChannelFactory", DAEMONHOST);
    sdb->add_notification_service("ArchiveNotifyEventChannelFactory", DAEMONHOST);
    sdb->add_notification_service("AlarmNotifyEventChannelFactory", DAEMONHOST);
    sdb->add_logging_service(DAEMONHOST, "Log");
    sdb->add_acs_log(DAEMONHOST);
    sdb->add_xml_cdb(DAEMONHOST, recovery, getenv("ACS_CDB"));
    sdb->add_alarm_service(DAEMONHOST);
    sdb->add_manager(DAEMONHOST, "", recovery);
    
    #undef DAEMONHOST

    CORBA::String_var defs = sdb->get_services_definition();
    std::string services = defs.in();
    sdb->close();
    return services;
}

void
ACSServicesHandlerImpl::start_acs (
    acsdaemon::DaemonSequenceCallback_ptr callback,
    ::CORBA::Short instance_number,
    const char * additional_command_line
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException,
    ::ACSErrTypeCommon::BadParameterEx
  ))
{
    bool recovery = additional_command_line && ACE_OS::strstr(additional_command_line, "-r");
    this->start_services(getServices(instance_number, false).c_str(), recovery, callback);

    // set local manager reference
    std::string host = ACSPorts::getIP();
    std::string managerPort = ACSPorts::getManagerPort(instance_number);
    std::string managerLocalCorbaloc = "corbaloc::" + host + ":" + managerPort + "/Manager";

    this->set_manager_reference(instance_number, managerLocalCorbaloc.c_str());
}


void
ACSServicesHandlerImpl::stop_acs (
    acsdaemon::DaemonSequenceCallback_ptr callback,
    ::CORBA::Short instance_number,
    const char * additional_command_line
  )
  ACE_THROW_SPEC ((
    CORBA::SystemException,
    ::ACSErrTypeCommon::BadParameterEx
  ))
{
    this->stop_services(getServices(instance_number, false).c_str(), callback);
}


char * ACSServicesHandlerImpl::status_acs ( 
    ::CORBA::Short instance_number
    )
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::acsdaemonErrType::FailedToGetAcsStatusEx
      ))
{
    int result;
    char *acsStatus=0;
    char command[100];
    std::string logFile="acsStatus_";

    logFile += getStringifiedTimeStamp().c_str();

    snprintf(command, 100, "acsStatus -b %d &> %s", instance_number, logFile.c_str());

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    result = ACE_OS::system(command);
    if (result < 0)
	{
	snprintf(command, 100, "rm -rf %s", logFile.c_str());
	ACE_OS::system(command);
	throw ::acsdaemonErrType::FailedToGetAcsStatusExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::status_acs").getFailedToGetAcsStatusEx();
	}
    
    ACS_SHORT_LOG ((LM_INFO, "Reading output from: '%s'.", logFile.c_str()));
    
    ifstream outFile(logFile.c_str(), ios::in|ios::ate);
    if (outFile.is_open())
	{
	int outFileSize = outFile.tellg();
	acsStatus = new char [outFileSize+1];
	outFile.seekg (0, ios::beg);
	outFile.read (acsStatus, outFileSize);
	outFile.close();
	acsStatus[outFileSize]=0;
	snprintf(command, 100, "rm -rf %s", logFile.c_str());
	ACE_OS::system(command);
	}
    else
	{
	snprintf(command, 100, "rm -rf %s", logFile.c_str());
	ACE_OS::system(command);
	throw ::acsdaemonErrType::FailedToGetAcsStatusExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::status_acs").getFailedToGetAcsStatusEx();
	}//if-else

    //acsStatus is deleted by CORBA
    return acsStatus;
}//ACSServicesHandlerImpl::status_acs

void ACSServicesHandlerImpl::shutdown ()
      ACE_THROW_SPEC ((
        CORBA::SystemException,
        ::maciErrType::NoPermissionEx
      ))
{
    if (h_service->isProtected())
	{
	throw ::maciErrType::NoPermissionEx();
	}
    ACS_SHORT_LOG ((LM_INFO, "Shutting down the ACS Services Daemon on remote request..."));
    h_service->shutdown(false);
}
