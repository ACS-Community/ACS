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
* "@$Id: acsServicesHandlerACS80Impl.cpp,v 1.2 2008/09/15 13:04:44 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
* agrimstr 2007-11-07 extracted service interface implementation to separate
*                     class
* azagar   2008-08-12 migrated to ACS 8.0 API
*/

#include "acsServicesHandlerACS80Impl.h"
#include <expat.h>

#define XML_TRUE 1
#define XML_FALSE 0
#define XML_STATUS_OK 1
#define XML_STATUS_ERROR 0

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
    services_definition_xml = services_definition_xml + "<naming_service ";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
}
    
void ServiceDefinitionBuilderImpl::add_notification_service (
    const char * name,
    const char * host)
{
    services_definition_xml = services_definition_xml + "<notification_service ";
    if (name != NULL && name[0] != '\0') services_definition_xml = services_definition_xml + "name=\"" + name + "\" ";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
}
    
void ServiceDefinitionBuilderImpl::add_cdb (
    const char * host)
{
    services_definition_xml = services_definition_xml + "<cdb ";
    if (host != NULL) services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
}
    
void ServiceDefinitionBuilderImpl::add_manager (
    const char * host,
    const char * domain)
{
    services_definition_xml = services_definition_xml + "<manager ";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    if (domain != NULL && domain[0] != '\0') services_definition_xml = services_definition_xml + "domain=\"" + domain + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
}
    
void ServiceDefinitionBuilderImpl::add_acs_log (
    const char * host)
{
    services_definition_xml = services_definition_xml + "<acs_log ";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
}
    
void ServiceDefinitionBuilderImpl::add_logging_service (
    const char * host,
    const char * name)
{
    services_definition_xml = services_definition_xml + "<logging_service ";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    if (name != NULL && name[0] != '\0') services_definition_xml = services_definition_xml + "name=\"" + name + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
}
    
void ServiceDefinitionBuilderImpl::add_interface_repository (
    const char * host,
    ::CORBA::Boolean load,
    ::CORBA::Boolean wait_load)
{
    services_definition_xml = services_definition_xml + "<interface_repository ";
    if (host != NULL && host[0] != '\0') services_definition_xml = services_definition_xml + "host=\"" + host + "\" ";
    services_definition_xml = services_definition_xml + "load=\"" + (load ? "true" : "false") + "\" ";
    services_definition_xml = services_definition_xml + "wait_load=\"" + (wait_load ? "true" : "false") + "\" ";
    services_definition_xml = services_definition_xml + "/>\n";
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

/************************** ACSServicesHandlerACS80Impl ****************************/

ACSServicesHandlerACS80Impl::ACSServicesHandlerACS80Impl () : h_name("ACS 8.0 Services Daemon"), h_type(::acsdaemon::servicesDaemonServiceName)
{
    cmdproc = tm.create<CommandProcessorThread>(h_name.c_str());
}

ACSServicesHandlerACS80Impl::~ACSServicesHandlerACS80Impl (void)
{
    ACS_SHORT_LOG((LM_DEBUG, "DESTROYING ACSServicesHandlerACS80Impl!"));
    tm.destroy(cmdproc);
}

std::string ACSServicesHandlerACS80Impl::getName ()
{
    return h_name;
}

std::string ACSServicesHandlerACS80Impl::getType(void)
{
    return h_type;
}

std::string ACSServicesHandlerACS80Impl::getPort(void)
{
    return ACSPorts::getServicesDaemonPort();
}

void ACSServicesHandlerACS80Impl::initialize(CORBA::ORB_ptr orb)
{
    m_orb = orb;
    ACS_SHORT_LOG((LM_DEBUG, "Initializing ACSServicesHandlerACS80Impl!"));
    acsQoS::init(orb);
    cmdproc->resume();
}

void ACSServicesHandlerACS80Impl::dispose(CORBA::ORB_ptr orb)
{
    ACS_SHORT_LOG((LM_DEBUG, "Disposing ACSServicesHandlerACS80Impl!"));
    cmdproc->exit();
    acsQoS::done();
}

/* CORBA interface */

char *prepareCommand(const char *command, short instance_number, bool wait, const char *name, const char *additional_command_line, bool log) {
    char buffer[64];
    sprintf(buffer, "%s -b %d", command, instance_number);
    ACE_CString commandline = buffer;
    if (wait) commandline = commandline + " -w";
    if (name != NULL) commandline = commandline + " -n " + name;
    if (additional_command_line != NULL) commandline = commandline + " " + additional_command_line;
    if (log) {
        ACE_CString logDirectory="~/.acs/commandcenter/";

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

        //create the directory
        ACE_OS::system(("mkdir -p " + logDirectory).c_str());
        commandline = commandline + " &> " + logDirectory + command + "_" + timeStamp.c_str();
    }
    return commandline.rep();
}

void execCommand(char *commandline, acsdaemon::DaemonCallback_ptr callback, CommandProcessorThread *cmdproc) {
    Request *newreq = new LocalRequest(callback, commandline);
    cmdproc->addRequest(newreq);
    acsdaemonErrType::AcsStartServicesCompletion ok(__FILE__, __LINE__, "execCommand");
    ACSErr::Completion_var comp = ok.returnCompletion(false);
    if (callback != NULL) {
        try {
            callback->working(comp.in());
        } catch (CORBA::TIMEOUT timeout) {
            ACS_SHORT_LOG((LM_WARNING, "Failed to make a 'working' callback call for local request!"));
        }
    }
}

void definitionXMLStartElementHandler(void *userData, const XML_Char *name, const XML_Char **atts) {
    RequestChainBuilder *rcb = (RequestChainBuilder*)userData;
    rcb->addRequest(name, atts);
}

::acsdaemon::ServiceDefinitionBuilder_ptr ACSServicesHandlerACS80Impl::create_service_definition_builder (
    ::CORBA::Short instance_number)
{
    return (new ServiceDefinitionBuilderImpl(instance_number))->_this();
}

void ACSServicesHandlerACS80Impl::start_services (
    const char * definition,
    ::CORBA::Boolean reuse_services,
    ::acsdaemon::DaemonSequenceCallback_ptr callback)
{
    RequestChainBuilder *rcb = new RequestChainBuilder(m_orb, true, cmdproc, reuse_services, callback);
    XML_Parser parser = XML_ParserCreate("UTF-8");
    XML_SetUserData(parser, (void*)rcb);
    XML_SetStartElementHandler(parser, &definitionXMLStartElementHandler);
    if (XML_Parse(parser, definition, strlen(definition), XML_TRUE) == XML_STATUS_ERROR) {
        ACS_SHORT_LOG ((LM_ERROR, "Failed to parse service definition XML!"));
    }
    XML_ParserFree(parser);
    rcb->startProcessing();
}
    
void ACSServicesHandlerACS80Impl::stop_services (
    const char * definition,
    ::acsdaemon::DaemonSequenceCallback_ptr callback)
{
    RequestChainBuilder *rcb = new RequestChainBuilder(m_orb, false, cmdproc, true, callback);
    XML_Parser parser = XML_ParserCreate("UTF-8");
    XML_SetUserData(parser, (void*)rcb);
    XML_SetStartElementHandler(parser, &definitionXMLStartElementHandler);
    if (XML_Parse(parser, definition, strlen(definition), XML_TRUE) == XML_STATUS_ERROR) {
        ACS_SHORT_LOG ((LM_ERROR, "Failed to parse service definition XML!"));
    }
    XML_ParserFree(parser);
    rcb->startProcessing();
}

void ACSServicesHandlerACS80Impl::start_naming_service (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Naming Service (instance %d).", instance_number));
    char *commandline = prepareCommand("acsNamingService", instance_number, true, NULL, "-s", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::start_notification_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting '%s' Notification Service (instance %d).", name == NULL ? "default" : name, instance_number));
    char *commandline = prepareCommand("acsNotifyService", instance_number, true, name, "-s", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::start_cdb (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Configuration Database (instance %d).", instance_number));
    char *commandline = prepareCommand("acsConfigurationDatabase", instance_number, true, NULL, "-s", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::start_manager (
    const char * domain,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Manager (instance %d).", instance_number));
    if (domain != NULL) {
        ACS_SHORT_LOG ((LM_WARNING, "Domain parameter of Manager startup is not supported!"));
    }
    char *commandline = prepareCommand("acsManager", instance_number, true, NULL, "-s", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::start_acs_log (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting ACS Log Service (instance %d).", instance_number));
    char *commandline = prepareCommand("acsACSLogService", instance_number, true, NULL, "-s", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::start_logging_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Logging Service '%s' (instance %d).", name, instance_number));
    if (name != NULL) {
        ACS_SHORT_LOG ((LM_WARNING, "Name parameter of Logging Service startup is not supported!"));
    }
    char *commandline = prepareCommand("acsLoggingService", instance_number, true, NULL, "-s", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::start_interface_repository (
    ::CORBA::Boolean load,
    ::CORBA::Boolean wait_load,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Starting Interface Repository (instance %d).", instance_number));
    char *commandline = prepareCommand("acsInterfaceRepository", instance_number, wait_load || !load, NULL, load ? "-s" : "-s -noloadIFR", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_naming_service (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Naming Service (instance %d).", instance_number));
    char *commandline = prepareCommand("acsNamingService", instance_number, true, NULL, "-k", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_notification_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping '%s' Notification Service (instance %d).", name == NULL ? "default" : name, instance_number));
    char *commandline = prepareCommand("acsNotifyService", instance_number, true, name, "-k", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_cdb (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Configuration Database (instance %d).", instance_number));
    char *commandline = prepareCommand("acsConfigurationDatabase", instance_number, true, NULL, "-k", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_manager (
    const char * domain,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Manager (instance %d).", instance_number));
    if (domain != NULL) {
        ACS_SHORT_LOG ((LM_WARNING, "Domain parameter of Manager shutdown is not supported!"));
    }
    char *commandline = prepareCommand("acsManager", instance_number, true, NULL, "-k", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_acs_log (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping ACS Log Service (instance %d).", instance_number));
    char *commandline = prepareCommand("acsACSLogService", instance_number, true, NULL, "-k", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_logging_service (
    const char * name,
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Logging Service '%s' (instance %d).", name, instance_number));
    if (name != NULL) {
        ACS_SHORT_LOG ((LM_WARNING, "Name parameter of Logging Service shutdown is not supported!"));
    }
    char *commandline = prepareCommand("acsLoggingService", instance_number, true, NULL, "-k", false);
    execCommand(commandline, callback, cmdproc);
}
    
void ACSServicesHandlerACS80Impl::stop_interface_repository (
    ::acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number)
{
    ACS_SHORT_LOG ((LM_INFO, "Stopping Interface Repository (instance %d).", instance_number));
    char *commandline = prepareCommand("acsInterfaceRepository", instance_number, true, NULL, "-k", false);
    execCommand(commandline, callback, cmdproc);
}

void
ACSServicesHandlerACS80Impl::start_acs (
    acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    char *commandline = prepareCommand("acsStart", instance_number, false, NULL, additional_command_line, true);
    execCommand(commandline, callback, cmdproc);
}


void
ACSServicesHandlerACS80Impl::stop_acs (
    acsdaemon::DaemonCallback_ptr callback,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    ACE_CString str = " -noShutdownLocalContainers";
    char *commandline = prepareCommand("acsStop", instance_number, false, NULL, (additional_command_line + str).c_str(), true);
    execCommand(commandline, callback, cmdproc);
}


char * ACSServicesHandlerACS80Impl::status_acs ( 
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
	    "::ACSServicesDaemonACS80Impl::status_acs").getFailedToGetAcsStatusEx();
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
	    "::ACSServicesDaemonACS80Impl::status_acs").getFailedToGetAcsStatusEx();
	}//if-else

    //acsStatus is deleted by CORBA
    return acsStatus;
}//ACSServicesHandlerACS80Impl::status_acs

void ACSServicesHandlerACS80Impl::shutdown ()
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
