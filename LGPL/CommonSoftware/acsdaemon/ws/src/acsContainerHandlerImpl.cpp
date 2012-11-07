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
* "@$Id: acsContainerHandlerImpl.cpp,v 1.23 2012/11/07 14:01:09 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
* agrimstr 2007-11-07 extracted container interface implementation to separate
*                     class
*/

#include "acsContainerHandlerImpl.h"
#include <unistd.h>

/*****************************************************************/

ACSContainerHandlerImpl::ACSContainerHandlerImpl () : h_name("ACS Container Daemon"), h_type(::acsdaemon::containerDaemonServiceName)
{
}

ACSContainerHandlerImpl::~ACSContainerHandlerImpl (void)
{
}

std::string ACSContainerHandlerImpl::getName(void)
{
    return h_name;
}

std::string ACSContainerHandlerImpl::getType(void)
{
    return h_type;
}


std::string ACSContainerHandlerImpl::getPort(void)
{
    return ACSPorts::getContainerDaemonPort();
}

/************************** CORBA interface ****************************/

void
ACSContainerHandlerImpl::start_container (
    const char * container_type,
    const char * container_name,
    ::CORBA::Short instance_number,
    const ::ACS::stringSeq & type_modifiers,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    if (container_type == 0 ||
	*container_type == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__, 
						  "::ACSContainerDaemonImpl::start_container");
	ex.setParameter("container_type");
	throw ex.getBadParameterEx();
	}

    if (container_name == 0 ||
	*container_name == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__, 
						  "::ACSContainerDaemonImpl::start_container");
	ex.setParameter("container_name");
	throw ex.getBadParameterEx();
	}

    const char * cmdln = (additional_command_line ? additional_command_line : "");

	int isCasaContainer = 0;
	for (unsigned int i = 0; i < type_modifiers.length(); ++i) {
		if (!strcmp(type_modifiers[i], "casaContainer")) {
			isCasaContainer = 1;
			break;
		}

	}

    // execute: "acsStartContainer -<type> -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    
    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    char * acsdata = ACE_OS::getenv("ACSDATA");
    if(acsdata != 0)
        logDirectory = std::string(acsdata) + std::string("/logs/");
    std::string logs = logDirectory;
    char * host = ACE_OS::getenv("HOST");
    if(host != NULL)
        logDirectory = logDirectory + std::string(host) + std::string("/");
    std::string containerName(container_name);
    std::string::size_type pos=containerName.rfind("/"); 
    if(pos != std::string::npos){
    	logDirectory.append(containerName,0,pos+1);
    	containerName.erase(0,pos+1);
    }
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());
    std::string chmod("chmod 775 ");
    chmod.append(logDirectory);
    ACE_OS::system(chmod.c_str());
    ACE_OS::system(("chmod 775 " + logs).c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

	char command[1000];

	if (isCasaContainer)
		snprintf(command, 1000, "acsStartContainerWithCASA -%s -b %d %s %s &> %sacsStartContainer_%s_%s&", container_type, instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());
	else
		snprintf(command, 1000, "acsStartContainer -%s -b %d %s %s &> %sacsStartContainer_%s_%s&", container_type, instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());

	ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStartContainerExImpl(
	    __FILE__, __LINE__, 
	    "::ACSContainerDaemonImpl::start_container").getFailedToStartContainerEx();
	}
   
}

void
ACSContainerHandlerImpl::start_container_sync (
    const char * container_type,
    const char * container_name,
    ::CORBA::Short instance_number,
    const ::ACS::stringSeq & type_modifiers,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    if (container_type == 0 ||
	*container_type == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__,
						  "::ACSContainerDaemonImpl::start_container_sync");
	ex.setParameter("container_type");
	throw ex.getBadParameterEx();
	}

    if (container_name == 0 ||
	*container_name == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__,
						  "::ACSContainerDaemonImpl::start_container_sync");
	ex.setParameter("container_name");
	throw ex.getBadParameterEx();
	}

    const char * cmdln = (additional_command_line ? additional_command_line : "");

	int isCasaContainer = 0;
	for (unsigned int i = 0; i < type_modifiers.length(); ++i) {
		if (!strcmp(type_modifiers[i], "casaContainer")) {
			isCasaContainer = 1;
			break;
		}

	}

    // execute: "acsStartContainer -<type> -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!

    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    char * acsdata = ACE_OS::getenv("ACSDATA");
    if(acsdata != 0)
        logDirectory = std::string(acsdata) + std::string("/logs/");
    std::string logs = logDirectory;
    char * host = ACE_OS::getenv("HOST");
    if(host != NULL)
        logDirectory = logDirectory + std::string(host) + std::string("/");
    std::string containerName(container_name);
    std::string::size_type pos=containerName.rfind("/");
    if(pos != std::string::npos){
    	logDirectory.append(containerName,0,pos+1);
    	containerName.erase(0,pos+1);
    }
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());
    std::string chmod("chmod 775 ");
    chmod.append(logDirectory);
    ACE_OS::system(chmod.c_str());
    ACE_OS::system(("chmod 775 " + logs).c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

	char command[1000];

	if (isCasaContainer)
		snprintf(command, 1000, "acsStartContainerWithCASA -%s -b %d %s %s &> %sacsStartContainer_%s_%s", container_type, instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());
	else
		snprintf(command, 1000, "acsStartContainer -%s -b %d %s %s &> %sacsStartContainer_%s_%s", container_type, instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());

	ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    ACS_SHORT_LOG ((LM_INFO, "Command: '%s' has been executed (%d).", command, result));

    if (result < 0)
    {
    	::acsdaemonErrType::FailedToStartContainerExImpl ex(
    			__FILE__, __LINE__,
    			"::ACSContainerDaemonImpl::start_container_sync");
    	ex.setReturnCode(result);
    	ex.log(LM_DEBUG);
    	throw ex.getFailedToStartContainerEx();
    }

}//start_container_sync



void ACSContainerHandlerImpl::stop_container (
    const char * container_name,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStopContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    if (container_name == 0 ||
	*container_name == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__,
						  "::ACSContainerDaemonImpl::stop_container");
	ex.setParameter("container_name");
	throw ex.getBadParameterEx();
	}

    const char * cmdln = (additional_command_line ? additional_command_line : "");

    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    char * acsdata = ACE_OS::getenv("ACSDATA");
    if(acsdata != 0)
        logDirectory = std::string(acsdata) + std::string("/logs/");
    std::string logs = logDirectory;
    char * host = ACE_OS::getenv("HOST");
    if(host != NULL)
        logDirectory = logDirectory + std::string(host) + std::string("/");
    std::string containerName(container_name);
    std::string::size_type pos=containerName.rfind("/");
    if(pos != std::string::npos){
    	logDirectory.append(containerName,0,pos+1);
    	containerName.erase(0,pos+1);
    }
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());
    std::string chmod("chmod 775 ");
    chmod.append(logDirectory);
    ACE_OS::system(chmod.c_str());
    ACE_OS::system(("chmod 775 " + logs).c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

    // execute: "acsStopContainer -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[1000];
    snprintf(command, 1000, "acsStopContainer -b %d %s %s &> %sacsStopContainer_%s_%s&", instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStopContainerExImpl(
	    __FILE__, __LINE__,
	    "::ACSContainerDaemonImpl::stop_container").getFailedToStopContainerEx();
	}

}


void ACSContainerHandlerImpl::stop_container_sync (
    const char * container_name,
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStopContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    if (container_name == 0 ||
	*container_name == 0)
	{
	::ACSErrTypeCommon::BadParameterExImpl ex(__FILE__, __LINE__, 
						  "::ACSContainerDaemonImpl::stop_container_sync");
	ex.setParameter("container_name");
	throw ex.getBadParameterEx();
	}

    const char * cmdln = (additional_command_line ? additional_command_line : "");

    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    char * acsdata = ACE_OS::getenv("ACSDATA");
    if(acsdata != 0)
        logDirectory = std::string(acsdata) + std::string("/logs/");
    std::string logs = logDirectory;
    char * host = ACE_OS::getenv("HOST");
    if(host != NULL)
        logDirectory = logDirectory + std::string(host) + std::string("/");
    std::string containerName(container_name);
    std::string::size_type pos=containerName.rfind("/"); 
    if(pos != std::string::npos){
    	logDirectory.append(containerName,0,pos+1);
    	containerName.erase(0,pos+1);
    }
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());
    std::string chmod("chmod 775 ");
    chmod.append(logDirectory);
    ACE_OS::system(chmod.c_str());
    ACE_OS::system(("chmod 775 " + logs).c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    if( timeStamp.find(":") != std::string::npos)
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find(":") != std::string::npos )
        timeStamp.replace(timeStamp.find(":"),1,".");
    if( timeStamp.find("T") != std::string::npos)
        timeStamp.replace(timeStamp.find("T"),1,"_");

    // execute: "acsStopContainer -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[1000];
    snprintf(command, 1000, "acsStopContainer -b %d %s %s &> %sacsStopContainer_%s_%s", instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    ACS_SHORT_LOG ((LM_INFO, "Command: '%s' has been executed (%d).", command, result));

    if (result < 0)
    {
    	::acsdaemonErrType::FailedToStopContainerExImpl ex(
    			__FILE__, __LINE__,
    			"::ACSContainerDaemonImpl::stop_container_sync");
    	ex.setReturnCode(result);
    	ex.log(LM_DEBUG);
    	throw ex.getFailedToStopContainerEx();
    }
}//stop_container_sync



void
ACSContainerHandlerImpl::shutdown ()
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::maciErrType::NoPermissionEx
    ))
{
	if (h_service->isProtected())
	{
	throw ::maciErrType::NoPermissionEx();
	}
	ACS_SHORT_LOG ((LM_INFO, "Shutting down the ACS Container Daemon on remote request..."));
	h_service->shutdown(false);
}


