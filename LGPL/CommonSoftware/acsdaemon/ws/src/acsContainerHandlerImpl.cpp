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
    throw(
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			)
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

	int isFortanContainer = 0;
	for (unsigned int i = 0; i < type_modifiers.length(); ++i) {
		if (!strcmp(type_modifiers[i], "fortranContainer")) {
			isFortanContainer = 1;
			break;
		}
	}

    // execute: "acsStartContainer -<type> -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    
    //get the directory name to store the container stdout
	std::string containerName(container_name);
	std::string logDirectory=m_daemonUtils.getLogDirectoryForContainer(containerName);

	char command[1000];

	if (isFortanContainer)
		snprintf(
				command,
				1000,
				"acsStartContainerWithFortran -%s -b %d %s %s &> %sacsStartContainer_%s_%s&",
				container_type,
				instance_number,
				container_name,
				cmdln,
				logDirectory.c_str(),
				m_daemonUtils.getSimpleContainerName(containerName).c_str(),
				m_daemonUtils.getTimestamp().c_str());
	else
		snprintf(
				command,
				1000,
				"acsStartContainer -%s -b %d %s %s &> %sacsStartContainer_%s_%s&",
				container_type,
				instance_number,
				container_name,
				cmdln,
				logDirectory.c_str(),
				m_daemonUtils.getSimpleContainerName(containerName).c_str(),
				m_daemonUtils.getTimestamp().c_str());

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
    throw(
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			)
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

	int isFortranContainer = 0;
	for (unsigned int i = 0; i < type_modifiers.length(); ++i) {
		if (!strcmp(type_modifiers[i], "fortranContainer")) {
			isFortranContainer = 1;
			break;
		}

	}

    // execute: "acsStartContainer -<type> -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!

    //get the directory name to store the container stdout
	std::string containerName(container_name);
	std::string logDirectory=m_daemonUtils.getLogDirectoryForContainer(containerName);

	char command[1000];

	if (isFortranContainer)
		snprintf(
				command,
				1000,
				"acsutilAwaitContainerStart -F -%s -b %d %s %s &> %sacsStartContainer_%s_%s",
				container_type,
				instance_number,
				container_name,
				cmdln,
				logDirectory.c_str(),
				m_daemonUtils.getSimpleContainerName(containerName).c_str(),
				m_daemonUtils.getTimestamp().c_str());
	else
		snprintf(
				command,
				1000,
				"acsutilAwaitContainerStart -%s -b %d %s %s &> %sacsStartContainer_%s_%s",
				container_type,
				instance_number,
				container_name,
				cmdln,
				logDirectory.c_str(),
				m_daemonUtils.getSimpleContainerName(containerName).c_str(),
				m_daemonUtils.getTimestamp().c_str());

	ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    ACS_SHORT_LOG ((LM_INFO, "Command: '%s' has been executed (%d).", command, result));

    if (result != 0)
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
    throw(
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStopContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			)
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
    std::string containerName(container_name);
    std::string logDirectory=m_daemonUtils.getLogDirectoryForContainer(containerName);

    // execute: "acsStopContainer -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[1000];
    snprintf(
    		command,
    		1000,
    		"acsStopContainer -b %d %s %s &> %sacsStopContainer_%s_%s&",
    		instance_number,
    		container_name,
    		cmdln,
    		logDirectory.c_str(),
    		m_daemonUtils.getSimpleContainerName(containerName).c_str(),
    		m_daemonUtils.getTimestamp().c_str());

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
    throw(
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStopContainerEx,
			::ACSErrTypeCommon::BadParameterEx
			)
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
    std::string containerName(container_name);
    std::string logDirectory=m_daemonUtils.getLogDirectoryForContainer(containerName);

    // execute: "acsStopContainer -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    char command[1000];
    snprintf(
    		command,
    		1000,
    		"acsStopContainer -b %d %s %s &> %sacsStopContainer_%s_%s",
    		instance_number,
    		container_name,
    		cmdln,
    		logDirectory.c_str(),
    		m_daemonUtils.getSimpleContainerName(containerName).c_str(),
    		m_daemonUtils.getTimestamp().c_str());

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
    throw(
			CORBA::SystemException,
			::maciErrType::NoPermissionEx
    )
{
	if (h_service->isProtected())
	{
	throw ::maciErrType::NoPermissionEx();
	}
	ACS_SHORT_LOG ((LM_INFO, "Shutting down the ACS Container Daemon on remote request..."));
	h_service->shutdown(false);
}


