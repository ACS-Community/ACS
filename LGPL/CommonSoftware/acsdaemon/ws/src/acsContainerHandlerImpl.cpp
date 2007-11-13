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
* "@$Id: acsContainerHandlerImpl.cpp,v 1.4 2007/11/13 19:49:30 agrimstrup Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2006-06-21 created
* agrimstr 2007-11-07 extracted container interface implementation to separate
*                     class
*/

#include "acsContainerHandlerImpl.h"

/*****************************************************************/

ACSContainerHandlerImpl::ACSContainerHandlerImpl () : h_name("ACS Container Daemon"), h_type("ACSContainerDaemon")
{
}

ACSContainerHandlerImpl::~ACSContainerHandlerImpl (void)
{
}

const char* ACSContainerHandlerImpl::getName(void)
{
    return h_name.c_str();
}

const char* ACSContainerHandlerImpl::getType(void)
{
    return h_type.c_str();
}


const char* ACSContainerHandlerImpl::getPort(void)
{
    return ACSPorts::getContainerDaemonPort().c_str();
}

/************************** CORBA interface ****************************/

void
ACSContainerHandlerImpl::start_container (
    const char * container_type,
    const char * container_name,
    ::CORBA::Short instance_number,
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

    // execute: "acsStartContainer -<type> -b <instance> <name> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    
    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
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

    std::string timeStamp(getStringifiedTimeStamp().c_str());

    char command[1000];

    // This nasty hack was put so that the ARCHIVE could modify the CLASSPATH to add
    // some additional jar files.  The container_type is being used as the driver
    // but this will be changed to something cleaner.
    if (!strcmp(container_type,"java-archive"))
	snprintf(command, 1000, "acsStartContainerOracleClasspath -%s -b %d %s %s &> %sacsStartContainer_%s_%s&", "java", instance_number, container_name, cmdln, logDirectory.c_str(), containerName.c_str(), timeStamp.c_str());
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
ACSContainerHandlerImpl::stop_container (
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

    std::string timeStamp(getStringifiedTimeStamp().c_str());

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





