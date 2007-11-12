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





