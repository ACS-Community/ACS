#include "acsServicesHandlerImpl.h"

/*****************************************************************/

ACSServicesHandlerImpl::ACSServicesHandlerImpl () : h_name("ACS Services Daemon"), h_type("ACSServicesDaemon")
{
}

ACSServicesHandlerImpl::~ACSServicesHandlerImpl (void)
{
}

const char* ACSServicesHandlerImpl::getName ()
{
    return h_name.c_str();
}

const char* ACSServicesHandlerImpl::getType(void)
{
    return h_type.c_str();
}

const char* ACSServicesHandlerImpl::getPort(void)
{
    return ACSPorts::getServicesDaemonPort().c_str();
}


/************************** CORBA interface ****************************/

void
ACSServicesHandlerImpl::start_acs (
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStartAcsEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{

    const char * cmdln = (additional_command_line ? additional_command_line : "");

    // execute: "acsStart  -b <instance> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!

    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());


    char command[1000];
    snprintf(command, 1000, "acsStart -b %d %s &> %sacsStart_%s&", instance_number, cmdln, logDirectory.c_str(), timeStamp.c_str());

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStartAcsExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::start_acs").getFailedToStartAcsEx();
	}
}



void
ACSServicesHandlerImpl::stop_acs (
    ::CORBA::Short instance_number,
    const char * additional_command_line
    )
    ACE_THROW_SPEC ((
			CORBA::SystemException,
			::acsdaemonErrType::FailedToStopAcsEx,
			::ACSErrTypeCommon::BadParameterEx
			))
{
    const char * cmdln = (additional_command_line ? additional_command_line : "");

    //get the directory name to store the container stdout
    std::string logDirectory="~/.acs/commandcenter/";
    
    //create the directory
    std::string mkdir("mkdir -p ");
    mkdir.append(logDirectory);
    ACE_OS::system(mkdir.c_str());

    std::string timeStamp(getStringifiedTimeStamp().c_str());


    char command[1000];
    // execute: "acsStop -b <instance> <args>"
    // TODO checks for ';', '&', '|' chars, they can run any other command!
    snprintf(command, 1000, "acsStop -b %d %s &> %sacsStop_%s&", instance_number, cmdln, logDirectory.c_str(), timeStamp.c_str());

    ACS_SHORT_LOG ((LM_INFO, "Executing: '%s'.", command));

    int result = ACE_OS::system(command);

    if (result < 0)
	{
	throw ::acsdaemonErrType::FailedToStopAcsExImpl(
	    __FILE__, __LINE__, 
	    "::ACSServicesDaemonImpl::stop_acs").getFailedToStopAcsEx();
	}
   
}//ACSServicesDaemonImpl::stop_acs

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
