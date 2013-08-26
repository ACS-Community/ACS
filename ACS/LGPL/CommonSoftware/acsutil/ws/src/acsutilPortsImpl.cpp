#include <iomanip>
#include <tao/corba.h>

#include "acsutilPorts.h"

using namespace std;
/**
 * @return the "ACS Base Port".  This is just an integer ranging from 0-9.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
const unsigned long
ACSPorts::getBasePort()
{
    //Get access to the environment variable
    ACE_TCHAR *envRef = ACE_OS::getenv("ACS_INSTANCE");

    //Default the return value to 0 just in case env. var. is not set.
    unsigned long basePort = 0;
    
    if (envRef && *envRef)
	{
	//convert envRef to a long.
	basePort = atol(envRef);
	//delete envRef;
	}
    return basePort;
}

/**
 * @return the port manager is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string 
ACSPorts::getManagerPort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 0) << ends;
    return realOutput.str();
}

std::string 
ACSPorts::getManagerPort()
{
    return ACSPorts::getManagerPort(getBasePort());
}

/**
 * @return the port the CORBA Naming Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string 
ACSPorts::getNamingServicePort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 1) << ends;
    return realOutput.str();
}

std::string 
ACSPorts::getNamingServicePort()
{
    return ACSPorts::getNamingServicePort(getBasePort());
}

/**
 * @return the port the CORBA Notification Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string 
ACSPorts::getNotifyServicePort(int baseport, const char *name)
{
    std::ostringstream realOutput;
    int add = 2;
    if (name != NULL) {
        if (strcmp(name, "") == 0 || strcmp(name, "NotifyEventChannelFactory") == 0) add = 2;
        else if (strcmp(name, "Logging") == 0 || strcmp(name, "LoggingNotifyEventChannelFactory") == 0) add = 5;
        else if (strcmp(name, "Archive") == 0 || strcmp(name, "ArchiveNotifyEventChannelFactory") == 0) add = 6;
        else if (strcmp(name, "Alarm") == 0 || strcmp(name, "AlarmNotifyEventChannelFactory") == 0) add = 7;
        else {
            char cmd[200];
            snprintf(cmd, 200, "acsstartupNotifyPortViaErrorCode -b %d -name %s", baseport, name);
            int result = ACE_OS::system(cmd);
            if (result != -1)
                add = (result >> 8) + 20;
            // otherwise defult port is returned... hmmm?!
        }
    }
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + add) << ends;
    return realOutput.str();
}

std::string 
ACSPorts::getNotifyServicePort()
{
    return ACSPorts::getNotifyServicePort(getBasePort());
}

/**
 * @return the port the CORBA Logging Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getLoggingServicePort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 3) << ends;
    return realOutput.str();   
}

std::string 
ACSPorts::getLoggingServicePort()
{
    return ACSPorts::getLoggingServicePort(getBasePort());
}

/**
 * @return the port the CORBA Interface Repository is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getIRPort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 4) << ends;
    return realOutput.str();   
}

std::string 
ACSPorts::getIRPort()
{
    return ACSPorts::getIRPort(getBasePort());
}

/**
 * @return the port the ACS Logging Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getLogPort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 11) << ends;
    return realOutput.str();
}

std::string 
ACSPorts::getLogPort()
{
    return ACSPorts::getLogPort(getBasePort());
}

/**
 * @return the port the ACS CDB is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getCDBPort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 12) << ends;
    return realOutput.str();
}

std::string 
ACSPorts::getCDBPort()
{
    return ACSPorts::getCDBPort(getBasePort());
}

/**
 * @return the port the ACS Alarm Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getAlarmServicePort(int baseport)
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (baseport*100 + 3000 + 13) << ends;    return realOutput.str();
}

std::string
ACSPorts::getAlarmServicePort()
{
    return ACSPorts::getAlarmServicePort(getBasePort());
}


/**
 * @return the port the ACS Container Daemon is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getContainerDaemonPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (2970) << ends;
    return realOutput.str();
}

/**
 * @return the port the ACS Services Daemon is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getServicesDaemonPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (2980) << ends;
    return realOutput.str();
}

/**
 * @return the IP address of this host.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
const char* 
ACSPorts::getIP()
{
    ACE_TCHAR hostname[200]; 
    hostname[0] = 0;

    //get the real hostname
    ACE_OS::hostname(hostname, 200);

    //garbage like this is why I'm glad we use CORBA instead of sockets;)
    return ACE_OS::inet_ntoa(*(reinterpret_cast<in_addr*>(ACE_OS::gethostbyname(hostname)->h_addr)));
}
