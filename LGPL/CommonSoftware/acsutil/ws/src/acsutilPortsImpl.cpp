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
ACSPorts::getManagerPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 0) << ends;
    return realOutput.str();
}

/**
 * @return the port the CORBA Naming Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string 
ACSPorts::getNamingServicePort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 1) << ends;
    return realOutput.str();
}

/**
 * @return the port the CORBA Notification Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string 
ACSPorts::getNotifyServicePort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 2) << ends;
    return realOutput.str();
}

/**
 * @return the port the CORBA Logging Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getLoggingServicePort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 3) << ends;
    return realOutput.str();   
}

/**
 * @return the port the CORBA Interface Repository is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getIRPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 4) << ends;
    return realOutput.str();   
}

/**
 * @return the port the ACS Logging Service is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getLogPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 11) << ends;
    return realOutput.str();
}

/**
 * @return the port the ACS CDB is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getCDBPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 12) << ends;
    return realOutput.str();
}

/**
 * @return the port the ACS Daemon is running on.
 * @htmlonly
 * <br><hr>
 * @endhtmlonly
 */
std::string
ACSPorts::getDaemonPort()
{
    std::ostringstream realOutput;
    realOutput << setw(4) << setfill('0') << (getBasePort()*100 + 3000 + 13) << ends;
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
