/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * rbourtem  2013-10-11  created 
 */

/* 
 * System Headers 
 */
#include <sstream>

/*
 * Local Headers 
 */
#include "acsdaemonTestAsyncStopRequest.h"
#include "acsutilPorts.h"
#include "acsutilORBHelper.h"
#include "maciHelper.h"
#include <maciSimpleClient.h>
#include "acsQoStimeout.h"

/* 
 * Signal catching functions  
 */

/* 
 *Local functions  
 */
 
using namespace std;
using namespace maci;

TestAcsDaemonAsyncStopRequest::TestAcsDaemonAsyncStopRequest()

{
  m_acsInstance = ACSPorts::getBasePort();
  m_managerCorbaloc = getManagerCorbaloc_();
  m_additionalCmdLine = "-m " + m_managerCorbaloc;
}

TestAcsDaemonAsyncStopRequest::~TestAcsDaemonAsyncStopRequest()
{
}

/*
 * return the manager reference as a corbaloc
 * ex: "corbaloc::134.171.12.33:3000/Manager"
 */
string TestAcsDaemonAsyncStopRequest::getManagerCorbaloc_()
{
	ACE_CString managerHost = maci::MACIHelper::getManagerHostname(0,0);
	string managerLoc = "corbaloc::";
	managerLoc = managerLoc + managerHost.c_str() + ":" + ACSPorts::getManagerPort(m_acsInstance).c_str() + "/Manager";
	return managerLoc;
}

void TestAcsDaemonAsyncStopRequest::run(const string &container, const string &host)
{
	completed_m = false;

	host_m = host;
	container_m = container;

	// No error string
	exceptionString_m = "";

	// Create the new thread
	int status = activate (THR_NEW_LWP | THR_JOINABLE, 1);

	if (status == 0)
	{
		// everything is OK
		return;
	}

	if (status < 0)
	{
		ACS_SHORT_LOG((LM_ERROR,"Failed to activate thread (-1) for host %s container %s",host_m.c_str(),container_m.c_str()));
	}
	else if (status > 0)
	{
		ACS_SHORT_LOG((LM_ERROR,"Failed to activate thread (+1) for host %s container %s",host_m.c_str(), container_m.c_str()));
	}
}

int TestAcsDaemonAsyncStopRequest::wait()
{
	return (ACE_Task_Base::wait());
}

int TestAcsDaemonAsyncStopRequest::svc()
{
	commandOK_m = true;
	// Invoke the relevant method
	int ret = stopContainerOnHost(host_m, container_m);
	if (ret < 0)
	{
		commandOK_m = false;
		return ret;
	}
	// completed
	completed_m = true;
	return ret;
}

int TestAcsDaemonAsyncStopRequest::stopContainerOnHost(string &host, string &containerName) throw (CORBA::SystemException,acsdaemonErrType::FailedToStopContainerEx,ACSErrTypeCommon::BadParameterEx)
{
  // Get CORBA reference of ACS Container daemon running on this host
	acsdaemon::ContainerDaemon_var  containerDaemon = getContainerDaemonRef_(host);
	if(CORBA::is_nil(containerDaemon.in()))
	{
		ACS_SHORT_LOG((LM_ERROR,"Failed to get containerdaemon reference"));
		return -5;
	}
	//SPARTA_LOG_DEBUG("Stopping container %s on %s",containerName.c_str(),host);
	try
	{
		// Use stop_container synch version 
		containerDaemon->stop_container_sync(containerName.c_str(),m_acsInstance,m_additionalCmdLine.c_str());
	}
	catch(acsdaemonErrType::FailedToStopContainerEx &ex)
	{
		ACS_SHORT_LOG((LM_ERROR,"Failed to stop %s container on %s",containerName.c_str(),host.c_str()));
		return -1;
	}
	catch(ACSErrTypeCommon::BadParameterEx &ex)
	{
		ACS_SHORT_LOG((LM_ERROR,"Failed to stop %s container on %s. Bad parameters!",containerName.c_str(),host.c_str()));
		return -2;
	}
	catch(CORBA::Exception &ex )
	{
		stringstream exSs;
		exSs << ex << ends;
		ACS_SHORT_LOG((LM_ERROR,"Failed to stop %s container on %s",containerName.c_str(),host.c_str()));
		ACS_SHORT_LOG((LM_ERROR,"Caught exception %s",exSs.str().c_str()));
		return -3;
	}
	return 0;
}

/*
 * Get the CORBA reference of the containerdaemon CORBA object running
 * on the host given as parameter
 */
acsdaemon::ContainerDaemon_ptr TestAcsDaemonAsyncStopRequest::getContainerDaemonRef_(const string & host)
{
	acsdaemon::ContainerDaemon_var containerDaemon;
	// Create the corbaloc for the daemon running on this host
	string daemonRef = "corbaloc::";
	daemonRef = daemonRef + host + ":" + ACSPorts::getContainerDaemonPort().c_str() + "/" + ::acsdaemon::containerDaemonServiceName;
	// Get CORBA reference of ACS Container daemon running on this host
	try
	{
		CORBA::ORB_ptr orb = ORBHelper::getORB();
		CORBA::Object_var obj = orb->string_to_object(daemonRef.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'",daemonRef.c_str()));
			return 0;
		}
		else
		{
			if (obj->_is_a("IDL:alma/acsdaemon/ContainerDaemon:1.0"))
			{
				// Container Daemon with that reference is up and running
				containerDaemon = acsdaemon::ContainerDaemon::_narrow(obj.in());
			}
			else
			{
				ACS_SHORT_LOG((LM_ERROR, "Reference '%s' has been resolved but does not refer to Container Daemon instance", 
				     daemonRef.c_str()));
				return 0;
			}
		}
	}
	catch( CORBA::OBJECT_NOT_EXIST &ex )
	{
		ACS_SHORT_LOG((LM_ERROR,"acscontainerdaemon on host %s is already running but object with reference '%s' does not exist",
		               host.c_str(), daemonRef.c_str()));
		return 0;
	}
	catch( CORBA::TRANSIENT &ex )
	{
		ACS_SHORT_LOG((LM_ERROR,"No Container Daemon has been found on host %s", host.c_str()));
		return 0;
	}
	catch( CORBA::Exception &ex )
	{
		ACS_SHORT_LOG((LM_ERROR,"Failed to resolve reference '%s'", daemonRef.c_str()));
		stringstream exSs;
		exSs << ex << ends;
		ACS_SHORT_LOG((LM_ERROR,"Caught exception %s",exSs.str().c_str()));
		return 0;
	}
	return containerDaemon._retn();
}

int shutdownContainers(vector<string> containers, string host) throw (CORBA::SystemException,acsdaemonErrType::FailedToStopContainerEx,ACSErrTypeCommon::BadParameterEx)
{
	// iterate even if there is an error
	bool errorFound = false;

	/*
	 * create a vector of async stop objects. when run these will
	 * create a thread which calls the stopObjectOnHost method
	 * we then wait for the threads to complete
	 */
	vector<TestAcsDaemonAsyncStopRequest *> asyncStop;
	asyncStop.resize(containers.size());
	for (unsigned int i = 0; i < containers.size(); i++)
	{
	    asyncStop[i] = new TestAcsDaemonAsyncStopRequest;
	    if (asyncStop[i] == NULL)
		{
			ACS_SHORT_LOG((LM_ERROR,"Failed to allocate asyncStopRequest number %u", i));
			return -1;
		}
	}
	for (unsigned int i = 0; i < containers.size(); i++)
	{
	    asyncStop[i]->run (containers[i], host);
	}
	for (unsigned int i = 0; i < containers.size(); i++)
	{
	    asyncStop[i]->wait();
	    if (!asyncStop[i]->commandOK())
	    {
	      errorFound = true;
	    }
	}
	for (unsigned int i = 0; i < containers.size(); i++)
	{
	    delete asyncStop[i];
	}

	if (errorFound)
	{
	  return -2;
	}

	return 0;
}

int main(int argc, char* argv[])
{
	SimpleClient client;
	
	if (client.init(argc,argv) == 0)
	{
	    return -1;
	} 
	else
	{
	    // Log into the manager before doing anything
	    client.login();
	}

	//Initialise ACE library
	
	int orbTimeoutMs = 12000; // 12 sec
    
	// Set the ORB Timeout
	try
	{
		CORBA::ORB_ptr orb = ORBHelper::getORB();
		acsQoS::Timeout::setORBTimeout(orbTimeoutMs,orb);
	}
	catch( CORBA::Exception &ex )
	{
		ACS_SHORT_LOG((LM_ERROR,"Caught CORBA exception while setting ORB timeout to %d ms",orbTimeoutMs));
		stringstream exSs;
		exSs << ex << ends;
		ACS_SHORT_LOG((LM_ERROR,"Caught exception %s",exSs.str().c_str()));
	}
	string hostname = getenv("HOST");
	vector<string> containers;
	containers.push_back("slowComponentContainer");
	containers.push_back("slowComponentContainer2");
    int ret = shutdownContainers(containers,hostname);
    if(ret != 0)
    {
        ACS_SHORT_LOG((LM_ERROR,"Error while shutting down slowComponentContainer and slowComponentContainer2 containers"));
		return -2;
    }
  
	return 0;  
}

