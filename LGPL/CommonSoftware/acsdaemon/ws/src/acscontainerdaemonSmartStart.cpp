/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: acscontainerdaemonSmartStart.cpp,v 1.1 2008/06/30 07:38:28 msekoran Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 */

/** @file acsutilSmartContainerDaemonStart.cpp
 *  acsutilSmartContainerDaemonStart is used to start ACS Container Deamon.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */   


#include <acsutilPorts.h>
#include <logging.h>
#include <acsdaemonC.h>
#include <ACSErrTypeCommon.h>
#include <acsdaemonErrType.h>
#include <maciErrType.h>
#include <getopt.h>

static struct option long_options[] = {
        {"help",        no_argument,       0, 'h'},
        {0, 0, 0, '\0'}};

void 
usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h}", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
}


int
main (int argc, char *argv[])
{
    int c;
    ACE_CString hostName;
    ACE_CString daemonRef;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "h",
                         long_options, &option_index); 
        if (c==-1) break;
        switch(c)
            {
                case 'h':
                    usage(argv[0]);
                    return 0;
            }
        }

	LoggingProxy * logger = new LoggingProxy(0, 0, 31);
	if (logger)
	{
		LoggingProxy::init(logger);
		LoggingProxy::ProcessName(argv[0]);
		LoggingProxy::ThreadName("main");
	}
	else
		ACS_SHORT_LOG((LM_INFO, "Failed to initialize logging."));

	try
	{
		// Initialize the ORB.
		CORBA::ORB_var orb = CORBA::ORB_init (argc,argv,"TAO");

		// construct default one
		if(hostName.length() == 0)
		{
			hostName = ACSPorts::getIP();
		}
		daemonRef = "corbaloc::";
		daemonRef = daemonRef + hostName + ":" + ACSPorts::getContainerDaemonPort().c_str() + "/" + ::acsdaemon::containerDaemonServiceName;
		ACS_SHORT_LOG((LM_INFO, "Using local Container Daemon reference: '%s'", daemonRef.c_str()));


		CORBA::Object_var obj = orb->string_to_object(daemonRef.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", daemonRef.c_str()));
			return -1;
		}
		else
		{
			if (obj->_is_a("IDL:alma/acsdaemon/ContainerDaemon:1.0"))
			{
				ACS_SHORT_LOG((LM_INFO, "Container Daemon with reference '%s' is already up and running.", daemonRef.c_str()));
				return 0;
			}
			else
			{
				ACS_SHORT_LOG((LM_ERROR, "Reference '%s' has been resolved but does not refer to Container Daemon instance.", daemonRef.c_str()));
				return -1;
			}
		}


	}
	catch( CORBA::OBJECT_NOT_EXIST &ex )
	{
		ACS_SHORT_LOG((LM_ERROR, "Server is already running but object with reference '%s' does not exist.", daemonRef.c_str()));
		return -1;
	}
	catch( CORBA::TRANSIENT &ex )
	{
		ACS_SHORT_LOG((LM_INFO, "No Container Daemon has been found. Trying to start a new instance."));
		system("acscontainerdaemon -u &");
		return 0;
	}
	catch( CORBA::Exception &ex )
	{
		ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", daemonRef.c_str()));
		ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
		return -1;
	}

	return -1;
}





