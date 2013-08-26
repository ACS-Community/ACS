/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: acsservicesdaemonStop.cpp,v 1.3 2008/10/27 21:11:23 msekoran Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 */

/** @file acsservicesdaemonStop.cpp
 *  acsservicesdaemonStop is used to remotely stop ACS Services Deamon.
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
        {"instance",    required_argument, 0, 'i'},
        {"host",        required_argument, 0, 'H'},
        {"daemon",      required_argument, 0, 'd'},
        {0, 0, 0, '\0'}};

void 
usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h}", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -i, --instance     ACS instance to use\n");
    ACE_OS::printf ("\t   -H, --host         Host where to stop the daemon\n");
    ACE_OS::printf ("\t   -d, --daemon       Daemon reference\n");
}


int
main (int argc, char *argv[])
{
    int c;
    ACE_CString daemonRef;
    ACE_CString hostName;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "hd:H:",
                         long_options, &option_index); 
        if (c==-1) break;
        switch(c)
            {
                case 'h':
                    usage(argv[0]);
                    return 0;
                case 'd':
                    daemonRef = optarg;
                    break;
                case 'H':
                    hostName = optarg;
                    break;
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
      if (daemonRef.length() == 0)
	  {
          if(hostName.length() == 0)
          {
	      hostName = ACSPorts::getIP();
          } 
	  daemonRef = "corbaloc::";
	  daemonRef = daemonRef + hostName + ":" + ACSPorts::getServicesDaemonPort().c_str() + "/" + ::acsdaemon::servicesDaemonServiceName;
	  ACS_SHORT_LOG((LM_INFO, "Using local Services Daemon reference: '%s'", daemonRef.c_str()));
	  
	  }
      else
          {
          ACS_SHORT_LOG((LM_INFO, "Services Daemon reference obtained via command line: '%s'", daemonRef.c_str()));
          }


		CORBA::Object_var obj = orb->string_to_object(daemonRef.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", daemonRef.c_str()));
			return -1;
		}

		acsdaemon::ServicesDaemon_var daemon = acsdaemon::ServicesDaemon::_narrow(obj.in());
		if (CORBA::is_nil(daemon.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", daemonRef.c_str()));
			return -1;
		}


                ACS_SHORT_LOG((LM_INFO, "Calling shutdown()."));

                daemon->shutdown();

                ACS_SHORT_LOG((LM_INFO, "Daemon shutdown message issued."));


	}
	catch( maciErrType::NoPermissionEx &ex )
	{
		ACS_SHORT_LOG((LM_WARNING, "Daemon is running in protected mode and cannot be shut down remotely!\n"));
		return -1;
	}
	catch( CORBA::Exception &ex )
	{
		ACS_SHORT_LOG((LM_ERROR, "Failed."));
		ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
		return -1;
	}

	return 0;
}





