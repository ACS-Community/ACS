/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: acsdaemonStopContainer.cpp,v 1.9 2008/06/27 11:41:07 msekoran Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 */

/** @file acsdaemonStopContainer.cpp
 *  acsdaemonStopContainer is used to remotely stop container via ACS Deamon.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */   


#include <acsutilPorts.h>
#include <logging.h>
#include <acsdaemonC.h>
#include <ACSErrTypeCommon.h>
#include <acsdaemonErrType.h>
#include <getopt.h>

static struct option long_options[] = {
        {"help",        no_argument,       0, 'h'},
        {"container",   required_argument, 0, 'c'},
        {"instance",    required_argument, 0, 'i'},
        {"host",        required_argument, 0, 'H'},
        {"daemon",      required_argument, 0, 'd'},
        {"additional",   required_argument, 0, 'a'},
        {0, 0, 0, '\0'}};

void 
usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h} -i INSTANCE -t TYPE -c CONTAINER [-d DAEMONREF] [-H HOST] [-a more options]", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -c, --container     container name\n");
    ACE_OS::printf ("\t   -i, --instance     ACS instance to use\n");
    ACE_OS::printf ("\t   -H, --host         Host where to stop the container\n");
    ACE_OS::printf ("\t   -d, --daemon       Daemon reference\n");
    ACE_OS::printf ("\t   -a, --additional    passthrough options for stopContaner. Put options between \"\"\n");
}


int
main (int argc, char *argv[])
{
    int c, instance = -1;
    ACE_CString daemonRef;
    ACE_CString hostName;
    ACE_CString containerName;
    ACE_CString additional;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "hc:i:d:H:a:",
                         long_options, &option_index); 
        if (c==-1) break;
        switch(c)
            {
                case 'h':
                    usage(argv[0]);
                    return 0;
                case 'i':
                    instance = ACE_OS::atoi(optarg);
                    break;
                case 'd':
                    daemonRef = optarg;
                    break;
                case 'H':
                    hostName = optarg;
                    break;
                case 'c':
                    containerName = optarg;
                    break;
                case 'a':
                    additional = optarg;
                    break;
            }
        }
    if (instance == -1)
        {
        ACE_OS::printf("Error: instance is a mandatory option try %s -h\n", argv[0]);
        return -1;
        } 

    if (containerName.length() == 0)
        {
        ACE_OS::printf("Error: container name is a mandatory option try %s -h\n", argv[0]);
        return -1;
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
	  daemonRef = daemonRef + hostName + ":" + ACSPorts::getContainerDaemonPort().c_str() + "/" + ::acsdaemon::containerDaemonServiceName;	
	  ACS_SHORT_LOG((LM_INFO, "Using local Container Daemon reference: '%s'", daemonRef.c_str()));
	  
	  }
      else
          {
          ACS_SHORT_LOG((LM_INFO, "Container Daemon reference obtained via command line: '%s'", daemonRef.c_str()));
          }


		CORBA::Object_var obj = orb->string_to_object(daemonRef.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_INFO, "Failed to resolve reference '%s'.", daemonRef.c_str()));
			return -1;
		}

		acsdaemon::ContainerDaemon_var daemon = acsdaemon::ContainerDaemon::_narrow(obj.in());
		if (CORBA::is_nil(daemon.in()))
		{
			ACS_SHORT_LOG((LM_INFO, "Failed to narrow reference '%s'.", daemonRef.c_str()));
			return -1;
		}


                ACS_SHORT_LOG((LM_INFO, "Calling stop_container(%s, %d, %s).", containerName.c_str(), instance, additional.c_str()));

                daemon->stop_container(containerName.c_str(), instance, additional.c_str());

                ACS_SHORT_LOG((LM_INFO, "Container stop message issued."));


	}
	catch (ACSErrTypeCommon::BadParameterEx &ex)
	{
		ACSErrTypeCommon::BadParameterExImpl exImpl(ex);
		exImpl.log();
		return -1;
	}
	catch (acsdaemonErrType::FailedToStopContainerEx &ex)
	{
		acsdaemonErrType::FailedToStopContainerExImpl exImpl(ex);
		exImpl.log();
		return -1;
	}
	catch( CORBA::Exception &ex )
	{
		ACS_SHORT_LOG((LM_INFO, "Failed."));
		ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
		return -1;
	}

	return 0;
}





