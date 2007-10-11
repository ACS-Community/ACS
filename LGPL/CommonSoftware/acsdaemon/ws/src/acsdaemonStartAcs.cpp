/*******************************************************************************
* E.S.O. - ACS project
*
*
* who       when        what
* --------  ----------  ----------------------------------------------
*/



#include <acsutilPorts.h>
#include <logging.h>
#include <acsdaemonC.h>
#include <ACSErrTypeCommon.h>
#include <acsdaemonErrType.h>
#include <getopt.h>

static struct option long_options[] = {
        {"help",        no_argument,       0, 'h'},
        {"instance",    required_argument, 0, 'i'},
        {"host",        required_argument, 0, 'H'},
        {"deamon",      required_argument, 0, 'd'},
        {"aditional",   required_argument, 0, 'a'},
        {0, 0, 0, '\0'}};

void 
usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h} -i INSTANCE [-d DAEMONREF] [-H HOST] [-a more options]", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -i, --instance     ACS instance to start\n");
    ACE_OS::printf ("\t   -H, --host         Host where to start ACS\n");
    ACE_OS::printf ("\t   -d, --daemon       Daemon reference\n");
    ACE_OS::printf ("\t   -a, --aditional    passthrough options for startACS\n");
}

int
main (int argc, char *argv[])
    {
    int c, instance = -1;
    ACE_CString daemonRef;
    ACE_CString hostName;
    char *aditional=NULL;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "hi:d:H:a",
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
                case 'a':
                    aditional = argv[option_index];
                    break;
            }
        }
    if (instance == -1)
        {
        ACE_OS::printf("Error: instance is a mandatory option try %s -h\n", argv[0]);
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
      CORBA::ORB_var orb = CORBA::ORB_init (argc,
                                            argv,
                                            "TAO"
                                            );

      
      // construct default one
      if (daemonRef.length() == 0)
	  {
          if(hostName.length() == 0)
          {
	      hostName = ACSPorts::getIP();
          } 
	  daemonRef = "corbaloc::";
	  daemonRef = daemonRef + hostName + ":" + ACSPorts::getServicesDaemonPort().c_str() + "/ACSServicesDaemon";	
	  ACS_SHORT_LOG((LM_INFO, "Using local ACS Services Daemon reference: '%s'", daemonRef.c_str()));
	  
	  }
      else
          {
          ACS_SHORT_LOG((LM_INFO, "ACS Services Daemon reference obtained via command line: '%s'", daemonRef.c_str()));
          }

      CORBA::Object_var obj = orb->string_to_object(daemonRef.c_str());
      if (CORBA::is_nil(obj.in()))
	  {
	  ACS_SHORT_LOG((LM_INFO, "Failed to resolve reference '%s'.", daemonRef.c_str()));
	  return -1;
	  }

      acsdaemon::ServicesDaemon_var daemon = acsdaemon::ServicesDaemon::_narrow(obj.in());
      if (CORBA::is_nil(daemon.in()))
	  {
	  ACS_SHORT_LOG((LM_INFO, "Failed to narrow reference '%s'.", daemonRef.c_str()));
	  return -1;
	  }

      ACS_SHORT_LOG((LM_INFO, "Calling start_acs(%d, %s).", instance, aditional));
      daemon->start_acs(instance, aditional);
      ACS_SHORT_LOG((LM_INFO, "Done."));
      
    }
    catch (ACSErrTypeCommon::BadParameterEx &ex)
    {
	ACSErrTypeCommon::BadParameterExImpl exImpl(ex);
	exImpl.log();
    }
    catch (acsdaemonErrType::FailedToStartAcsEx &ex)
    {
	acsdaemonErrType::FailedToStartAcsExImpl exImpl(ex);
	exImpl.log();
    }
    catch( CORBA::Exception &ex )
    {

	ACS_SHORT_LOG((LM_INFO, "Failed."));
	ACE_PRINT_EXCEPTION (ex, ACE_TEXT ("Caught unexpected exception:"));
	return -1;
    }
    
    return 0;
}
