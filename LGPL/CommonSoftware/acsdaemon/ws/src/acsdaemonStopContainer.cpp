/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: acsdaemonStopContainer.cpp,v 1.1 2007/10/09 23:39:10 nbarriga Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2006/06/21  created
 */

/** @file acsdaemonStartContainer.cpp
 *  acsdaemonStartContainer is used to remotely start container via ACS Deamon.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly
 */   


#include <acsutilPorts.h>
#include <logging.h>
#include <acsdaemonC.h>
#include <ACSErrTypeCommon.h>
#include <acsdaemonErrType.h>




int
main (int argc, char *argv[])
{
	if (argc < 3 || argc > 4)
	{
		ACE_OS::printf("\n\tusage: %s container_name instance_number [additional_command_line]\n\n", argv[0]);
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

		ACE_CString ref;

		// Command line option -d or -daemonReference
		for (int pos = 1; pos < argc-1; pos++)
			if (ACE_OS::strcmp(argv[pos], "-d")==0 || ACE_OS::strcmp(argv[pos], "-daemonReference")==0)
			{
				// increase pos to point to the reference
				pos++;

				ref = argv[pos];
				ACS_SHORT_LOG((LM_INFO, "ACS Daemon reference obtained via command line: '%s'", ref.c_str()));
				break;
			}

		// construct default one
		if (ref.length() == 0)
		{
			const char * hostName = ACSPorts::getIP(); 
			ref = "corbaloc::";
			ref = ref + hostName + ":" + ACSPorts::getDaemonPort().c_str() + "/ACSDaemon";	
			ACS_SHORT_LOG((LM_INFO, "Using local ACS Daemon reference: '%s'", ref.c_str()));

		}

		CORBA::Object_var obj = orb->string_to_object(ref.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_INFO, "Failed to resolve reference '%s'.", ref.c_str()));
			return -1;
		}

		acsdaemon::Daemon_var daemon = acsdaemon::Daemon::_narrow(obj.in());
		if (CORBA::is_nil(daemon.in()))
		{
			ACS_SHORT_LOG((LM_INFO, "Failed to narrow reference '%s'.", ref.c_str()));
			return -1;
		}


		int instance = atoi(argv[2]);
                ACS_SHORT_LOG((LM_INFO, "Calling stop_container(%s, %d, %s).", argv[1], instance, argv[3]));

                daemon->stop_container(argv[1], instance, argv[3]);

                ACS_SHORT_LOG((LM_INFO, "Container stopped."));


	}
	catch (ACSErrTypeCommon::BadParameterEx &ex)
	{
		ACSErrTypeCommon::BadParameterExImpl exImpl(ex);
		exImpl.log();
	}
	catch (acsdaemonErrType::FailedToStopContainerEx &ex)
	{
		acsdaemonErrType::FailedToStopContainerExImpl exImpl(ex);
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





