/*******************************************************************************
 * E.S.O. - ACS project
 *
 * "@(#) $Id: acsdaemonImpStop.cpp,v 1.1 2008/12/01 13:39:56 msekoran Exp $"
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
        {"reference",      required_argument, 0, 'r'},
        {0, 0, 0, '\0'}};

void 
usage(const char *argv)
{
    ACE_OS::printf ("\n\tusage: %s {-h}", argv);
    ACE_OS::printf ("\t   -h, --help         show this help message\n");
    ACE_OS::printf ("\t   -r, --reference    Imp reference\n");
}


int
main (int argc, char *argv[])
{
    int c;
    ACE_CString impRef;
    for(;;)
        {
        int option_index = 0;
        c = getopt_long (argc, argv, "hr:",
                         long_options, &option_index); 
        if (c==-1) break;
        switch(c)
            {
                case 'h':
                    usage(argv[0]);
                    return 0;
                case 'r':
                    impRef = optarg;
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

	if (impRef.length() == 0)
	{
		ACS_SHORT_LOG((LM_ERROR, "No Imp reference given."));
		usage(argv[0]);
		return -1;
	}

	try
	{
		// Initialize the ORB.
		CORBA::ORB_var orb = CORBA::ORB_init (argc,argv,"TAO");

		CORBA::Object_var obj = orb->string_to_object(impRef.c_str());
		if (CORBA::is_nil(obj.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to resolve reference '%s'.", impRef.c_str()));
			return -1;
		}

		acsdaemon::ImpBase_var imp = acsdaemon::ImpBase::_narrow(obj.in());
		if (CORBA::is_nil(imp.in()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to narrow reference '%s'.", impRef.c_str()));
			return -1;
		}


                ACS_SHORT_LOG((LM_INFO, "Calling shutdown()."));

                imp->shutdown();

                ACS_SHORT_LOG((LM_INFO, "Imp shutdown message issued."));


	}
	catch( maciErrType::NoPermissionEx &ex )
	{
		ACS_SHORT_LOG((LM_WARNING, "Imp is running in protected mode and cannot be shut down remotely!\n"));
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





