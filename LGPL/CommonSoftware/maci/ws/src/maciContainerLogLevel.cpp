/*******************************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: maciContainerLogLevel.cpp,v 1.8 2010/03/30 21:34:55 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2002-04-15 corrected shutdown action
* msekoran  2001/12/23  created
*/


/** @file maciContainerSetLogLevel.cpp
 *  maciContainerSetLogLevel is used to set container log levels runtime.
 *  @htmlonly
 *  <br><br>
 *  @endhtmlonly

 *  @param container This is simply the name of the container in the CDB that we want to stop.
 *  @htmlonly
 *  <br><hr>
 *  @endhtmlonly

 */



#include <vltPort.h>
#include <acsutil.h>

#include <logging.h>
#include <maciHelper.h>
#include <maciContainerImpl.h>

 using namespace maci;

void
printUsageAndExit(int argc, char *argv[])
{
	ACE_OS::printf("\n\tusage: %s <container name wildcard> {set,get,list,refresh} [<loggerName | \"default\"> <minLogLevel> <minLogLevelLocal>] [<ORB options>]\nWith log level values {1-6,8-11,99=OFF}\n", argv[0]);
	exit(-1);
}

// Only integer values for certain levels are defined, e.g. DEBUG = 3.
// No illegal levels should be sent to the LoggingConfigurable processes (containers, manager, ...)
int
isLogLevelValid(int logLevel)
{
	return ( (logLevel >= 1 && logLevel <= 11 && logLevel != 7) || logLevel==99 );
}

int
main (int argc, char *argv[])
{

    if (argc < 3)
    	printUsageAndExit(argc, argv);

	char * command = argv[2];

	#define SET 1
	#define GET 2
	#define LIST 3
	#define REFRESH 4
	int cmd;
	if (ACE_OS::strcmp(command, "set") == 0)
		cmd = SET;
	else if (ACE_OS::strcmp(command, "get") == 0)
		cmd = GET;
	else if (ACE_OS::strcmp(command, "list") == 0)
		cmd = LIST;
	else if (ACE_OS::strcmp(command, "refresh") == 0)
		cmd = REFRESH;
	else
	{
		ACE_OS::printf("Invalid command, must be one of {set,get,list,refresh}.\n");
	    printUsageAndExit(argc, argv);
	}

	if (cmd == SET && argc < 4)
	{
		// if only 4 then default is set
		ACE_OS::printf("Not enough parameters.\n");
	    printUsageAndExit(argc, argv);
	}

	if (cmd == GET && argc < 4)
	{
		ACE_OS::printf("Not enough parameters.\n");
		printUsageAndExit(argc, argv);
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


	maci::Manager_var mgr = MACIHelper::resolveManager(orb.ptr(), argc, argv, 0, 0);
    if (CORBA::is_nil(mgr.ptr()))
	{
	    ACS_SHORT_LOG((LM_ERROR, "Failed to resolve Manager reference."));
	    return -1;
	}

	maci::HandleSeq handles;
	handles.length(0);

	// a little hack to manager (this clas should implement administrator interface, etc..)
	maci::Handle h = 0x05000000;
	maci::ContainerInfoSeq_var containers = mgr->get_container_info(h, handles, argv[1]);


	if (!containers.ptr() || containers->length()==0)
	{
	    ACS_SHORT_LOG((LM_INFO, "No containers matching name %s found.", argv[1]));
	    return -1;
	}

	for (CORBA::ULong i = 0; i < containers->length(); i++)
	{
	    try
	    {
	    	ACS_SHORT_LOG((LM_INFO, "Container: %s", containers[i].name.in()));

	    	if (cmd == LIST)
	    	{
	    		ACS_SHORT_LOG((LM_INFO, "\tLogger names:"));
	    		maci::stringSeq_var loggerNames = containers[i].reference->get_logger_names();
	    		for (CORBA::ULong j = 0; j < loggerNames->length(); j++)
	   				ACS_SHORT_LOG((LM_INFO, "\t\t%s", loggerNames[j].in()));
	    	}
	    	else if (cmd == REFRESH)
	    	{
	    		ACS_SHORT_LOG((LM_INFO, "\tRefreshing logging config."));
				containers[i].reference->refresh_logging_config();
	    	}
	    	else if (cmd == GET)
	    	{
	    		char * loggerName = argv[3];

			    LoggingConfigurable::LogLevels logLevels;
			    if (ACE_OS::strcmp(loggerName, "default") == 0)
			     	logLevels = containers[i].reference->get_default_logLevels();
			    else
			    	logLevels = containers[i].reference->get_logLevels(loggerName);

			    ACS_SHORT_LOG((LM_INFO, "\tLog levels for logger '%s':", loggerName));
			    ACS_SHORT_LOG((LM_INFO, "\t\tuseDefault      : %s", logLevels.useDefault ? "true" : "false"));
			    ACS_SHORT_LOG((LM_INFO, "\t\tminLogLevel     : %d", logLevels.minLogLevel));
			    ACS_SHORT_LOG((LM_INFO, "\t\tminLogLevelLocal: %d", logLevels.minLogLevelLocal));
	    	}
	    	else if (cmd == SET)
	    	{
	    		char * loggerName = argv[3];

			    LoggingConfigurable::LogLevels logLevels;

			    logLevels.useDefault = argc < 6;
			    if (!logLevels.useDefault)
			    {
	    			logLevels.minLogLevel = atoi(argv[4]);
	    			logLevels.minLogLevelLocal = atoi(argv[5]);
			    	if (!isLogLevelValid(logLevels.minLogLevel) || !isLogLevelValid(logLevels.minLogLevelLocal))
			    	{
			    		printUsageAndExit(argc, argv);
			    	}
			    }
			    else
			    {
	    			logLevels.minLogLevel = 0;
	    			logLevels.minLogLevelLocal = 0;
			    }

			    ACS_SHORT_LOG((LM_INFO, "\tSetting levels for logger '%s':", loggerName));
			    ACS_SHORT_LOG((LM_INFO, "\t\tuseDefault      : %s", logLevels.useDefault ? "true" : "false"));
			    //if (!logLevels.useDefault)
			    {
			    	ACS_SHORT_LOG((LM_INFO, "\t\tminLogLevel     : %d", logLevels.minLogLevel));
			    	ACS_SHORT_LOG((LM_INFO, "\t\tminLogLevelLocal: %d", logLevels.minLogLevelLocal));
			    }

			    if (ACE_OS::strcmp(loggerName, "default") == 0)
	    			containers[i].reference->set_default_logLevels(logLevels);
				else
	    			containers[i].reference->set_logLevels(loggerName, logLevels);
	    	}

			ACS_SHORT_LOG((LM_INFO, "\t... done."));
	    }
	    catch( CORBA::Exception &ex )
	    {
	        ACS_SHORT_LOG((LM_INFO, "... failed!"));
			ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION, ACE_TEXT ("Exception caught while calling remote method."));
	    }

	}

	ACS_SHORT_LOG((LM_INFO, "Done all."));

    }
  catch( CORBA::Exception &ex )
  {
      ACS_SHORT_LOG((LM_INFO, "Failed."));
      ACE_PRINT_EXCEPTION (ex,
                           ACE_TEXT ("Caught unexpected exception:"));

      return -1;
  }

  return 0;
}


