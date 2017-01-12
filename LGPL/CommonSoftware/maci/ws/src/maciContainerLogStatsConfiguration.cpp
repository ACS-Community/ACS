/*******************************************************************************
* E.S.O. - ACS project
*
* who       when      what
* --------  --------  ----------------------------------------------
* mmanas  2015/01/21  created
*/

#include <vltPort.h>
#include <acsutil.h>

#include <logging.h>
#include <maciHelper.h>
#include <maciContainerImpl.h>

#include <getopt.h>

 using namespace maci;

#define CONTAINER_ARGV 4
#define CMD_ARGV 5

// Function in charge if providing and usage information and exiting the application. To be used when wrong parameters are entered
void
printUsageAndExit(int argc, char *argv[])
{
	ACE_OS::printf("\n usage: %s <container name> {set,get,list} "
			"<loggerName> [-c {Enable / Disable / Default}] [-n <Element name>] [-p <Calculation period>] [-g <Granularity>] \n"
			"\t [OPTIONAL][used only with SET command] -c allows configure the statistics status for the logger.\n"
			"\t\tDefault sets all logger statistics to default unless explicitly stated \n"
			"\t [OPTIONAL][used only with SET command] -n allows configuration of the statistics logger module name\n"
			"\t [OPTIONAL][used only with SET command] -p allows configuration of statistics calculation period (in seconds). Should be > 0 \n"
			"\t [OPTIONAL][used only with SET command] -g allows configuration of statistics granularity (in seconds). Should be > 0 \n" , argv[0]);
	exit(-1);
}

// Function in charge of indicating input parameter error for "Configuration period"
void
printWrongPeriodParameterUsage(int argc, char *argv[])
{
	ACE_OS::printf("\nWrong Usage of \"Calculation period\" parameter: Should be an integer > 0 \n" );
	printUsageAndExit(argc, argv);
}

// Function in charge of indicating input parameter error for "Configuration period"
void
printWrongGranularityParameterUsage(int argc, char *argv[])
{
	ACE_OS::printf("\nWrong Usage of \"Granularity\" parameter: Should be an integer > 0 \n" );
	printUsageAndExit(argc, argv);
}

// Optional arguments fpr SET command parsed
struct arguments {
	char * command;
	char * elementName;
	unsigned int period;
	unsigned int granularity;
} ;

// Function detecting SET optional parameters and returning them (elementName, period, granularity)
arguments
getOptionalParameters(int argc, char *argv[])
{
	// Initailisation of parameters. The init values are considered to indicate that no modification is requested
	arguments taken;
	taken.command = "";
	taken.elementName = "";
	taken.period = 0;
	taken.granularity = 0;

	// Retrieve valid arguments, all other arguments will be ignored
	int c;
	while ((c = getopt(argc, argv, "hc:n:p:g:")) != -1) {
		switch(c) {
			case 'h':
				printUsageAndExit(argc, argv);
				break;
			case 'c':
				taken.command = optarg;
				break;
			case 'n':
				taken.elementName = optarg;
				break;
			case 'p':
				taken.period = atoi(optarg);
				if (taken.period <= 0 )
				{
					// Invalid usage of parameter Period
					printWrongPeriodParameterUsage(argc, argv);
				}
				break;
            case 'g':
            	taken.granularity = atoi(optarg);
				if (taken.granularity <= 0 )
				{
					// Invalid usage of parameter Granularity
					printWrongGranularityParameterUsage(argc, argv);
				}
            	break;
			default:
				break;
		}
	}
	// Return information retrieved from arguments
	return taken;
}

// This application is in charge of providing and setting logger statistics configuration
int
main (int argc, char *argv[])
{

    if (argc < 3)
    	printUsageAndExit(argc, argv);

	char * command = argv[2];

	// Arguments control
	#define SET 1
	#define GET 2
	#define LIST 3
	int cmd;
	if (ACE_OS::strcmp(command, "set") == 0)
		cmd = SET;
	else if (ACE_OS::strcmp(command, "get") == 0)
		cmd = GET;
	else if (ACE_OS::strcmp(command, "list") == 0)
		cmd = LIST;
	else
	{
		ACE_OS::printf("Invalid command, must be one of {set,get,list}.\n");
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

	// Configuration of proxy
    LoggingProxy * logger = new LoggingProxy(0, 0, 31);
    if (logger)
    {
    	LoggingProxy::init(logger);
    	LoggingProxy::ProcessName(argv[0]);
    	LoggingProxy::ThreadName("main");
    }
    else
    	ACS_SHORT_LOG((LM_INFO, "Failed to initialize logging."));

    // Main execution
    try
    {
		// Initialize the ORB.
		CORBA::ORB_var orb = CORBA::ORB_init (argc,argv,"TAO");

		maci::Manager_var mgr = MACIHelper::resolveManager(orb.ptr(), argc, argv, 0, 0);
		if (CORBA::is_nil(mgr.ptr()))
		{
			ACS_SHORT_LOG((LM_ERROR, "Failed to resolve Manager reference."));
			return -1;
		}

		maci::HandleSeq handles;
		handles.length(0);

		// a little hack to manager (this class should implement administrator interface, etc..)
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
					ACS_SHORT_LOG((LM_INFO, "\tLogger information:"));
					ACS_SHORT_LOG((LM_INFO, "\t\t[StatisticsIdentification].[LoggerName] | [Statistics status] | [Calculation period] | [Granularity]"));
					Logging::ACSLogStatistics::logStatsInformationSeq_var loggerInformation = containers[i].reference->get_statistics_logger_configuration();

					for (CORBA::ULong j = 0; j < loggerInformation->length(); j++)
					{
						ACS_SHORT_LOG((LM_INFO, "\t\t%s.%s | %s | %d | %d", loggerInformation[j].statsId.in(),
																			loggerInformation[j].loggerName.in(),
																			loggerInformation[j].statsStatus ? "statsDisabled" : "statsEnabled",
																			loggerInformation[j].statsPeriodConfiguration,
																			loggerInformation[j].statsGranularityConfiguration));
					}
				}
				else if (cmd == GET)
				{
					char * loggerName = argv[3];

					Logging::ACSLogStatistics::LogStatsInformation_var loggerInformation =
							containers[i].reference->get_statistics_logger_configuration_byname(loggerName);

					ACS_SHORT_LOG((LM_INFO, "\tLogger information '%s':", loggerName));
					ACS_SHORT_LOG((LM_INFO, "\t\tStatisticsIdentification      : %s", loggerInformation->statsId.in() ));
					ACS_SHORT_LOG((LM_INFO, "\t\tLoggerName                    : %s", loggerInformation->loggerName.in() ));
					ACS_SHORT_LOG((LM_INFO, "\t\tStatistics status             : %s", loggerInformation->statsStatus ? "statsDisabled" : "statsEnabled"));
					ACS_SHORT_LOG((LM_INFO, "\t\tCalculation Period            : %d", loggerInformation->statsPeriodConfiguration));
					ACS_SHORT_LOG((LM_INFO, "\t\tStatistics Granularity        : %d", loggerInformation->statsGranularityConfiguration));

				}
				else if (cmd == SET)
				{
					// Retrieve required arguments
					char * loggerName = argv[3];

					// Retrieve logger statistics configuration
					Logging::ACSLogStatistics::LogStatsInformation_var loggerInformation=
							containers[i].reference->get_statistics_logger_configuration_byname(loggerName);

					// Retrieve optional arguments allowed for SET command
					arguments taken = getOptionalParameters(argc, argv);
					if (taken.command != "" &&
						( ACE_OS::strcmp(taken.command, "Enable") == 0 ||
						  ACE_OS::strcmp(taken.command, "enable") == 0 ||
						  ACE_OS::strcmp(taken.command, "ENABLE") == 0 ) )
					{
						loggerInformation->statsStatus = false;
					}
					else if (taken.command != "" &&
							( ACE_OS::strcmp(taken.command, "Disable") == 0 ||
							  ACE_OS::strcmp(taken.command, "disable") == 0 ||
							  ACE_OS::strcmp(taken.command, "DISABLE") == 0 ) )
					{
						loggerInformation->statsStatus = true;
					}
					else if (taken.command != "" &&
							( ACE_OS::strcmp(taken.command, "Default") == 0 ||
							  ACE_OS::strcmp(taken.command, "default") == 0 ||
							  ACE_OS::strcmp(taken.command, "DEFAULT") == 0 ) )
					{
						loggerInformation->statsStatus = Logging::loggingStatistics::DEFAULT_STATISTICS_STATE;
						loggerInformation->statsPeriodConfiguration = Logging::loggingStatistics::DEFAULT_STATISTICS_PERIOD;
						loggerInformation->statsGranularityConfiguration = Logging::loggingStatistics::DEFAULT_STATISTICS_GRANULARITY;
					}
					else if (taken.command != "")
					{
						// Invalid arguments
						ACS_SHORT_LOG((LM_INFO, "No valid configuration introduced for \"Statistics status\" "));
						printUsageAndExit(argc, argv);
					}

					if (taken.elementName != "")
					{
						loggerInformation->statsId = CORBA::string_dup(taken.elementName);
					}
					if (taken.period != 0)
					{
						loggerInformation->statsPeriodConfiguration = taken.period;
					}
					if (taken.granularity != 0)
					{
						loggerInformation->statsGranularityConfiguration = taken.granularity;
					}

					// Set logger statistics configuration with values from arguments
					containers[i].reference->set_statistics_logger_configuration_byname(loggerName, loggerInformation);

					// Retrieve and print the configuration just set
					loggerInformation=containers[i].reference->get_statistics_logger_configuration_byname(loggerName);

					ACS_SHORT_LOG((LM_INFO, "\tLogger statistics configuration '%s':", loggerName));
					ACS_SHORT_LOG((LM_INFO, "\t\tStatisticsIdentification      : %s", loggerInformation->statsId.in() ));
					ACS_SHORT_LOG((LM_INFO, "\t\tLoggerName                    : %s", loggerInformation->loggerName.in() ));
					ACS_SHORT_LOG((LM_INFO, "\t\tStatistics status             : %s", loggerInformation->statsStatus ? "statsDisabled" : "statsEnabled"));
					ACS_SHORT_LOG((LM_INFO, "\t\tCalculation Period            : %d", loggerInformation->statsPeriodConfiguration));
					ACS_SHORT_LOG((LM_INFO, "\t\tStatistics Granularity        : %d", loggerInformation->statsGranularityConfiguration));
				}

				// End of SET command
				ACS_SHORT_LOG((LM_INFO, "\t... done."));
			}
		    catch( Logging::LoggerDoesNotExistEx &ex )
			{
				ACS_SHORT_LOG((LM_INFO, "Logger %s does not exist", ex.LoggerName.in()));
				ex._tao_print_exception("Caught LoggerDoesNotExistEx exception:");
			}
			catch( CORBA::Exception &ex )
			{
				ACS_SHORT_LOG((LM_INFO, "... failed!"));
				ex._tao_print_exception("Exception caught while calling remote method.");
			}

		}
		// End for all containers
		ACS_SHORT_LOG((LM_INFO, "Done all."));
    }
	catch( CORBA::Exception &ex )
	{
	  ACS_SHORT_LOG((LM_INFO, "Failed."));
	  ex._tao_print_exception("Caught unexpected exception:");

	  // KO
	  return -1;
	}

	// OK
	return 0;
}


