#include <memory>

#include <logging.h>
#include <loggingGenericLogger.h>
#include "ACSAlarmSystemInterface.h"
#include "ACSAlarmSystemInterfaceFactory.h"
#include "Timestamp.h"
#include "ACSFaultState.h"
#include "Properties.h"
#include "faultStateConstants.h"
//#include "asiConfigurationConstants.h"

using namespace acsalarm;

void printUsageAndExit()
{
	std::cout << "\n\nUsage: \n\n" << "TestAcsAlarmSending <NUM_ALARMS_TO_SEND>\n\n" << "where NUM_ALARMS_TO_SEND is how many alarms you wish to send.\n\n";
	exit(-1);	
}

/*
 * Crude test case to send a fault to the laser server using the cpp laser source library.
 */
int main(int argc, char *argv[])
{
	Logging::Logger::setGlobalLogger(new Logging::GenericLogger("testLogger"));

	if(argc < 2)
	{
		printUsageAndExit();
	}

	// TEST 1: the "long hand" way to send alarms
	int numAlarmsToSend = atoi(argv[1]);

	// constants we will use when creating the fault
	string family = "AlarmSource";
	string member = "ALARM_SOURCE_MOUNT";
	int code = 1;

	std::cout << "Testing long-hand style of sending alarms" << std::endl;

	//try 
	{
		// initialize the AlarmSystemInterfaceFactory 
		ACSAlarmSystemInterfaceFactory::init(NULL);

		// create the AlarmSystemInterface
		auto_ptr<ACSAlarmSystemInterface> alarmSource = ACSAlarmSystemInterfaceFactory::createSource();

		// create the FaultState
		auto_ptr<ACSFaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(family, member, code);

		// set the fault state's descriptor
		string stateString = faultState::ACTIVE_STRING;
		fltstate->setDescriptor(stateString);
		
		// create a Timestamp and use it to configure the FaultState
		Timestamp * tstampPtr = new Timestamp();
		auto_ptr<Timestamp> tstampAutoPtr(tstampPtr);
		fltstate->setUserTimestamp(tstampAutoPtr);

		// create a Properties object and configure it, then assign to the FaultState
		Properties * propsPtr = new Properties();
		propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, "prefix");
		propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, "suffix");
		propsPtr->setProperty("TEST_PROPERTY", "TEST_VALUE");
		auto_ptr<Properties> propsAutoPtr(propsPtr);
		fltstate->setUserProperties(propsAutoPtr);

		for(int i = 0; i < numAlarmsToSend; i++)
		{
			// push the FaultState using the AlarmSystemInterface previously created
			alarmSource->push(*fltstate);
		}

		ACSAlarmSystemInterfaceFactory::done();
	}
	/* 
	// TODO later:
	catch (ASIException e) 
	{
		e.printStackTrace();
	}
	*/

	std::cout << "Testing short-hand style of sending alarms" << std::endl;

	// TEST 2: the "short hand" way to send alarms
	//try 
	{
		// initialize the AlarmSystemInterfaceFactory 
		ACSAlarmSystemInterfaceFactory::init(NULL);

		// create a Properties object and configure it 
		Properties props;
		props.setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, "prefix");
		props.setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, "suffix");
		props.setProperty("TEST_PROPERTY", "TEST_VALUE");

		for(int i = 0; i < numAlarmsToSend; i++)
		{
			// push the FaultState using the AlarmSystemInterface previously created
			ACSAlarmSystemInterfaceFactory::createAndSendAlarm(family, member, code, faultState::ACTIVE_STRING, props);
		}

		ACSAlarmSystemInterfaceFactory::done();
	}
	/* 
	// TODO later:
	catch (ASIException e) 
	{
		e.printStackTrace();
	}
	*/
}
