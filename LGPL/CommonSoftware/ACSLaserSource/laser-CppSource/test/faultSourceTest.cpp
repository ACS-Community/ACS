#include <memory>
#include "AlarmSystemInterface.h"
#include "AlarmSystemInterfaceFactory.h"
#include "Timestamp.h"
#include "FaultState.h"
#include "Properties.h"
#include "faultStateConstants.h"
#include "asiConfigurationConstants.h"

using namespace laserSource;
using namespace laserUtil;

/*
 * Crude test case to send a fault to the laser server using the cpp laser source library.
 */
int main(int argc, char *argv[])
{
	//try 
	{
		// constants we will use when creating the fault
		string family = "AlarmSource";
		string member = "ALARM_SOURCE_MOUNT";
		int code = 1;

		// create the AlarmSystemInterface
		auto_ptr<AlarmSystemInterface> alarmSource = AlarmSystemInterfaceFactory::createSource();

		// create the FaultState
		auto_ptr<FaultState> fltstate = AlarmSystemInterfaceFactory::createFaultState(family, member, code);

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

		// push the FaultState using the AlarmSystemInterface previously created
		FaultState stateToPush(*fltstate);
		alarmSource->push(stateToPush);
	} 
	/* 
	// TODO later:
	catch (ASIException e) 
	{
		e.printStackTrace();
	}
	*/
}
