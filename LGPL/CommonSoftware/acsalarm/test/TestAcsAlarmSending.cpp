/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include <memory>

#include <logging.h>
#include <loggingGenericLogger.h>
#include "AlarmSystemInterface.h"
#include "ACSAlarmSystemInterfaceFactory.h"
#include "Timestamp.h"
#include "FaultState.h"
#include "Properties.h"
#include "faultStateConstants.h"
//#include "asiConfigurationConstants.h"

using namespace acsalarm;
using std::string;
using std::auto_ptr;

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
		AlarmSystemInterface* alarmSource = ACSAlarmSystemInterfaceFactory::createSource();

		// create the FaultState
		auto_ptr<FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(family, member, code);

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

	std::cout << "Testing experimental short-hand style of sending alarms with properties" << std::endl;

	// TEST 2: the "short hand" way to send alarms, with properties
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

	std::cout << "Testing experimental short-hand style of sending alarms without properties" << std::endl;

	// TEST 3: the "short hand" way to send alarms, without properties
	//try 
	{
		// initialize the AlarmSystemInterfaceFactory 
		ACSAlarmSystemInterfaceFactory::init(NULL);

		for(int i = 0; i < numAlarmsToSend; i++)
		{
			// push the FaultState using the AlarmSystemInterface previously created
			ACSAlarmSystemInterfaceFactory::createAndSendAlarm(family, member, code, faultState::ACTIVE_STRING);
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

