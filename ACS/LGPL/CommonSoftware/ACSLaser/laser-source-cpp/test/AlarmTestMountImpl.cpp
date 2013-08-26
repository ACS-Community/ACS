/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
* 
*/

#include <AlarmTestMountImpl.h>
#include "ACSAlarmSystemInterfaceFactory.h"
#include "FaultState.h"
#include "faultStateConstants.h"

using namespace acscomponent;
using namespace testalarmsystem;
using std::string;
using std::auto_ptr;
using acsalarm::Properties;
using acsalarm::Timestamp;

AlarmTestMountImpl::AlarmTestMountImpl(const ACE_CString &name,maci::ContainerServices * containerServices) : 
    ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::AlarmTestMount::AlarmTestMount");

	// create the AlarmSystemInterface
	alarmSource = ACSAlarmSystemInterfaceFactory::createSource();

	// initialize the counter
	counter = 0;
}

AlarmTestMountImpl::~AlarmTestMountImpl()
{
	ACS_TRACE("::AlarmTestMount::~AlarmTestMount");
}

void AlarmTestMountImpl::faultMount() 
{
	// alternate using the "short-hand" and the "long-hand" techniques for sending the alarm 
	// so that we will have test coverage of both styles of sending an alarm
	if((counter++ % 2) == 0) 
	{
		sendAlarmLongHand("Mount", "ALARM_SOURCE_MOUNT", 1, true);
	}
	else 
	{
		sendAlarmShortHand("Mount", "ALARM_SOURCE_MOUNT", 1, true);
	}
}

void AlarmTestMountImpl::terminate_faultMount()
{
	sendAlarmLongHand("Mount", "ALARM_SOURCE_MOUNT", 1, false);
}

void AlarmTestMountImpl::sendAlarmLongHand(std::string family, std::string member, int code, bool active) 
{
	ACS_TRACE("::AlarmTestMount::sendAlarmLongHand entering");

	// create the FaultState
	auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(family, member, code);

	// set the fault state's descriptor
	string stateString;
	if (active) 
	{
		stateString = faultState::ACTIVE_STRING;
	} 
	else 
	{
		stateString = faultState::TERMINATE_STRING;
	}
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
	alarmSource->push(*fltstate);

	ACS_TRACE("::AlarmTestMount::sendAlarmLongHand exiting");
}

void AlarmTestMountImpl::sendAlarmShortHand(std::string family, std::string member, int code, bool active) 
{
	ACS_TRACE("::AlarmTestMount::sendAlarmShortHand entering");

	// create a Properties object and configure it, then assign to the FaultState
	Properties props;
	props.setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, "prefix");
	props.setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, "suffix");
	props.setProperty("TEST_PROPERTY", "TEST_VALUE");

	ACSAlarmSystemInterfaceFactory::createAndSendAlarm(family, member, code, active, props);

	ACS_TRACE("::AlarmTestMount::sendAlarmShortHand exiting");
}

/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(AlarmTestMountImpl)
/* ----------------------------------------------------------------*/
