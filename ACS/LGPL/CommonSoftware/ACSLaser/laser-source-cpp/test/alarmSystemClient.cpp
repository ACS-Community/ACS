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
*
* "@(#) $Id: alarmSystemClient.cpp,v 1.1 2009/10/11 08:16:37 acaproni Exp $"
*
*/

#include <stdlib.h>
#include <string>
#include <maciSimpleClient.h>
#include "AlarmSystemInterface.h"
#include "ACSAlarmSystemInterfaceFactory.h"
#include "FaultState.h"
#include "faultStateConstants.h"

ACE_RCSID(acsexmpl, alarmSystemClient, "$Id: alarmSystemClient.cpp,v 1.1 2009/10/11 08:16:37 acaproni Exp $")
using namespace maci;

/**
 * Send an alarm
 *
 * @param alarmSource The source the send the alarm
 * @param ff FaultFamily
 * @param fm FaultMember
 * @param fc FaultCode
 * @param active If true send the alarm as ACTIVE, otherwise as TERMINATE
 */
void sendAlarm(
		acsalarm::AlarmSystemInterface* source,
		const std::string ff,
		const std::string fm,
		const int fc,
		bool active) {
	// create the FaultState
	auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(ff, fm, fc);

	// set the fault state's descriptor
	std::string stateString;
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
	acsalarm::Timestamp* tstampPtr = new acsalarm::Timestamp();
	auto_ptr<acsalarm::Timestamp> tstampAutoPtr(tstampPtr);
	fltstate->setUserTimestamp(tstampAutoPtr);

	// create a Properties object and configure it, then assign to the FaultState
	acsalarm::Properties * propsPtr = new acsalarm::Properties();
	propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, "prefix");
	propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, "suffix");
	propsPtr->setProperty("TEST_PROPERTY", "TEST_VALUE");
	auto_ptr<acsalarm::Properties> propsAutoPtr(propsPtr);
	fltstate->setUserProperties(propsAutoPtr);

	// push the FaultState using the AlarmSystemInterface previously created
	source->push(*fltstate);
}
       
/**
 * A client to send alarms: it expects to find the number of alarm to send
 * as a parameter in the command line.
 * As an option, it is also possible to set in the command line the
 * fault family and the fault member.
 * By default the fault family is "ALCLIENT" and the Fault Member is (ALARM).
 * The fault code is always set to 1.
 * For each alarm to send a triplet like <ALCLIENT, ALARM_1, 1> is created.
 * <P>
 * For each alarm, the process activates it and then terminates it.
 * First all the alarms are activated then terminated.
 * <P>
 * The purposes of this process are:
 * <UL>
 * 	<LI>Check if sending alarms from a client works
 * 	<LI>have a client that sends alarms from the command line
 *  <LI>allow to investigate problems in the sources without the complexity of the container
 * <UL>
 */
int main(int argc, char *argv[])
{
    // Checks command-line arguments.
    if (argc !=2 && argc!=4)
	{
	ACS_SHORT_LOG((LM_INFO, "Usage: %s <num. of alarms to send> [<FaultFamily Fault_Member>]", argv[0]));
	return -1;
	}
    
    //Creates and initializes the SimpleClient object
    SimpleClient client;
    if (client.init(argc,argv) == 0)
	{
	ACE_DEBUG((LM_DEBUG,"Cannot init client"));
	return -1;
	}
    else
	{
	//Must log into manager before we can really do anything
	client.login();
	}

    // Get the number of alarms to send
    int alarmsToSend = (int)strtof(argv[1],NULL);
    if (alarmsToSend==0) {
    	// It can happen if the user set 0 as param or if the conversion made
    	// by strtof failed
    	ACS_SHORT_LOG((LM_ERROR, "%d alarms to send: nothing to do. Check val of first param (%s). Is it a number?", alarmsToSend,argv[1]));
    	return -1;
    }

    // Init the alarm system factory
    ACSAlarmSystemInterfaceFactory::init(client.manager());
    ACS_SHORT_LOG((LM_DEBUG, "ACSAlarmSystemInterfaceFactory initialized"));

    std::string FF="ALCLIENT";
    std::string FM="ALARM";
    const int FC=1;
    if (argc==4) {
    	FF=argv[2];
    	FM=argv[3];
    }
    ACS_SHORT_LOG((LM_INFO, "Generating %d alarms with triplets like <%s, %s_n, 1>", alarmsToSend,FF.c_str(), FM.c_str()));

    // create the AlarmSystemInterface
 	auto_ptr<acsalarm::AlarmSystemInterface> alarmSource(ACSAlarmSystemInterfaceFactory::createSource());
 	ACS_SHORT_LOG((LM_DEBUG, "Source created"));

 	ACS_SHORT_LOG((LM_INFO, "Sending ACTIVE alarms"));
 	for (int t=0; t<alarmsToSend; t++) {
 		char tempStr[8];
 		sprintf(tempStr,"%d",t);
 		std::string fmTosend=FM;
 		fmTosend+='_';
 		fmTosend+=tempStr;
 		sendAlarm(alarmSource.get(),FF,fmTosend,FC,true);
 	}

 	sleep(5);

 	ACS_SHORT_LOG((LM_INFO, "Sending TERMINATE alarms"));
 	for (int t=0; t<alarmsToSend; t++) {
 		char tempStr[8];
 		sprintf(tempStr,"%d",t);
		std::string fmTosend=FM;
		fmTosend+='_';
		fmTosend+=tempStr;
		sendAlarm(alarmSource.get(),FF,fmTosend,FC,false);
	}
 	auto_ptr<acsalarm::AlarmSystemInterface> tstSource(ACSAlarmSystemInterfaceFactory::createSource());

// 	ACSAlarmSystemInterfaceFactory::done();

 	client.logout();

 	ACS_SHORT_LOG((LM_INFO, "%s done.",argv[0]));
    return 0;
}

/*___oOo___*/




