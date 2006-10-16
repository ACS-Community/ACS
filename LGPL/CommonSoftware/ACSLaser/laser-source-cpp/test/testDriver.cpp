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
*
* "@(#) $Id: testDriver.cpp,v 1.5 2006/10/16 16:57:24 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring 2006-09-06 created
*/

#include <maciSimpleClient.h>
#include <acsncSimpleConsumer.h>
#include <testCppAlarmSourceComponentsC.h>
#include <AlarmSystemC.h>
#include "ACSJMSMessageEntityC.h"

using namespace maci;

/* ----------------------------------------------------------------*/
void myHandlerFunction(com::cosylab::acs::jms::ACSJMSMessageEntity evt, void * handlerParam)
{
   int *eventCount = (int *)handlerParam;
   std::cout << "Detected notification channel event: " 
	     << ++(*eventCount) << std::endl;
   std::cout << "Event text was: " << evt.text << std::endl;
}

/*******************************************************************************/
void printUsageAndExit()
{
	std::cout << "\n\nUsage: \n\n" << "testDriver <NUM_ALARMS_TO_SEND>\n\n" << "where NUM_ALARMS_TO_SEND is how many alarms you wish to send.\n\n";
	exit(-1);	
}

int main(int argc, char *argv[])
{
	if(argc < 2)
	{
		printUsageAndExit();
	}
	int numAlarmsToSend = atoi(argv[1]);
	
	// Create and initialize the SimpleClient object
	SimpleClient client;
	if (client.init(argc,argv) == 0)
	{
		return -1;
	}
	else
	{
		// Must log into manager before we can really do anything
		client.login();
	}
   
	// Get the component which will be used to generate alarms
	testalarmsystem::AlarmTestMount_var alarmTestMount = client.get_object<testalarmsystem::AlarmTestMount>("ALARM_SOURCE_MOUNTCPP", 0, true);
    
	// set up a consumer to listen to the notification channel for alarms
	int receivedEvtCount = 0;
	nc::SimpleConsumer<com::cosylab::acs::jms::ACSJMSMessageEntity> *m_simpConsumer_p = 0;
   ACS_NEW_SIMPLE_CONSUMER(m_simpConsumer_p, com::cosylab::acs::jms::ACSJMSMessageEntity, 
		"CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES", myHandlerFunction, (void*) & receivedEvtCount);
   m_simpConsumer_p->consumerReady();

	ACE_Time_Value tv(30);
	client.run(tv);

	int sentEvtCount = 0;
	int MAX_TIME_TO_WAIT = 30;
	int timeWaited = 0;
	while(receivedEvtCount < numAlarmsToSend && (timeWaited < MAX_TIME_TO_WAIT))
	{
		// generate an alarm
		if(sentEvtCount < numAlarmsToSend)
		{
			alarmTestMount->faultMount();
			sentEvtCount++;
		}
		ACE_Time_Value tv(1);
		client.run(tv);
		timeWaited++;
	}
    
	if(receivedEvtCount >= numAlarmsToSend)
	{
		std::cout << "disconnecting consumer" << std::endl;
		std::cout << "received: " << receivedEvtCount << " events, and sent: " << sentEvtCount << " events" << std::endl;
	}
	else
	{
		std::cout << "ERROR: never detected all the events before the timeout elapsed" << std::endl;
	}
  	m_simpConsumer_p->disconnect();   
	m_simpConsumer_p = 0;

	// release the component and logout from manager
	client.manager()->release_component(client.handle(), "ALARM_SOURCE_MOUNTCPP");
	client.logout();
    
	// Sleep for 10 sec to allow everything to cleanup and stablize
	ACE_OS::sleep(10);
	return 0;
}


/*___oOo___*/
