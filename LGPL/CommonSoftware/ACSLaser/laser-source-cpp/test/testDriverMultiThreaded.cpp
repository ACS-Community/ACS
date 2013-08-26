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
* "@(#) $Id: testDriverMultiThreaded.cpp,v 1.1 2008/04/06 17:46:00 sharring Exp $"
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

class SpawnSenderThread : public ACE_Task_Base
{
	private:
		CORBA::Long toSend;
		int numThreads;
		int mountNum;
		int mountNameNum;
		testalarmsystem::AlarmTestMountMultiThreaded_var alarmTestMountMultiThreaded;
		SimpleClient* client;

	public:

		virtual int svc(void)
		{
			cout << "::testDriverMultiThreaded::SpawnSenderThread::svc method entering" << std::endl;

			cout << "::testDriverMultiThreaded::SpawnSenderThread::svc method acquiring component" << std::endl;
			if(0 != mountNum)
			{
					alarmTestMountMultiThreaded = client->get_object<testalarmsystem::AlarmTestMountMultiThreaded>("ALARM_SOURCE_MOUNT_MULTITHREAD_CPP", 0, true);
			}
			else
			{
					alarmTestMountMultiThreaded = client->get_object<testalarmsystem::AlarmTestMountMultiThreaded>("ALARM_SOURCE_MOUNT_MULTITHREAD_CPP2", 0, true);
			}

			cout << "::testDriverMultiThreaded::SpawnSenderThread::svc method calling faultMount for mount " << std::endl;
			alarmTestMountMultiThreaded->faultMount(numThreads, toSend, mountNameNum);

			cout << "::testDriverMultiThreaded::SpawnSenderThread::svc method exiting" << std::endl;
			return 0;
		}

		void setClient(SimpleClient* clientptr)
		{
			client = clientptr;
		}

		void setMountNum(int num)
		{
			mountNum = num;
		}

		void setNumThreads(int num)
		{
			numThreads = num;
		}

		void setNumToSend(CORBA::Long num)
		{
			toSend = num;
		}

		void setMountNameNum(int mtNameNum)
		{
			mountNameNum = mtNameNum;
		}
};

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
	std::cout << "\n\nUsage: \n\n" << "testDriver <NUM_THREADS> <NUM_ALARMS_TO_SEND>\n\n" << "where NUM_THREADS is # of threads & NUM_ALARMS_TO_SEND is # alarms to send per thread.\n\n";
	exit(-1);	
}

int main(int argc, char *argv[])
{
	if(argc < 3)
	{
		printUsageAndExit();
	}
	int numThreads = atoi(argv[1]);
	int numAlarmsToSend = atoi(argv[2]);
	
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
   
	// set up a consumer to listen to the notification channel for alarms
	int receivedEvtCount = 0;
	nc::SimpleConsumer<com::cosylab::acs::jms::ACSJMSMessageEntity> *m_simpConsumer_p = 0;
   ACS_NEW_SIMPLE_CONSUMER(m_simpConsumer_p, com::cosylab::acs::jms::ACSJMSMessageEntity, 
		"CMW.ALARM_SYSTEM.ALARMS.SOURCES.ALARM_SYSTEM_SOURCES", myHandlerFunction, (void*) & receivedEvtCount);
   m_simpConsumer_p->consumerReady();

	ACE_Time_Value tv(5);
	client.run(tv);

	int MAX_TIME_TO_WAIT = 6000;
	int timeWaited = 0;

	// start threads
	SpawnSenderThread spawnSenderThreads[4];
	int NUM_COMPONENTS = 4;
	for(int i = 0; i < NUM_COMPONENTS; i++)
	{
		spawnSenderThreads[i].setNumToSend(numAlarmsToSend);
		spawnSenderThreads[i].setNumThreads(numThreads);
		spawnSenderThreads[i].setMountNum((i%2));
		spawnSenderThreads[i].setMountNameNum(i);
		spawnSenderThreads[i].setClient(&client);
		spawnSenderThreads[i].activate();
	}

	int totalToSend = numThreads * NUM_COMPONENTS * numAlarmsToSend;
	while((receivedEvtCount <  totalToSend) && (timeWaited < MAX_TIME_TO_WAIT))
	{
		ACE_Time_Value tv(1);
		client.run(tv);
		timeWaited++;
	}

	if(receivedEvtCount >= totalToSend)
	{
		std::cout << "received: " << receivedEvtCount << " events" << std::endl;
	}
	else
	{
		std::cout << "ERROR: never detected all the events before the timeout (" << MAX_TIME_TO_WAIT << ") elapsed" << std::endl;
	}

	// wait on threads to finish
	for(int i = 0; i < NUM_COMPONENTS; i++)
	{
		spawnSenderThreads[i].wait();
	}

	std::cout << "disconnecting consumer" << std::endl;
  	m_simpConsumer_p->disconnect();   
	m_simpConsumer_p = 0;

	// logout from manager
	cout << "client releasing component 1" << std::endl;
	client.manager()->release_component(client.handle(), "ALARM_SOURCE_MOUNT_MULTITHREAD_CPP");
	cout << "client releasing component 2" << std::endl;
	client.manager()->release_component(client.handle(), "ALARM_SOURCE_MOUNT_MULTITHREAD_CPP2");

	cout << "client logging out" << std::endl;
	client.logout();
    
	// Sleep for 10 sec to allow everything to cleanup and stablize
	ACE_OS::sleep(10);
	return 0;
}


/*___oOo___*/
