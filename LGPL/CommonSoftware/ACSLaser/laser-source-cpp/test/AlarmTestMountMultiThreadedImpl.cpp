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

#include <AlarmTestMountMultiThreadedImpl.h>
#include "ACSAlarmSystemInterfaceFactory.h"
#include "FaultState.h"
#include "faultStateConstants.h"
#include <acsThread.h>
#include <sstream>

using namespace acscomponent;
using namespace testalarmsystem;
using std::stringstream;
using std::string;
using std::auto_ptr;
using acsalarm::Properties;
using acsalarm::Timestamp;
using acsalarm::AlarmSystemInterface;


class SenderThread : public ACS::Thread
{
	private:
		int sent;
		CORBA::Long toSend;
		AlarmTestMountMultiThreadedImpl *mount;
		string nameStr;

	public:

		SenderThread(const ACE_CString &name, AlarmTestMountMultiThreadedImpl *mountPtr, 
			const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
			ACS::Thread(name), mount(mountPtr), nameStr(name.c_str())
		{
			ACS_TRACE("SenderThread::SenderThread entering with thread named: " + nameStr); 
			ACS_TRACE("SenderThread::SenderThread exiting with thread named: " + nameStr); 
		}

		~SenderThread() 
		{ 
			ACS_TRACE("SenderThread::~SenderThread entering with thread named: " + nameStr); 
			ACS_TRACE("SenderThread::~SenderThread exiting with thread named: " + nameStr); 
		}
    
   	virtual void onStart()
		{
			ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::onStart entering with thread named: " + nameStr);
			ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::onStart exiting with thread named: " + nameStr);
		}

   	virtual void onStop()
		{
			ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::onStop entering with thread named: " + nameStr);
			mount->stop();
			ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::onStop exiting with thread named: " + nameStr);
		}
	
		virtual void runLoop()
		{
			ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::runLoop entering with thread named: " + nameStr);

			string family("Mount");
			string member("ALARM_SOURCE_MOUNTCPP");
				
			if(sent++ < toSend)
			{
				ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::sending alarm from thread named: " + nameStr);


				// TODO: remove this when uncommenting the above lines. 
				if(check()) 
				{
					// alternate using the "short-hand" and the "long-hand" techniques for sending the alarm 
					// so that we will have test coverage of both styles of sending an alarm

					//if((sent %1000) == 0)
					//{
					//	mount->sendAlarmLongHand(family, member, 1, true);
					//	ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::taking a rest in thread: " + nameStr);
					//	ACE_OS::sleep(1);
					//}
					//else if((sent %7) == 0)
					//{
						mount->sendAlarmShortHand(family, member, 1, true);
					//}
					//else if((sent %13) == 0)
					//{
					//	mount->sendAlarmControlStyle();
					//}
					//else if((sent % 19) == 0) 
					//{
						//mount->sendAlarmShortHandWithoutProperties(family, member, 1, true);
					//}
					//else 
					//{
						//mount->sendAlarmLongHand(family, member, 1, true);
					//}
				}
			}
			else
			{
				setStopped();	
				ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::runLoop stopping thread named: " + nameStr);
			}
			ACS_TRACE("::AlarmTestMountMultiThreaded::SenderThread::runLoop exiting with thread named: " + nameStr);
		}

		void setNumToSend(CORBA::Long num)
		{
			sent = 0;
			toSend = num;
		}

		void setMount(AlarmTestMountMultiThreadedImpl* sendingMount)
		{
			mount = sendingMount;
		}
};

AlarmTestMountMultiThreadedImpl::AlarmTestMountMultiThreadedImpl(const ACE_CString &name,maci::ContainerServices * containerServices) : 
    ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::AlarmTestMountMultiThreaded");
	stopped = false;
	alarmSource = ACSAlarmSystemInterfaceFactory::createSource("ALARM_SYSTEM_SOURCES");
}

AlarmTestMountMultiThreadedImpl::~AlarmTestMountMultiThreadedImpl()
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::~AlarmTestMountMultiThreaded");
	stopped = true;
}

void AlarmTestMountMultiThreadedImpl::faultMount(CORBA::Long threads, CORBA::Long numFaultsPerMount, CORBA::Long mountNumber)
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::faultMount entering");
	SenderThread *senderThreads[threads];
	for(int i = 0; i < threads; i++) 
	{
		stringstream out;
		out << i;
		string threadNumberString = out.str();
		string threadName("thread_");
		threadName += threadNumberString;

		stringstream out2;
		out2 << mountNumber;
		string mountNumberString = out2.str();
		string mountName("_mount_");
		mountName += mountNumberString;

		threadName += mountName;

		AlarmTestMountMultiThreadedImpl * selfPtr = this;
		senderThreads[i] = getContainerServices()->getThreadManager()->create<SenderThread, AlarmTestMountMultiThreadedImpl*>(threadName.c_str(), selfPtr); 
		senderThreads[i]->setNumToSend(numFaultsPerMount);
		senderThreads[i]->resume();
	}
	bool *done = new bool[threads];
	bool allDone = false;
	for (int i = 0; i < threads; i++)
	{
		done[i] = false;
	}

	while(!allDone)
	{
		for(int i = 0; i < threads; i++)
		{
			if(senderThreads[i]->isStopped()) 
			{
				done[i] = true;
			}
		}
		for(int j = 0; j < threads; j++) 
		{
			if(done[j] != true)
			{
				ACE_OS::sleep(2);
				break;
				//for(int i = 0; i < threads; i++)
				//{
					//ACE_OS::sleep(2);
					//ACS_TRACE("::AlarmTestMountMultiThreaded::~AlarmTestMountMultiThreaded destroying sender threads");
					//getContainerServices()->getThreadManager()->destroy(senderThreads[i]);
				//}
			}
			allDone = true;
		}
	}
	ACS_TRACE("::AlarmTestMountMultiThreaded::faultMount exiting");
}

void AlarmTestMountMultiThreadedImpl::terminate_faultMount() 
{
	string family("Mount");
	string member("ALARM_SOURCE_MOUNTCPP");
	sendAlarmLongHand(family, member, 1, false);
}

void AlarmTestMountMultiThreadedImpl::sendAlarmLongHand(std::string& family, std::string& member, int code, bool active) 
{
	string noneStr("none");
	sendAlarmLongHand(family, member, code, active, noneStr);
}

void AlarmTestMountMultiThreadedImpl::sendAlarmLongHand(std::string& family, std::string& member, int code, bool active, string & threadName) 
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmLongHand entering for thread: " + threadName);
	//if(stopped) 
	//{
		//ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmLongHand mount is stopped for thread: " + threadName);

		// NOTE: if this line is uncommented, it *may* prevent the container crash, however, I leave it
		// commented out because I think crashing the container will help us to locate the problem.
		//return;
	//}

	// create the FaultState
	try 
	{
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
		//alarmSource->push(*fltstate);
		AlarmSystemInterface* myInterface = ACSAlarmSystemInterfaceFactory::createSource("ALARM_SYSTEM_SOURCES");
		//myInterface->setThreadName(threadName);
		myInterface->push(*fltstate); 
		// TODO: remove above line, uncomment line above it...
	}
	catch(acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl ex)
	{
		ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmLongHand caught an acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl exception in thread: " + threadName);
	}

	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmLongHand exiting for thread: " + threadName);
}

void AlarmTestMountMultiThreadedImpl::sendAlarmShortHand(std::string& family, std::string& member, int code, bool active) 
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmShortHand entering");

	// create a Properties object and configure it, then assign to the FaultState
	Properties props;
	props.setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, "prefix");
	props.setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, "suffix");
	props.setProperty("TEST_PROPERTY", "TEST_VALUE");

	ACSAlarmSystemInterfaceFactory::createAndSendAlarm(family, member, code, active, props);

	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmShortHand exiting");
}

void AlarmTestMountMultiThreadedImpl::sendAlarmShortHandWithoutProperties(std::string& family, std::string& member, int code, bool active) 
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmShortHandWithoutProperties entering");
	ACSAlarmSystemInterfaceFactory::createAndSendAlarm(family, member, code, active);
	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmShortHandWithoutProperties exiting");
}

void AlarmTestMountMultiThreadedImpl::sendAlarmControlStyle()
{
	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmControlStyle entering");

	// Send the fault. We must use the "ALARM_SYSTEM_SOURCES" name
	acsalarm::FaultState faultState("ControlAlarmFamily", "ControlAlarmMember", 10);
	faultState.setDescriptor(faultState::ACTIVE_STRING);
	ACSAlarmSystemInterfaceFactory::createSource("ALARM_SYSTEM_SOURCES")->push(faultState); 

	ACS_TRACE("::AlarmTestMountMultiThreaded::sendAlarmControlStyle exiting");
}


/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(AlarmTestMountMultiThreadedImpl)
/* ----------------------------------------------------------------*/
