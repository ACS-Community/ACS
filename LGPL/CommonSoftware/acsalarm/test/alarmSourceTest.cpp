/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: alarmSourceTest.cpp,v 1.2 2012/03/27 07:31:50 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2011-06-21  created
*/
#include <cppunit/TestSuite.h>
#include <cppunit/TestCase.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestCaller.h>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

#include <stdio.h>
#include <iostream>
#include <fstream>
#include <logging.h>
#include <loggingGenericLogger.h>
#include <AlarmSourceImpl.h>
#include <acsutilPorts.h>
#include <stdlib.h>
#include <MockManager.h>

using namespace std;
using acsalarm::AlarmSystemInterface;

class AlarmSourceTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(AlarmSourceTestCase);
	CPPUNIT_TEST( testEnableDisable );
	CPPUNIT_TEST( testAlarmSending );
	CPPUNIT_TEST( testAlarmQueuing );
	CPPUNIT_TEST( testAlarmQueuingWithTimer );
	CPPUNIT_TEST( testTerminateAll );
	CPPUNIT_TEST_SUITE_END();

	public:
	AlarmSourceTestCase()
	{
		ACS_TRACE("AlarmSourceTestCase::AlarmSourceTestCase entering");

		myMockMgr = new maci::MockManager();
		alarmSource = new acsalarm::AlarmSourceImpl();

		ACS_TRACE("AlarmSourceTestCase::AlarmSourceTestCase exiting");
	}

	~AlarmSourceTestCase()
	{
		delete alarmSource;
		delete myMockMgr;
	}

	void setUp()
	{
		ACSAlarmSystemInterfaceFactory::init(myMockMgr);
		alarmSource->start();
	}

	void tearDown()
	{
		alarmSource->tearDown();
		ACSAlarmSystemInterfaceFactory::done();
	}

	/**
	 * Test the alarm sending feature by checking
	 * - if alarm are sent
	 * - duplicated alarms are not sent
	 *
	 */
	void testAlarmSending()
	{
		std::cout<<"testAlarmSending"<<std::endl;
		std::string ff="AlarmSendingFF";
		std::string fm="AlarmSendingFM";

		// raise alarms
		for (int t=0; t<10; t++) {
			alarmSource->raiseAlarm(ff,fm,t);
		}
		// clear alarms
		for (int t=0; t<10; t++) {
			alarmSource->clearAlarm(ff,fm,t);
		}
		// Send more alarms with set
		for (int t=11; t<20; t++) {
			alarmSource->setAlarm(ff,fm,t,t%2==0);
		}
		sleep(5);
	}

	/**
	 * Test if the alarm  are sent or not after disabling the source
	 */
	void testEnableDisable()
	{
		std::cout<<"testEnableDisable"<<std::endl;
		std::string ff="AlarmEnDisFF";
		std::string fm="AlarmEnDisFM";

		// Send an alarm to check that the sending is working
		alarmSource->raiseAlarm(ff,fm,0);
		alarmSource->disableAlarms();
		// Send several alarms with set (they must not appear!)
		for (int t=11; t<20; t++) {
			alarmSource->setAlarm(ff,fm,t,t%2==0);
		}
		alarmSource->enableAlarms();
		// Send an alarm to check that the sending is working (this must appear)
		alarmSource->raiseAlarm(ff,fm,1);
		// Clear the alarms
		alarmSource->clearAlarm(ff,fm,0);
		alarmSource->clearAlarm(ff,fm,1);
		sleep(5);
	}

	/**
	 * Test the queueing of alarms
	 */
	void testAlarmQueuing()
	{
		std::cout<<"testAlarmQueuing"<<std::endl;
		std::string ff="AlarmQueuingFF";
		std::string fm="AlarmQueuingFM";

		// This alarm will be sent immediately
		alarmSource->raiseAlarm(ff,fm,0);

		std::cout<<"testAlarmQueuing: Start queuing"<<std::endl;
		alarmSource->queueAlarms();
		// Send several alarms with set several times
		// After queuing they must appear only once when flushing!
		for (int j=0; j<5; j++) {
			for (int t=11; t<20; t++) {
				alarmSource->setAlarm(ff,fm,t,t%2==0);
			}
		}
		std::cout<<"testAlarmQueuing: Flushing queue"<<std::endl;
		alarmSource->flushAlarms();
		std::cout<<"testAlarmQueuing: sending an alarm without queuing"<<std::endl;
		// This alarm will be sent immediately
		alarmSource->raiseAlarm(ff,fm,1);
		sleep(5);
	}

	/**
	 * Test the queueing of alarms
	 */
	void testAlarmQueuingWithTimer()
	{
		std::cout<<"testAlarmTimerQueuing"<<std::endl;
		std::string ff="AlarmTimerQueuingFF";
		std::string fm="AlarmTimerQueuingFM";

		// This alarm will be sent immediately
		alarmSource->raiseAlarm(ff,fm,0);

		std::cout<<"testAlarmTimerQueuing: Start queuing for 30 seconds"<<std::endl;
		alarmSource->queueAlarms(300000000);
		// Send several alarms with set several times
		// After queuing they must appear only once when flushing!
		for (int j=0; j<5; j++) {
			for (int t=11; t<20; t++) {
				alarmSource->setAlarm(ff,fm,t,t%2==0);
			}
		}
		std::cout<<"testAlarmTimerQueuing: waiting"<<std::endl;
		sleep(35);
		std::cout<<"testAlarmTimerQueuing: sending an alarm without queuing"<<std::endl;
		// This alarm will be sent immediately
		alarmSource->raiseAlarm(ff,fm,1);
		sleep(5);
	}


	void testTerminateAll()
	{
		std::cout<<"testTerminateAll"<<std::endl;
		std::string ff="AlarmTerminateAllFF";
		std::string fm="AlarmTerminateAllFM";

		// Send several alarms with set several times
		std::cout<<"Activating 10 alarms"<<std::endl;
		for (int t=0; t<10; t++) {
			alarmSource->raiseAlarm(ff,fm,t);
		}
		std::cout<<"Terminating alarm with code 5"<<std::endl;
		// Remove alarm 5 to be sure it is not cleared again
		alarmSource->clearAlarm(ff,fm,5);
		sleep(3);
		std::cout<<"Terminating all alarms i.e [0-4]+[6-9]"<<std::endl;
		alarmSource->terminateAllAlarms();
		sleep(5);
	}

	private:
		maci::MockManager * myMockMgr;

		// The object to test
		acsalarm::AlarmSource* alarmSource;
};

CPPUNIT_TEST_SUITE_REGISTRATION(AlarmSourceTestCase);

int main(int argc, char *argv[])
{
	LoggingProxy::ProcessName("AlarmSourceTestCase");
    LoggingProxy *m_logger = new LoggingProxy (0, 0, 31, 0);
    LoggingProxy::init (m_logger);
    ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created."));

	CppUnit::TextUi::TestRunner runner;
	runner.addTest( AlarmSourceTestCase::suite() );
	runner.run();

	ACS_SHORT_LOG((LM_INFO, "Flushing logs."));
	m_logger->flush();
	LoggingProxy::done();
    delete m_logger;

	return 0;
}


