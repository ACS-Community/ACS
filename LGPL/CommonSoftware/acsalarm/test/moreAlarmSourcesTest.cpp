/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
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
* "@(#) $Id: moreAlarmSourcesTest.cpp,v 1.1 2012/04/26 12:43:46 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-04-26  created 
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

/**
 * AlarmSourcesTestCase test the AlarmSources when updated by a single thread.
 * As the functioning of a single AlarmSource is tested by another thread,
 * this test does not call all the method of AlarmSource but only those
 * that are interesting in respect of being updated by a single thread.
 */
class AlarmSourcesTestCase : public CppUnit::TestFixture
{
	CPPUNIT_TEST_SUITE(AlarmSourcesTestCase);
	CPPUNIT_TEST( testAlarmSending );
	CPPUNIT_TEST( testEnableDisable );
	CPPUNIT_TEST( testAlarmQueuing );
	CPPUNIT_TEST( testAlarmQueuingWithTimer );
	CPPUNIT_TEST_SUITE_END();

	public:
	AlarmSourcesTestCase()
	{
		ACS_TRACE("AlarmSourcesTestCase::AlarmSourceTestCase entering");

		myMockMgr = new maci::MockManager();
		thread = new acsalarm::AlarmSourceThread();
		alarmSource1 = new acsalarm::AlarmSourceImpl(thread);
		alarmSource2 = new acsalarm::AlarmSourceImpl(thread);

		ACS_TRACE("AlarmSourcesTestCase::AlarmSourceTestCase exiting");
	}

	~AlarmSourcesTestCase()
	{
		delete alarmSource2;
		delete alarmSource1;
		delete thread;
		delete myMockMgr;
	}

	void setUp()
	{
		ACSAlarmSystemInterfaceFactory::init(myMockMgr);
		alarmSource1->start();
		alarmSource2->start();
	}

	void tearDown()
	{
		alarmSource1->tearDown();
		alarmSource2->tearDown();
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
		std::string fm1="AlarmSendingFM_1";
		std::string fm2="AlarmSendingFM_2";

		// raise alarms on source 1
		for (int t=0; t<10; t++) {
			alarmSource1->raiseAlarm(ff,fm1,t);
		}
		// raise alarms on source 1
		for (int t=0; t<10; t++) {
			alarmSource2->raiseAlarm(ff,fm2,t);
		}
		// clear alarms on both sources
		for (int t=0; t<10; t++) {
			alarmSource1->clearAlarm(ff,fm1,t);
			alarmSource2->clearAlarm(ff,fm2,t);
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
		std::string fm1="AlarmEnDisFM_1";
		std::string fm2="AlarmEnDisFM_2";

		// Send an alarm to check that the sending is working
		alarmSource1->raiseAlarm(ff,fm1,0);
		alarmSource1->disableAlarms();
		alarmSource2->raiseAlarm(ff,fm2,0);
		alarmSource2->disableAlarms();
		// Send several alarms with set (they must not appear!)
		for (int t=11; t<20; t++) {
			alarmSource1->setAlarm(ff,fm1,t,t%2==0);
			alarmSource2->setAlarm(ff,fm2,t,t%2==0);
		}
		alarmSource1->enableAlarms();
		alarmSource2->enableAlarms();
		// Send an alarm to check that the sending is working (this must appear)
		alarmSource1->raiseAlarm(ff,fm1,1);
		alarmSource2->raiseAlarm(ff,fm2,1);
		// Clear the alarms
		alarmSource1->clearAlarm(ff,fm1,0);
		alarmSource2->clearAlarm(ff,fm2,1);
		sleep(5);
	}

	/**
	 * Test the queueing of alarms
	 */
	void testAlarmQueuing()
	{
		std::cout<<"testAlarmQueuing"<<std::endl;
		std::string ff="AlarmQueuingFF";
		std::string fm1="AlarmQueuingFM_1";
		std::string fm2="AlarmQueuingFM_2";

		// This alarm will be sent immediately
		alarmSource1->raiseAlarm(ff,fm1,0);
		alarmSource2->raiseAlarm(ff,fm2,0);

		std::cout<<"testAlarmQueuing: Start queuing on source 1"<<std::endl;
		alarmSource1->queueAlarms();

		// Send an alarm with the source 2
		alarmSource2->raiseAlarm(ff,fm2,1);

		std::cout<<"testAlarmQueuing: Start queuing on source 2"<<std::endl;
		alarmSource2->queueAlarms();

		// Send several alarms with set
		// After queuing they must appear only once when flushing!
		for (int j=0; j<5; j++) {
			for (int t=11; t<20; t++) {
				alarmSource1->setAlarm(ff,fm1,t,t%2==0);
				alarmSource2->setAlarm(ff,fm2,t,t%2==0);
			}
		}
		std::cout<<"testAlarmQueuing: Flushing queue 1"<<std::endl;
		alarmSource1->flushAlarms();
		std::cout<<"testAlarmQueuing: Flushing queue 2"<<std::endl;
		alarmSource2->flushAlarms();

		std::cout<<"testAlarmQueuing: sending 2 alarm2 without queuing"<<std::endl;
		// These alarms will be sent immediately
		alarmSource1->raiseAlarm(ff,fm1,2);
		alarmSource2->raiseAlarm(ff,fm2,2);
		sleep(5);
	}

	/**
	 * Test the queueing of alarms
	 */
	void testAlarmQueuingWithTimer()
	{
		std::cout<<"testAlarmTimerQueuing"<<std::endl;
		std::string ff="AlarmTimerQueuingFF";
		std::string fm1="AlarmTimerQueuingFM_1";
		std::string fm2="AlarmTimerQueuingFM_2";

		// This alarm will be sent immediately
		alarmSource1->raiseAlarm(ff,fm1,0);
		alarmSource2->raiseAlarm(ff,fm2,0);

		std::cout<<"testAlarmTimerQueuing: Start queuing source 1 for 20 seconds"<<std::endl;
		alarmSource1->queueAlarms(200000000);
		std::cout<<"testAlarmTimerQueuing: Start queuing source 2 for 30 seconds"<<std::endl;
		alarmSource2->queueAlarms(300000000);
		// Send several alarms
		// After queuing they must appear only once when flushing!
		for (int j=0; j<5; j++) {
			for (int t=11; t<20; t++) {
				alarmSource1->setAlarm(ff,fm1,t,t%2==0);
				alarmSource2->setAlarm(ff,fm2,t,t%2==0);
			}
		}
		std::cout<<"testAlarmTimerQueuing: waiting source 1 alarms"<<std::endl;
		sleep(25);
		std::cout<<"testAlarmTimerQueuing: waiting source 2 alarms"<<std::endl;
		sleep(10);
		std::cout<<"testAlarmTimerQueuing: sending 2 alarm2 without queuing"<<std::endl;
		// This alarm will be sent immediately
		alarmSource1->raiseAlarm(ff,fm1,1);
		alarmSource2->raiseAlarm(ff,fm2,1);
		sleep(5);
	}

	private:
		maci::MockManager * myMockMgr;

		// The thread to update the AlarmSource
		acsalarm::AlarmSourceThread* thread;

		// The first source object to test
		acsalarm::AlarmSource* alarmSource1;

		// The second source object to test
		acsalarm::AlarmSource* alarmSource2;

};

CPPUNIT_TEST_SUITE_REGISTRATION(AlarmSourcesTestCase);

int main(int argc, char *argv[])
{
	LoggingProxy::ProcessName("AlarmSourcesTestCase");
    LoggingProxy *m_logger = new LoggingProxy (0, 0, 31, 0);
    LoggingProxy::init (m_logger);
    ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created."));

	CppUnit::TextUi::TestRunner runner;
	runner.addTest( AlarmSourcesTestCase::suite() );
	runner.run();

	ACS_SHORT_LOG((LM_INFO, "Flushing logs."));
	m_logger->flush();
	LoggingProxy::done();
    delete m_logger;

	return 0;
}


