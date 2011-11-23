/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005
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
*/
#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>
#include <cdbjDALC.h>
#include <stdio.h>
#include <iostream>
#include <fstream>
#include <logging.h>
#include <loggingGenericLogger.h>
#include <ACSAlarmSystemInterfaceFactory.h>
#include <acsutilPorts.h>
#include <stdlib.h>
#include <maciHelper.h>

#define XML_HEADER_LENGTH 4
#define XML_HEADER_LINE_ONE "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
#define XML_HEADER_LINE_TWO "<alarm-system-configuration xmlns=\"urn:schemas-cosylab-com:acsalarm-alarmservice:1.0\" \n\t\t"
#define XML_HEADER_LINE_THREE "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> \n"
#define XML_HEADER_LINE_FOUR "\t<configuration-property name=\"Implementation\">"
#define XML_TAIL "</configuration-property>\n</alarm-system-configuration>\n"

using namespace std;
using acsalarm::AlarmSystemInterface;

/**
 * NOTE: This test is similar to the FactoryTestCase in ACS/LGPL/CommonSoftware/acsalarm/test. This test does not actually send alarms; 
 * it only checks which style alarms would be sent in different scenarios. It differs from the acsalarm test in that we are dealing with
 * the CERN style here instead of the ACS (logging) style.
 *
 * FINAL NOTE: this test uses CERN style alarms and is located here (rather than in acsalarm) because it requires initializing the
 * ACSAlarmSystemFactory DLL which is not possible (yet) when acsalarm module is built (as it requires this module).
 */
class FactoryTestCase : public CPPUNIT_NS::TestFixture
{
	CPPUNIT_TEST_SUITE(FactoryTestCase);
	CPPUNIT_TEST(testCERNAlarmSystem);
	CPPUNIT_TEST(testFaultStateCreation);
	CPPUNIT_TEST(testAlarmSourceCreation);
	CPPUNIT_TEST_SUITE_END();

	public:
		FactoryTestCase();
		~FactoryTestCase();
		void setUp();
		void tearDown();
		
	protected:
		void testCERNAlarmSystem();
		void testFaultStateCreation();
		void testAlarmSourceCreation();

	private:
		CORBA::ORB_var orb;
};

FactoryTestCase::FactoryTestCase()
{
	ACS_TRACE("FactoryTestCase::FactoryTestCase()");
}

FactoryTestCase::~FactoryTestCase()
{
	ACS_TRACE("FactoryTestCase::~FactoryTestCase()");
}

void FactoryTestCase::setUp()
{
	ACS_TRACE("FactoryTestCase::setUp entering");
	int  nargc=0;
	char **nargv=0;
	orb =  CORBA::ORB_init (nargc, nargv, 0);
	maci::Manager_ptr manager = maci::MACIHelper::resolveManager (orb.in(), nargc, nargv, 0, 0);
	ACSAlarmSystemInterfaceFactory::init(manager);
}

void FactoryTestCase::tearDown()
{
	ACS_TRACE("FactoryTestCase::tearDown()");
	ACSAlarmSystemInterfaceFactory::done();
}

/**
 * Check if the CERN implementation of the AS is choosen when
 * there CERN is in the CDB
 * 
 * @throws Exception
 */
void FactoryTestCase::testCERNAlarmSystem() 
{
	ACS_TRACE("FactoryTestCase::testCERNAlarmSystem()");
	CPPUNIT_ASSERT_MESSAGE("Wrong implementation in use (CERN case)", !ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem());
}
	

/**
 * Test the creation of a FaultState
 * 
 * @throws Exception
 */
void FactoryTestCase::testFaultStateCreation()
{
	ACS_TRACE("FactoryTestCase::testFaultStateCreation()");
	auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState("Family", "Member", 0);
	CPPUNIT_ASSERT_MESSAGE("Error creating a FS", (fltstate.get() != NULL));
}

/**
 * Test the creation of a source (proxy)
 * 
 * @throws Exception
 */
void FactoryTestCase::testAlarmSourceCreation()
{
	ACS_TRACE("FactoryTestCase::testAlarmSourceCreation()");
	AlarmSystemInterface* alarmSource = ACSAlarmSystemInterfaceFactory::createSource();
	CPPUNIT_ASSERT_MESSAGE("Error creating an alarm source", (alarmSource!= NULL));
}
	
CPPUNIT_TEST_SUITE_REGISTRATION(FactoryTestCase);

int main(int argc, char *argv[])
{
	Logging::Logger::setGlobalLogger(new Logging::GenericLogger("testLogger"));

	// Create the event manager and test controller
	CPPUNIT_NS::TestResult controller;

	// Add a listener that colllects test result
	CPPUNIT_NS::TestResultCollector result;
	controller.addListener( &result );        

	// Add a listener that print dots as test run.
	CPPUNIT_NS::BriefTestProgressListener progress;
	controller.addListener( &progress );      

	// Add the top suite to the test runner
	CPPUNIT_NS::TestRunner runner;
	runner.addTest( CPPUNIT_NS::TestFactoryRegistry::getRegistry().makeTest() );
	runner.run( controller );

	// Print test in a compiler compatible format.
	CPPUNIT_NS::CompilerOutputter outputter( &result, std::cerr );
	outputter.write(); 

	return result.wasSuccessful() ? 0 : 1;
}
