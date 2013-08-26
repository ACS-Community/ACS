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
#include <MockManager.h>

#define XML_HEADER_LENGTH 4
#define XML_HEADER_LINE_ONE "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?>\n"
#define XML_HEADER_LINE_TWO "<alarm-system-configuration xmlns=\"urn:schemas-cosylab-com:acsalarm-alarmservice:1.0\" \n\t\t"
#define XML_HEADER_LINE_THREE "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> \n"
#define XML_HEADER_LINE_FOUR "\t<configuration-property name=\"Implementation\">"
#define XML_TAIL "</configuration-property>\n</alarm-system-configuration>\n"

using namespace std;
using acsalarm::AlarmSystemInterface;

/**
 * NOTE: this tests the following scenarios:
 *
 * 1) NO "Alarms" branch present in the CDB - in this case, the "ACS" style alarms (i.e. to the logging system) should be used
 * 2) "ACS" as the implementation specified in the CDB/Alarms/AlarmSystemConfiguration/AlarmSystemConfiguration.xml file
 *    in this case, the ACS style alarms (i.e. to the logging system) should be used.
 * 3) An erroneous value (neither ACS nor CERN) in the CDB/Alarms/AlarmSystemConfiguration/AlarmSystemConfiguration.xml file;
 *    in this case, also, ACS style alarms (i.e. to the logging system) should be sent.
 *
 * NOTE ALSO: this test does not actually send alarms; it only checks which style alarms would be sent in different scenarios.
 *
 * FINAL NOTE: this test does not test CERN style alarms because this requires initializing the ACSAlarmSystemFactory with a non-NULL
 * manager reference, which we cannot do at this point in the build. This scenario is tested in the ACSLaser/laser-source-cpp/test
 * module.
 */
class FactoryTestCase : public CPPUNIT_NS::TestFixture
{
	CPPUNIT_TEST_SUITE(FactoryTestCase);
	CPPUNIT_TEST(testNoAlarmBranch);
	CPPUNIT_TEST(testACSAlarmSystem);
	CPPUNIT_TEST(testWrongImplementationProp);
	CPPUNIT_TEST(testFaultStateCreation);
	CPPUNIT_TEST(testAlarmSourceCreation);
	CPPUNIT_TEST_SUITE_END();

	public:
		FactoryTestCase();
		~FactoryTestCase();
		void setUp();
		void tearDown();

	protected:
		void testNoAlarmBranch();
		void testACSAlarmSystem();
		void testWrongImplementationProp();
		void testFaultStateCreation();
		void testAlarmSourceCreation();

	private:
		char cwd[1024];
		maci::MockManager * myMockMgr;
		void configureAlarmBranch(string cdbdir, string alarmSystemImplementation);
		void replaceAlarmBranch(string cdbdir);
		void renameAlarmBranch(string cdbdir);
		void clearCdbCache();
};

FactoryTestCase::FactoryTestCase()
{
	ACS_TRACE("FactoryTestCase::FactoryTestCase entering");

	if (getcwd(cwd,sizeof(cwd)) == (char *) NULL) {
		ACS_SHORT_LOG((LM_ERROR, "FactoryTestCase::FactoryTestCase() - Failed to get current working dir"));
	}
	myMockMgr = new maci::MockManager();

	ACS_TRACE("FactoryTestCase::FactoryTestCase exiting");
}

FactoryTestCase::~FactoryTestCase()
{
	delete myMockMgr;
}

void FactoryTestCase::setUp()
{
}

void FactoryTestCase::tearDown()
{
	configureAlarmBranch(cwd,"ACS");
}

void FactoryTestCase::clearCdbCache()
{
	system("cdbjDALClearCache");
}

/**
 * Check if the ACS implementation of the AS is choosen when
 * there is no Alarm branch in the CDB
 *
 * @throws Exception
 */
void FactoryTestCase::testNoAlarmBranch() {
	renameAlarmBranch(cwd);
	clearCdbCache();
	ACSAlarmSystemInterfaceFactory::init(myMockMgr);
	replaceAlarmBranch(cwd);
	CPPUNIT_ASSERT_MESSAGE("Wrong implementation in use (no Alarms in CDB case)", ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem()==ACS_AS);
	ACSAlarmSystemInterfaceFactory::done();
}

/**
 * Check if the ACS implementation of the AS is choosen when
 * there ACS is in the CDB
 *
 * @throws Exception
 */
void FactoryTestCase::testACSAlarmSystem() {
	configureAlarmBranch(cwd, "ACS");
	clearCdbCache();
	ACSAlarmSystemInterfaceFactory::init(myMockMgr);
	CPPUNIT_ASSERT_MESSAGE("Wrong implementation in use (ACS case)", ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem()==ACS_AS);
	ACSAlarmSystemInterfaceFactory::done();
}

/**
 * Check if the ACS implementation is used when the Implementation property is wrong
 * @throws Exception
 */
void FactoryTestCase::testWrongImplementationProp() {
	configureAlarmBranch(cwd, "Wrong property");
	clearCdbCache();
	ACSAlarmSystemInterfaceFactory::init(myMockMgr);
	CPPUNIT_ASSERT_MESSAGE("Wrong implementation in use (wrong prop case)", ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem()==ACS_AS);
	ACSAlarmSystemInterfaceFactory::done();
}

/**
 * Test the creation of a FaultState
 *
 * @throws Exception
 */
void FactoryTestCase::testFaultStateCreation()
{
	configureAlarmBranch(cwd, "ACS");
	clearCdbCache();
	ACSAlarmSystemInterfaceFactory::init(myMockMgr);
	auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState("Family", "Member", 0);
	CPPUNIT_ASSERT_MESSAGE("Error creating a FS", (fltstate.get() != NULL));
	ACSAlarmSystemInterfaceFactory::done();
}

/**
 * Test the creation of a source (proxy)
 *
 * @throws Exception
 */
void FactoryTestCase::testAlarmSourceCreation()
{
	configureAlarmBranch(cwd, "ACS");
	clearCdbCache();
	ACSAlarmSystemInterfaceFactory::init(myMockMgr);
	AlarmSystemInterface* alarmSource = ACSAlarmSystemInterfaceFactory::createSource();
	CPPUNIT_ASSERT_MESSAGE("Error creating an alarm source", (alarmSource != NULL));
	ACSAlarmSystemInterfaceFactory::done();
}

/**
 * Remove the Alarm branch from the CDB
 *
 * @param CDBFolder The directory of the CDB
 */
void FactoryTestCase::renameAlarmBranch(string CDBFolder)
{
	string CDBdir = CDBFolder + "/CDB/Alarms";
	string CDBbackupdir = CDBFolder + "/CDB/Alarms.bak";

	int result = std::rename(CDBdir.c_str(), CDBbackupdir.c_str());
	if (result != 0 ) {
		ACS_SHORT_LOG((LM_ERROR,"Error renaming CDB/Alarms directory"));
	}
}

/**
 * Replace the Alarm branch from the CDB
 *
 * @param CDBFolder The directory of the CDB
 */
void FactoryTestCase::replaceAlarmBranch(string CDBFolder)
{
	string CDBdir = CDBFolder + "/CDB/Alarms";
	string CDBbackupdir = CDBFolder + "/CDB/Alarms.bak";

	int result = std::rename(CDBbackupdir.c_str(), CDBdir.c_str());
	if (result != 0 ) {
		ACS_SHORT_LOG((LM_ERROR,"Error renaming CDB/Alarms.bak directory"));
	}
}

/**
 * Rewrite the Alarm branch of the CDB.
 *
 * @param CDBFolder The directory of the CDB
 * @param ASImplementation The value of the implementation property of the CDB
 */
void FactoryTestCase::configureAlarmBranch(string CDBFolder, string ASImplementation)
{
	string CDBdir = CDBFolder + "/CDB/Alarms";
	string configDirName = CDBdir + "/Administrative/AlarmSystemConfiguration";
	string configFileName = configDirName + "/AlarmSystemConfiguration.xml";
	try
	{
		filebuf buffer;
		ostream output(&buffer);
		buffer.open(configFileName.c_str(), ios::out);
		output << XML_HEADER_LINE_ONE;
		output << XML_HEADER_LINE_TWO;
		output << XML_HEADER_LINE_THREE;
		output << XML_HEADER_LINE_FOUR;
		output << ASImplementation;
		output << XML_TAIL;
	}
	catch (...)
	{
		ACS_SHORT_LOG((LM_ERROR, "Error setting up the Alarm branch of the CDB:"));
	}
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


