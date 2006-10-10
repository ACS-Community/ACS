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
#include <maciSimpleClient.h>

#define XML_HEADER_LENGTH 4
#define XML_HEADER_LINE_ONE "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
#define XML_HEADER_LINE_TWO "<alarm-system-configuration xmlns=\"urn:schemas-cosylab-com:AcsAlarmSystem:1.0\" \n\t\t"
#define XML_HEADER_LINE_THREE "xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> \n"
#define XML_HEADER_LINE_FOUR "\t<configuration-property name=\"Implementation\">"
#define XML_TAIL "</configuration-property>\n</alarm-system-configuration>\n"

using namespace std;

/*
 * NOTE: this test case only tests the following scenario:
 *
 * 1) CERN is specified as the implementation mechanism in the CDB/Alarms/AlarmSystemConfiguration/AlarmSystemConfiguration.xml file.
 *
 * All other scenarios are tested in the acsalarm/test module.
 */
class FactoryTestCase : public CPPUNIT_NS::TestFixture
{
	CPPUNIT_TEST_SUITE(FactoryTestCase);
	CPPUNIT_TEST(testCERNAlarmSystem);
	CPPUNIT_TEST_SUITE_END();

	public:
		FactoryTestCase();
		~FactoryTestCase();
		void setUp();
		void tearDown();
		
	protected:
		void testCERNAlarmSystem();

	private:
		char cwd[1024];
		void configureAlarmBranch(string cdbdir, string alarmSystemImplementation);
		void clearCdbCache();
};

FactoryTestCase::FactoryTestCase()
{
	ACS_TRACE("FactoryTestCase::FactoryTestCase entering");

	if (getcwd(cwd,sizeof(cwd)) == (char *) NULL) {
		ACS_SHORT_LOG((LM_ERROR, "FactoryTestCase::FactoryTestCase() - Failed to get current working dir"));
	}

	ACS_TRACE("FactoryTestCase::FactoryTestCase exiting");
}

FactoryTestCase::~FactoryTestCase()
{
}

void FactoryTestCase::setUp()
{
}

void FactoryTestCase::tearDown()
{
	configureAlarmBranch(cwd,"CERN");
	ACSAlarmSystemInterfaceFactory::done();
}

void FactoryTestCase::clearCdbCache()
{
	system("cdbjDALClearCache");
}

/**
 * Check if the CERN implementation of the AS is choosen when
 * there CERN is in the CDB
 * 
 * @throws Exception
 */
void FactoryTestCase::testCERNAlarmSystem() 
{
	// Create and initialize a SimpleClient object
	maci::SimpleClient client;
	char * args[1];
	args[0] = "FactoryTestCaseProcessName";
	CPPUNIT_ASSERT_MESSAGE("Couldn't initialize the simple client", (client.init(1, args) != 0));

	// Must log into manager before we can really do anything
	client.login();

	configureAlarmBranch(cwd,"CERN");
	clearCdbCache();
	ACSAlarmSystemInterfaceFactory::init(client.manager());
	CPPUNIT_ASSERT_MESSAGE("Wrong implementation in use (CERN case)", !ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem());
	client.logout();
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
	string configDirName = CDBdir + "/AlarmSystemConfiguration";
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

	// initialize the AlarmSystemInterfaceFactory 
	ACSAlarmSystemInterfaceFactory::init(NULL);

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

	// close the AlarmSystemInterfaceFactory 
	ACSAlarmSystemInterfaceFactory::done();

	return result.wasSuccessful() ? 0 : 1;
}
