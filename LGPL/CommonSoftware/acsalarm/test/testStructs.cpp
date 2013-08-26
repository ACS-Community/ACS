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
* "@(#) $Id: testStructs.cpp,v 1.1 2012/04/05 13:18:16 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2012-04-04  created 
*/

/**
 * Test alarm message structs.
 *
 * This test is derived from testUnitDriver.cpp and testUnitDriver2.cpp without
 * XML testing. XML is in fact generated in the CERN implementation and tested there.
 */

static char *rcsId="@(#) $Id: testStructs.cpp,v 1.1 2012/04/05 13:18:16 acaproni Exp $"; 

#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

#include <FaultState.h>
#include <ASIMessage.h>
#include <ACSAlarmSystemInterfaceFactory.h>
#include <faultStateConstants.h>
#include <logging.h>
#include <loggingGenericLogger.h>

// constants we will use when creating the fault state
#define FAMILY_VALUE "AlarmSource"
#define MEMBER_VALUE "ALARM_SOURCE_MOUNT"
#define DESCRIPTOR_VALUE "TestDescriptor"
#define PREFIX_VALUE_VALUE "prefixValue"
#define SUFFIX_VALUE_VALUE "suffixValue"
#define TEST_NAME_VALUE "testProperty"
#define TEST_VALUE_VALUE "testValue"
#define CODE_VALUE 1
#define SECONDS_VALUE 9999
#define MICROSECONDS_VALUE 8888
#define DUMMY_HOSTNAME "DummyHostname"

class AcsAlarmTestCase : public CPPUNIT_NS::TestFixture
{
    CPPUNIT_TEST_SUITE(AcsAlarmTestCase);
    CPPUNIT_TEST(testFaultState);
    CPPUNIT_TEST(testProps);
    CPPUNIT_TEST(testTimestamp);
    CPPUNIT_TEST(testASIMessageFaultStateNotPopulated);
	CPPUNIT_TEST(testASIMessageFaultStatePopulated);
    CPPUNIT_TEST_SUITE_END();

  protected:
    void testFaultState();
    void testProps();
    void testTimestamp();
    void testASIMessageFaultStateNotPopulated();
	void testASIMessageFaultStatePopulated();

  private:
    void commonTestASIMessage(std::auto_ptr<std::vector<acsalarm::FaultState> > fltstates, bool fullyPopulated);
};

void AcsAlarmTestCase::testTimestamp()
{
	acsalarm::Timestamp timestamp;

	// test setSeconds & getSeconds
	timestamp.setSeconds(100);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::getSeconds/setSeconds appears to be broken", (100 == timestamp.getSeconds()) );

	// test setMicroSeconds & getMicroSeconds
	timestamp.setMicroSeconds(1000);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::getMicroSeconds/setMicroSeconds appears to be broken", (1000 == timestamp.getMicroSeconds()) );

	// test constructor(secs, microsecs)
	acsalarm::Timestamp timestamp2(100, 1000);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::Timestamp(secs, microsecs) appears to be broken", (100 == timestamp.getSeconds()) );
	CPPUNIT_ASSERT_MESSAGE("Timestamp::Timestamp(secs, microsecs) appears to be broken", (1000 == timestamp.getMicroSeconds()) );

	// test == operator
	CPPUNIT_ASSERT_MESSAGE("Timestamp:: == operator appears to be broken", (timestamp == timestamp2) );

	// test = operator
	acsalarm::Timestamp timestamp3 = timestamp2;
	CPPUNIT_ASSERT_MESSAGE("Timestamp:: = operator appears to be broken", (timestamp3 == timestamp2) );

	// test copy constructor
	acsalarm::Timestamp timestamp4(timestamp3);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::Timestamp(timestamp) - copy constructor appears to be broken", (timestamp3 == timestamp4) );

}

void AcsAlarmTestCase::testProps()
{
	acsalarm::Properties properties;

	// test getProperty and setProperty methods
	std::string key("key");
	std::string value("value");
	properties.setProperty(key, value);
	CPPUNIT_ASSERT_MESSAGE("Properties::getProperty/setProperty appears to be broken", (value == properties.getProperty(key)) );

	// test propertyNames method
	std::string key2("key2");
	std::string value2("value2");
	properties.setProperty(key2, value2);
	properties.setProperty(key2, value2);
	std::auto_ptr< std::vector<std::string> > keys = properties.propertyNames();
	CPPUNIT_ASSERT_MESSAGE("Properties::propertyNames appears to be broken", (keys->size() == 2) );
	for(unsigned int i = 0; i < keys->size(); i++)
	{
		CPPUNIT_ASSERT_MESSAGE("Properties::propertyNames appears to be broken", (keys->at(i) == key || keys->at(i) == key2) );
	}

	// test copy constructor
	acsalarm::Properties properties2(properties);
	CPPUNIT_ASSERT_MESSAGE("Properties::Properties(&properties) - copy constructor appears to be broken", (value == properties2.getProperty(key)) );

	std::auto_ptr< std::vector<std::string> > keys2 = properties2.propertyNames();
	CPPUNIT_ASSERT_MESSAGE("Properties::Properties(&properties) - copy constructor appears to be broken", (keys2->size() == 2) );
	for(unsigned int i = 0; i < keys2->size(); i++)
	{
		CPPUNIT_ASSERT_MESSAGE("Properties::(&properties) - copy constructor appears to be broken", (keys2->at(i) == key || keys2->at(i) == key2) );
	}

	// test == operator
	CPPUNIT_ASSERT_MESSAGE("Properties:: == operator appears to be broken", (properties == properties2) );

	// test = operator
	acsalarm::Properties properties3 = properties2;
	CPPUNIT_ASSERT_MESSAGE("Properties:: = operator appears to be broken", (value == properties3.getProperty(key)) );

	std::auto_ptr< std::vector<std::string> > keys3 = properties3.propertyNames();
	CPPUNIT_ASSERT_MESSAGE("Properties:: = operator appears to be broken", (keys3->size() == 2) );
	for(unsigned int i = 0; i < keys3->size(); i++)
	{
		CPPUNIT_ASSERT_MESSAGE("Properties:: = operator appears to be broken", (keys3->at(i) == key || keys3->at(i) == key2) );
	}

}

void AcsAlarmTestCase::testFaultState()
{
	const std::string member(MEMBER_VALUE);
	const std::string family(FAMILY_VALUE);
	const std::string descriptor(DESCRIPTOR_VALUE);

	// create the FaultState
	std::auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(family, member, CODE_VALUE);

	// test family getters
	CPPUNIT_ASSERT_MESSAGE("FaultState::getFamily appears to be broken", (family == fltstate->getFamily()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::getMember appears to be broken", (member == fltstate->getMember()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::getCode appears to be broken", (CODE_VALUE == fltstate->getCode()) );

	// test family setter
	std::string newfamily = "newfamily";
	fltstate->setFamily(newfamily);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setFamily appears to be broken", (newfamily == fltstate->getFamily()) );

	// restore previous value
	fltstate->setFamily(family);

	// test member setter
	std::string newmember = "newmember";
	fltstate->setMember(newmember);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setMember appears to be broken", (newmember == fltstate->getMember()) );

	// restore previous value
	fltstate->setMember(member);

	// test code setter
	int newcode = 2;
	fltstate->setCode(newcode);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setCode appears to be broken", (newcode == fltstate->getCode()) );

	// restore previous value
	fltstate->setCode(CODE_VALUE);

	// test descriptor setter
	fltstate->setDescriptor(descriptor);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setDescriptor appears to be broken", (descriptor == fltstate->getDescriptor()) );

	// test timestamp getters/setters
	acsalarm::Timestamp * tstampPtr = new acsalarm::Timestamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	std::auto_ptr<acsalarm::Timestamp> tstampAutoPtr(tstampPtr);
	fltstate->setUserTimestamp(tstampAutoPtr);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setUserTimestamp appears to be broken", (*tstampPtr == fltstate->getUserTimestamp()) );

	// test properties getters/setters
	acsalarm::Properties * propsPtr = new acsalarm::Properties();
	propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, PREFIX_VALUE_VALUE);
	propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, SUFFIX_VALUE_VALUE);
	propsPtr->setProperty(TEST_NAME_VALUE, TEST_VALUE_VALUE);
	std::auto_ptr<acsalarm::Properties> propsAutoPtr(propsPtr);
	fltstate->setUserProperties(propsAutoPtr);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setUserProperties appears to be broken", (*propsPtr == fltstate->getUserProperties()) );

	// test activated by backup getters/setters
	bool activatedByBackup = true;
	fltstate->setActivatedByBackup(activatedByBackup);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setActivatedByBackup appears to be broken", (activatedByBackup == fltstate->getActivatedByBackup()) );
	activatedByBackup = false;
	fltstate->setActivatedByBackup(activatedByBackup);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setActivatedByBackup appears to be broken", (activatedByBackup == fltstate->getActivatedByBackup()) );

	// test terminated by backup getters/setters
	bool terminatedByBackup = true;
	fltstate->setTerminatedByBackup(terminatedByBackup);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setTerminatedByBackup appears to be broken", (terminatedByBackup == fltstate->getTerminatedByBackup()) );
	terminatedByBackup = false;
	fltstate->setTerminatedByBackup(terminatedByBackup);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setTerminatedByBackup appears to be broken", (terminatedByBackup == fltstate->getTerminatedByBackup()) );

	// test assignment operator
	acsalarm::FaultState assignedFaultState = *fltstate;
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getFamily",
		(assignedFaultState.getFamily() == fltstate->getFamily()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getMember",
		(assignedFaultState.getMember() == fltstate->getMember()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getCode",
		(assignedFaultState.getCode() == fltstate->getCode()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getDescriptor",
		(assignedFaultState.getDescriptor() == fltstate->getDescriptor()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getTerminatedByBackup",
		(assignedFaultState.getTerminatedByBackup() == fltstate->getTerminatedByBackup()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getActivatedByBackup",
		(assignedFaultState.getActivatedByBackup() == fltstate->getActivatedByBackup()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getUserTimestamp",
		(assignedFaultState.getUserTimestamp() == fltstate->getUserTimestamp()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::= (assignment operator) appears to be broken; getUserProperties",
		(assignedFaultState.getUserProperties() == fltstate->getUserProperties()) );
}

void AcsAlarmTestCase::commonTestASIMessage(std::auto_ptr<std::vector<acsalarm::FaultState> > statesAutoPtr, bool fullyPopulated)
{
	// create the ASIMessage
	acsalarm::ASIMessage asiMessage(statesAutoPtr);

	// populate the ASIMessage's source timestamp (with the current time)
	acsalarm::Timestamp * timestampPtr = new acsalarm::Timestamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	std::auto_ptr<acsalarm::Timestamp> timestampAutoPtr(timestampPtr);
	asiMessage.setSourceTimestamp(timestampAutoPtr);

	// populate the ASIMessage's source name
	asiMessage.setSourceName(asiConfigurationConstants::ALARM_SOURCE_NAME);

	// populate the ASIMessage's source hostname
	asiMessage.setSourceHostname(DUMMY_HOSTNAME);

	// set the ASIMessage's version
	std::string version(asiConfigurationConstants::ASI_VERSION);
	asiMessage.setVersion(version);

	// test backup setter/getter
	asiMessage.setBackup(true);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::setBackup appears to be broken", (true == asiMessage.getBackup()) );
	asiMessage.setBackup(false);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::setBackup appears to be broken", (false == asiMessage.getBackup()) );

	// test source name setter/getter
	asiMessage.setSourceName("dummyString");
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::setSourceName appears to be broken", ("dummyString" == asiMessage.getSourceName()) );
	asiMessage.setSourceName(asiConfigurationConstants::ALARM_SOURCE_NAME);

	// test source timestamp setter/getter
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::setSourceTimestamp appears to be broken", (*timestampPtr == asiMessage.getSourceTimestamp()) );

	// test version setter/getter
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::setVersion appears to be broken", (std::string(asiConfigurationConstants::ASI_VERSION) == asiMessage.getVersion()) );
}

void AcsAlarmTestCase::testASIMessageFaultStateNotPopulated()
{
	const std::string member(MEMBER_VALUE);
	const std::string family(FAMILY_VALUE);
	const std::string descriptor(DESCRIPTOR_VALUE);

	// create the FaultState but do NOT explicitly populate the properties or timestamp of it
	// in this test case; testASIMessageFaultStatePopulated will test that variation
	acsalarm::FaultState fltstate(family, member, CODE_VALUE);

	// set descriptor
	fltstate.setDescriptor(descriptor);

	// TODO: test multiple FaultState objects in the vector?
	// add the FaultState to a vector
	std::vector<acsalarm::FaultState>* statesPtr = new std::vector<acsalarm::FaultState>();
	statesPtr->push_back(fltstate);
	std::auto_ptr<std::vector<acsalarm::FaultState> > statesAutoPtr(statesPtr);

	commonTestASIMessage(statesAutoPtr, false);
}

void AcsAlarmTestCase::testASIMessageFaultStatePopulated()
{

	const std::string member(MEMBER_VALUE);
	const std::string family(FAMILY_VALUE);
	const std::string descriptor(DESCRIPTOR_VALUE);

	// create the FaultState
	acsalarm::FaultState fltstate(family, member, CODE_VALUE);

	// set descriptor
	fltstate.setDescriptor(descriptor);

	// FULLY populate the FaultState for this test case...

	// create a Timestamp and use it to configure the FaultState
	acsalarm::Timestamp * tstampPtr = new acsalarm::Timestamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	std::auto_ptr<acsalarm::Timestamp> tstampAutoPtr(tstampPtr);
	fltstate.setUserTimestamp(tstampAutoPtr);

	// create a Properties object and configure it, then assign to the FaultState
	acsalarm::Properties * propsPtr = new acsalarm::Properties();
	propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, PREFIX_VALUE_VALUE);
	propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, SUFFIX_VALUE_VALUE);
	propsPtr->setProperty(TEST_NAME_VALUE, TEST_VALUE_VALUE);
	std::auto_ptr<acsalarm::Properties> propsAutoPtr(propsPtr);
	fltstate.setUserProperties(propsAutoPtr);

	// TODO: test multiple FaultState objects in the vector?
	// add the FaultState to a vector
	std::vector<acsalarm::FaultState>* statesPtr = new std::vector<acsalarm::FaultState>();
	statesPtr->push_back(fltstate);
	std::auto_ptr<std::vector<acsalarm::FaultState> > statesAutoPtr(statesPtr);

	commonTestASIMessage(statesAutoPtr, true);
}

CPPUNIT_TEST_SUITE_REGISTRATION(AcsAlarmTestCase);

int main(int argc, char *argv[])
{
	LoggingProxy::ProcessName("AlarmSourceTestCase");
	LoggingProxy *m_logger = new LoggingProxy (0, 0, 31, 0);
	LoggingProxy::init (m_logger);

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
	std::cout.flush();
	CPPUNIT_NS::CompilerOutputter outputter( &result, std::cerr );
	outputter.write();

	// close the AlarmSystemInterfaceFactory
	ACSAlarmSystemInterfaceFactory::done();

	m_logger->flush();
	LoggingProxy::done();
	delete m_logger;

	return result.wasSuccessful() ? 0 : 1;
}
