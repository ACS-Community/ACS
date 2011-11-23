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
#include <FaultState.h>
#include <faultStateConstants.h>
#include <asiConfigurationConstants.h>
#include <ASIMessage.h>

#include <logging.h>

#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

#include <acsalarmDOMErrorHandler.h>
#include <acsalarmStrX.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/Wrapper4InputSource.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLDouble.hpp>
#include <xercesc/dom/DOMBuilder.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <MockManager.h>

// constants we will use when creating the fault
#define DUMMY_PROCESS_NAME "DummyClientProcess"
#define DUMMY_HOSTNAME "DummyHostname"
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

#define FAULT_STATE_ELEMENT_NAME "fault-state"
#define ASI_MESSAGE_ELEMENT_NAME "ASI-message"
#define DESCRIPTOR_ELEMENT_NAME "descriptor"

#define USER_PROPERTIES_ELEMENT_NAME "user-properties"
#define USER_TIMESTAMP_ELEMENT_NAME "user-timestamp"
#define PROPERTY_ELEMENT_NAME "property"
#define FAMILY_ATTRIBUTE_NAME "family"
#define MEMBER_ATTRIBUTE_NAME "member"
#define CODE_ATTRIBUTE_NAME "code"
#define NAME_ATTRIBUTE_NAME "name"
#define VALUE_ATTRIBUTE_NAME "value"
#define SECONDS_ATTRIBUTE_NAME "seconds"
#define MICROSECONDS_ATTRIBUTE_NAME "microseconds"
#define XMLNS_ATTRIBUTE_NAME "xmlns:xsi"
#define XMLNS_VALUE "http://www.w3.org/2001/XMLSchema-instance"
#define BACKUP_ATTRIBUTE_NAME "backup"
#define BACKUP_VALUE "false"
#define VERSION_ATTRIBUTE_NAME "version"
#define VERSION_VALUE asiConfigurationConstants::ASI_VERSION
#define TYPE_ATTRIBUTE_NAME "xsi:type"
#define TYPE_VALUE "ASI-message"
#define SOURCE_NAME_NAME "source-name"
#define SOURCE_NAME_VALUE asiConfigurationConstants::ALARM_SOURCE_NAME
#define SOURCE_HOST_NAME_NAME "source-hostname"
#define SOURCE_TIMESTAMP_NAME "source-timestamp"
#define FAULT_STATES_NAME "fault-states"

#ifndef MEMPARSE_ENCODING
   #if defined(OS390)
      #define MEMPARSE_ENCODING "ibm-1047-s390"
   #elif defined(OS400)
      #define MEMPARSE_ENCODING "ibm037"
   #else
      #define MEMPARSE_ENCODING "ascii"
   #endif
#endif 
 
XERCES_CPP_NAMESPACE_USE
using acsalarm::acsDOMErrorHandler;
using acsalarm::StrX;
using acsalarm::FaultState;
using acsalarm::ASIMessage;
using acsalarm::Properties;
using acsalarm::Timestamp;
using std::auto_ptr;
using std::vector;
using std::string;

class AcsAlarmTestCase : public CPPUNIT_NS::TestFixture
{
    CPPUNIT_TEST_SUITE(AcsAlarmTestCase);
    CPPUNIT_TEST(testASIMessageFaultStateNotPopulated);
    CPPUNIT_TEST(testASIMessageFaultStatePopulated);
    CPPUNIT_TEST_SUITE_END();

  public:
		AcsAlarmTestCase();
		~AcsAlarmTestCase();
		void setUp();
		void tearDown();

  protected:
    void testASIMessageFaultStateNotPopulated();
    void testASIMessageFaultStatePopulated();

	private:
		void verifyFaultStateElement(DOMDocument * doc, bool);
		void verifyFaultStateDescriptorElement(DOMDocument * doc);
		void verifyFaultStateUserPropertiesElement(DOMDocument * doc);
		void verifyFaultStateUserTimestampElement(DOMDocument * doc);

		void verifyASIMessageXML(string, bool);
		void verifyASIMessageElement(DOMDocument * doc);
		void verifyASIMessageSourceNameElement(DOMDocument * doc);
		void verifyASIMessageSourceHostnameElement(DOMDocument * doc);
		void verifyASIMessageSourceTimestampElement(DOMDocument * doc);
		void verifyASIMessageFaultStatesElement(DOMDocument * doc);
		void commonTestASIMessage(auto_ptr<vector<FaultState> > fltstates, bool fullyPopulated);

		DOMDocument *parseDOM(string);

		DOMBuilder  *parser;
		acsDOMErrorHandler* errHandler;

		// Constants
		XMLCh* ASI_MESSAGE_TAG_NAME;
		XMLCh* FAULT_STATE_TAG_NAME;
		XMLCh* DESCRIPTOR_TAG_NAME;
		XMLCh* USER_PROPERTIES_TAG_NAME;
		XMLCh* USER_TIMESTAMP_TAG_NAME;
		XMLCh* PROPERTY_TAG_NAME;
		XMLCh* FAMILY_TAG_NAME;
		XMLCh* MEMBER_TAG_NAME;
		XMLCh* CODE_TAG_NAME;
		XMLCh* NAME_TAG_NAME;
		XMLCh* VALUE_TAG_NAME;
		XMLCh* SECONDS_TAG_NAME;
		XMLCh* MICROSECONDS_TAG_NAME;
		XMLCh* FAMILY_VALUE_XMLCH;
		XMLCh* MEMBER_VALUE_XMLCH;
		XMLCh* DESCRIPTOR_VALUE_XMLCH;
		XMLCh* TEST_NAME_VALUE_XMLCH;
		XMLCh* PREFIX_NAME_VALUE_XMLCH;
		XMLCh* SUFFIX_NAME_VALUE_XMLCH;
		XMLCh* TEST_VALUE_VALUE_XMLCH;
		XMLCh* PREFIX_VALUE_VALUE_XMLCH;
		XMLCh* SUFFIX_VALUE_VALUE_XMLCH;
		XMLCh* XMLNS_TAG_NAME;
		XMLCh* XMLNS_VALUE_XMLCH;
		XMLCh* BACKUP_TAG_NAME;
		XMLCh* BACKUP_VALUE_XMLCH;
		XMLCh* VERSION_TAG_NAME;
		XMLCh* VERSION_VALUE_XMLCH;
		XMLCh* TYPE_TAG_NAME;
		XMLCh* TYPE_VALUE_XMLCH;
		XMLCh* SOURCE_NAME_TAG_NAME;
		XMLCh* SOURCE_NAME_VALUE_XMLCH;
		XMLCh* SOURCE_HOST_NAME_TAG_NAME;
		XMLCh* SOURCE_HOST_NAME_VALUE_XMLCH;
		XMLCh* SOURCE_TIMESTAMP_TAG_NAME;
		XMLCh* FAULT_STATES_TAG_NAME;
};

//------------------------

AcsAlarmTestCase::AcsAlarmTestCase()
{
}

AcsAlarmTestCase::~AcsAlarmTestCase()
{
}

void AcsAlarmTestCase::setUp()
{
	XMLPlatformUtils::Initialize();

	bool doNamespaces = false;
	bool doSchema = false;
	bool schemaFullChecking = true;

	// Instantiate the DOM parser.
	static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
	DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(gLS);
	parser = ((DOMImplementationLS*)impl)->createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS, 0);

	// configure the DOM parser
	parser->setFeature(XMLUni::fgDOMNamespaces, doNamespaces);
	parser->setFeature(XMLUni::fgXercesSchema, doSchema);
	parser->setFeature(XMLUni::fgXercesSchemaFullChecking, schemaFullChecking);
	parser->setFeature(XMLUni::fgDOMValidation, false);
	parser->setFeature(XMLUni::fgXercesDOMHasPSVIInfo, true);
	parser->setFeature(XMLUni::fgDOMDatatypeNormalization, true);

	// Set the error handler to an instance of acsDOMErrorHandler
	errHandler = new acsDOMErrorHandler();
	parser->setErrorHandler(errHandler);

	FAULT_STATE_TAG_NAME = 	XMLString::transcode(FAULT_STATE_ELEMENT_NAME);
	ASI_MESSAGE_TAG_NAME = 	XMLString::transcode(ASI_MESSAGE_ELEMENT_NAME);
	DESCRIPTOR_TAG_NAME = XMLString::transcode(DESCRIPTOR_ELEMENT_NAME);
	USER_PROPERTIES_TAG_NAME = XMLString::transcode(USER_PROPERTIES_ELEMENT_NAME);
	USER_TIMESTAMP_TAG_NAME = XMLString::transcode(USER_TIMESTAMP_ELEMENT_NAME);
	PROPERTY_TAG_NAME = XMLString::transcode(PROPERTY_ELEMENT_NAME);
	FAMILY_TAG_NAME = XMLString::transcode(FAMILY_ATTRIBUTE_NAME);
	MEMBER_TAG_NAME = XMLString::transcode(MEMBER_ATTRIBUTE_NAME);
	CODE_TAG_NAME = XMLString::transcode(CODE_ATTRIBUTE_NAME);
	NAME_TAG_NAME = XMLString::transcode(NAME_ATTRIBUTE_NAME);
	VALUE_TAG_NAME = XMLString::transcode(VALUE_ATTRIBUTE_NAME);
	SECONDS_TAG_NAME = XMLString::transcode(SECONDS_ATTRIBUTE_NAME);
	MICROSECONDS_TAG_NAME = XMLString::transcode(MICROSECONDS_ATTRIBUTE_NAME);
	FAMILY_VALUE_XMLCH = XMLString::transcode(FAMILY_VALUE);
	MEMBER_VALUE_XMLCH = XMLString::transcode(MEMBER_VALUE);
	DESCRIPTOR_VALUE_XMLCH = XMLString::transcode(DESCRIPTOR_VALUE);
	TEST_NAME_VALUE_XMLCH = XMLString::transcode(TEST_NAME_VALUE);
	PREFIX_NAME_VALUE_XMLCH = XMLString::transcode(faultState::ASI_PREFIX_PROPERTY_STRING);
	SUFFIX_NAME_VALUE_XMLCH = XMLString::transcode(faultState::ASI_SUFFIX_PROPERTY_STRING);
	TEST_VALUE_VALUE_XMLCH = XMLString::transcode(TEST_VALUE_VALUE);
	PREFIX_VALUE_VALUE_XMLCH = XMLString::transcode(PREFIX_VALUE_VALUE);
	SUFFIX_VALUE_VALUE_XMLCH = XMLString::transcode(SUFFIX_VALUE_VALUE);
	XMLNS_TAG_NAME = 	XMLString::transcode(XMLNS_ATTRIBUTE_NAME);
	XMLNS_VALUE_XMLCH = XMLString::transcode(XMLNS_VALUE);
	BACKUP_TAG_NAME = 	XMLString::transcode(BACKUP_ATTRIBUTE_NAME);
	BACKUP_VALUE_XMLCH = XMLString::transcode(BACKUP_VALUE);
	VERSION_TAG_NAME = 	XMLString::transcode(VERSION_ATTRIBUTE_NAME);
	VERSION_VALUE_XMLCH = XMLString::transcode(VERSION_VALUE);
	TYPE_TAG_NAME = 	XMLString::transcode(TYPE_ATTRIBUTE_NAME);
	TYPE_VALUE_XMLCH = XMLString::transcode(TYPE_VALUE);
	SOURCE_NAME_TAG_NAME = 	XMLString::transcode(SOURCE_NAME_NAME);
	SOURCE_NAME_VALUE_XMLCH = XMLString::transcode(SOURCE_NAME_VALUE);
	SOURCE_HOST_NAME_TAG_NAME = 	XMLString::transcode(SOURCE_HOST_NAME_NAME);
	SOURCE_HOST_NAME_VALUE_XMLCH = XMLString::transcode(DUMMY_HOSTNAME);
	SOURCE_TIMESTAMP_TAG_NAME = 	XMLString::transcode(SOURCE_TIMESTAMP_NAME);
	FAULT_STATES_TAG_NAME = 	XMLString::transcode(FAULT_STATES_NAME);
}

void AcsAlarmTestCase::tearDown()
{
	XMLString::release(&FAULT_STATE_TAG_NAME);
	XMLString::release(&ASI_MESSAGE_TAG_NAME);
	XMLString::release(&DESCRIPTOR_TAG_NAME);
	XMLString::release(&USER_PROPERTIES_TAG_NAME);
	XMLString::release(&USER_TIMESTAMP_TAG_NAME);
	XMLString::release(&PROPERTY_TAG_NAME);
	XMLString::release(&FAMILY_TAG_NAME);
	XMLString::release(&MEMBER_TAG_NAME);
	XMLString::release(&CODE_TAG_NAME);
	XMLString::release(&NAME_TAG_NAME);
	XMLString::release(&VALUE_TAG_NAME);
	XMLString::release(&SECONDS_TAG_NAME);
	XMLString::release(&MICROSECONDS_TAG_NAME);
	XMLString::release(&FAMILY_VALUE_XMLCH);
	XMLString::release(&MEMBER_VALUE_XMLCH);
	XMLString::release(&DESCRIPTOR_VALUE_XMLCH);
	XMLString::release(&TEST_NAME_VALUE_XMLCH);
	XMLString::release(&PREFIX_NAME_VALUE_XMLCH);
	XMLString::release(&SUFFIX_NAME_VALUE_XMLCH);
	XMLString::release(&TEST_VALUE_VALUE_XMLCH);
	XMLString::release(&PREFIX_VALUE_VALUE_XMLCH);
	XMLString::release(&SUFFIX_VALUE_VALUE_XMLCH);
	XMLString::release(&XMLNS_TAG_NAME);
	XMLString::release(&XMLNS_VALUE_XMLCH);
	XMLString::release(&BACKUP_TAG_NAME);
	XMLString::release(&BACKUP_VALUE_XMLCH);
	XMLString::release(&VERSION_TAG_NAME);
	XMLString::release(&VERSION_VALUE_XMLCH);
	XMLString::release(&TYPE_TAG_NAME);
	XMLString::release(&TYPE_VALUE_XMLCH);
	XMLString::release(&SOURCE_NAME_TAG_NAME);
	XMLString::release(&SOURCE_NAME_VALUE_XMLCH);
	XMLString::release(&SOURCE_HOST_NAME_TAG_NAME);
	XMLString::release(&SOURCE_HOST_NAME_VALUE_XMLCH);
	XMLString::release(&SOURCE_TIMESTAMP_TAG_NAME);
	XMLString::release(&FAULT_STATES_TAG_NAME);

	parser->release();
	delete errHandler;
	XMLPlatformUtils::Terminate();
}

/*
 * XML fragment something like this:
 *
 * <ASI-message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" backup="false" version="0.9" xsi:type="ASI-message">
 * 
 * ... etc ... 
 *
 * </ASI-message>
 */
void AcsAlarmTestCase::verifyASIMessageElement(DOMDocument * doc) 
{
	// Verify that the ASI-message element exists
	DOMNodeList * asiMessageNodes = doc->getElementsByTagName(ASI_MESSAGE_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; no ASI-message element found", 
		(NULL != asiMessageNodes && asiMessageNodes->getLength() == 1));

	// verify that there are the expected attributes (family, member, code) on the fault-state element
	DOMNode * asiMessageItem = asiMessageNodes->item(0);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; there is no ASI-message elemement",
		(NULL != asiMessageItem));	

	// verify that there are 4 attributes in total
	DOMNamedNodeMap * attributesMap = asiMessageItem->getAttributes();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain expected number (4) of attributes",
		(NULL!= attributesMap && attributesMap->getLength() == 4));

	// check that the element has a "xmlns" attribute
	DOMNode * xmlnsNode = attributesMap->getNamedItem(XMLNS_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain 'xmlns' attribute",
		(NULL!= xmlnsNode));

	// verify that the value of xmlns attribute is correct
	const XMLCh * xmlnsNodeValue = xmlnsNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; value of 'xmlns' attribute is not correct",
		(NULL != xmlnsNodeValue && XMLString::equals(xmlnsNodeValue, XMLNS_VALUE_XMLCH)));

	// check that the element has a "backup" attribute
	DOMNode * backupNode = attributesMap->getNamedItem(BACKUP_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain 'backup' attribute",
		(NULL!= backupNode));

	// verify that the value of backup attribute is correct
	const XMLCh * backupNodeValue = backupNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; value of 'backup' is not correct",
		(NULL != backupNodeValue && XMLString::equals(backupNodeValue, BACKUP_VALUE_XMLCH)));

	// check that the element has a "version" attribute
	DOMNode * versionNode = attributesMap->getNamedItem(VERSION_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain 'version' attribute",
		(NULL!= versionNode));

	// verify that the value of version attribute is correct
	const XMLCh * versionNodeValue = versionNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; value of 'version' is not correct",
		(NULL != versionNodeValue && XMLString::equals(versionNodeValue, VERSION_VALUE_XMLCH)));

	// check that the element has a "xsi:type" attribute
	DOMNode * typeNode = attributesMap->getNamedItem(TYPE_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain 'xsi:type' attribute",
		(NULL!= typeNode));

	// verify that the value of xsi:type attribute is correct
	const XMLCh * typeNodeValue = typeNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; value of 'xsi:type' is not correct",
		(NULL != typeNodeValue && XMLString::equals(typeNodeValue, TYPE_VALUE_XMLCH)));
}

/*
 *  <source-name>ALARM_SYSTEM_SOURCES</source-name>
 */
void AcsAlarmTestCase::verifyASIMessageSourceNameElement(DOMDocument * doc)
{
	// Verify that the source-name element exists
	DOMNodeList * sourceNameNodes = doc->getElementsByTagName(SOURCE_NAME_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; no source-name sub-element found", 
		(NULL != sourceNameNodes && sourceNameNodes->getLength() == 1));

	// check value of source-name
	DOMNode * sourceNameElementNode = sourceNameNodes->item(0);
	DOMNode * sourceNameTextNode = sourceNameElementNode->getFirstChild();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; source-name value is not present or null",
		(NULL != sourceNameTextNode));

	const XMLCh * sourceNameNodeValue = sourceNameTextNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; value of source-name is not correct",
		(NULL != sourceNameNodeValue && XMLString::equals(sourceNameNodeValue, SOURCE_NAME_VALUE_XMLCH)));
}

/*
 *  <source-hostname>newbie.aoc.nrao.edu</source-hostname>
 */
void AcsAlarmTestCase::verifyASIMessageSourceHostnameElement(DOMDocument * doc) 
{
	// Verify that the source-hostname element exists
	DOMNodeList * sourceHostNameNodes = doc->getElementsByTagName(SOURCE_HOST_NAME_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; no source-hostname sub-element found", 
		(NULL != sourceHostNameNodes && sourceHostNameNodes->getLength() == 1));

	// check value of hostname
	DOMNode * sourceHostNameElementNode = sourceHostNameNodes->item(0);
	DOMNode * sourceHostNameTextNode = sourceHostNameElementNode->getFirstChild();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; source-hostname value is not present or null",
		(NULL != sourceHostNameTextNode));

	const XMLCh * sourceHostNameNodeValue = sourceHostNameTextNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; value of source-hostname is not correct",
		(NULL != sourceHostNameNodeValue && XMLString::equals(sourceHostNameNodeValue, SOURCE_HOST_NAME_VALUE_XMLCH)));

}

/*
 *  <source-timestamp seconds="" microseconds=""/>
 */
void AcsAlarmTestCase::verifyASIMessageSourceTimestampElement(DOMDocument * doc) 
{
	// Verify that the source-timestamp element exists
	DOMNodeList * sourceTimestampNodes = doc->getElementsByTagName(SOURCE_TIMESTAMP_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; no source-timestamp sub-element found", 
		(NULL != sourceTimestampNodes && sourceTimestampNodes->getLength() == 1));

	// verify that there are the expected attributes (seconds, microseconds) on the element
	DOMNode * sourceTimestampItem = sourceTimestampNodes->item(0);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; there is no source-timestamp elemement",
		(NULL != sourceTimestampItem));	

	// verify that there are 2 attributes in total
	DOMNamedNodeMap * attributesMap = sourceTimestampItem->getAttributes();
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain expected number (2) of attributes",
		(NULL!= attributesMap && attributesMap->getLength() == 2));

	// check that the element has a "seconds" attribute
	DOMNode * secondsNode = attributesMap->getNamedItem(SECONDS_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain 'seconds' attribute",
		(NULL!= secondsNode));

	// check for seconds attribute
	DOMNode * secondsValueNode = attributesMap->getNamedItem(MICROSECONDS_TAG_NAME);
	const XMLCh * secondsValue = secondsValueNode->getNodeValue();
	char *secondsCharValue = XMLString::transcode(secondsValue);
	int secondsIntValue = atoi(secondsCharValue);
	XMLString::release(&secondsCharValue);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; user-timestamp element, 'seconds' attribute value is not correct",
		(NULL!= secondsValue && secondsIntValue == MICROSECONDS_VALUE));

	// check that the element has a "microseconds" attribute
	DOMNode * microsecondsNode = attributesMap->getNamedItem(MICROSECONDS_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; does not contain 'microseconds' attribute",
		(NULL!= microsecondsNode));

	// check for microseconds attribute
	DOMNode * microsecondsValueNode = attributesMap->getNamedItem(MICROSECONDS_TAG_NAME);
	const XMLCh * microsecondsValue = microsecondsValueNode->getNodeValue();
	char *microsecondsCharValue = XMLString::transcode(microsecondsValue);
	int microsecondsIntValue = atoi(microsecondsCharValue);
	XMLString::release(&microsecondsCharValue);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; user-timestamp element, 'microseconds' attribute value is not correct",
		(NULL!= microsecondsValue && microsecondsIntValue == MICROSECONDS_VALUE));
}

/*
 * <fault-states> 
 *
 * ... etc ...
 *
 * </fault-states>
 */
void AcsAlarmTestCase::verifyASIMessageFaultStatesElement(DOMDocument * doc) 
{
	// Verify that the fault-states element exists
	DOMNodeList * faultStateNodes = doc->getElementsByTagName(FAULT_STATES_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::toXML appears to be broken; no fault-states sub-element found", 
		(NULL != faultStateNodes && faultStateNodes->getLength() == 1));
}

void AcsAlarmTestCase::commonTestASIMessage(auto_ptr<vector<FaultState> > statesAutoPtr, bool fullyPopulated)
{
	// create the ASIMessage
	ASIMessage asiMessage(statesAutoPtr);

	// populate the ASIMessage's source timestamp (with the current time)
	Timestamp * timestampPtr = new Timestamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	auto_ptr<Timestamp> timestampAutoPtr(timestampPtr);
	asiMessage.setSourceTimestamp(timestampAutoPtr);

	// populate the ASIMessage's source name
	asiMessage.setSourceName(asiConfigurationConstants::ALARM_SOURCE_NAME);

	// populate the ASIMessage's source hostname
	asiMessage.setSourceHostname(DUMMY_HOSTNAME);

	// set the ASIMessage's version
	string version(asiConfigurationConstants::ASI_VERSION);
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
	CPPUNIT_ASSERT_MESSAGE("ASIMessage::setVersion appears to be broken", (string(asiConfigurationConstants::ASI_VERSION) == asiMessage.getVersion()) );

	// test toXML method
	verifyASIMessageXML(asiMessage.toXML(), fullyPopulated);
}

void AcsAlarmTestCase::testASIMessageFaultStateNotPopulated()
{
	const string member(MEMBER_VALUE);
	const string family(FAMILY_VALUE);
	const string descriptor(DESCRIPTOR_VALUE);

	// create the FaultState but do NOT explicitly populate the properties or timestamp of it
	// in this test case; testASIMessageFaultStatePopulated will test that variation
	FaultState fltstate(family, member, CODE_VALUE);

	// set descriptor 
	fltstate.setDescriptor(descriptor);

	// TODO: test multiple FaultState objects in the vector?
	// add the FaultState to a vector
	vector<FaultState>* statesPtr = new vector<FaultState>();
	statesPtr->push_back(fltstate);
	auto_ptr<vector<FaultState> > statesAutoPtr(statesPtr); 

	commonTestASIMessage(statesAutoPtr, false);
}

void AcsAlarmTestCase::testASIMessageFaultStatePopulated()
{

	const string member(MEMBER_VALUE);
	const string family(FAMILY_VALUE);
	const string descriptor(DESCRIPTOR_VALUE);

	// create the FaultState
	FaultState fltstate(family, member, CODE_VALUE);

	// set descriptor 
	fltstate.setDescriptor(descriptor);

	// FULLY populate the FaultState for this test case...

	// create a Timestamp and use it to configure the FaultState
	Timestamp * tstampPtr = new Timestamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	auto_ptr<Timestamp> tstampAutoPtr(tstampPtr);
	fltstate.setUserTimestamp(tstampAutoPtr);

	// create a Properties object and configure it, then assign to the FaultState
	Properties * propsPtr = new Properties();
	propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, PREFIX_VALUE_VALUE);
	propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, SUFFIX_VALUE_VALUE);
	propsPtr->setProperty(TEST_NAME_VALUE, TEST_VALUE_VALUE);
	auto_ptr<Properties> propsAutoPtr(propsPtr);
	fltstate.setUserProperties(propsAutoPtr);

	// TODO: test multiple FaultState objects in the vector?
	// add the FaultState to a vector
	vector<FaultState>* statesPtr = new vector<FaultState>();
	statesPtr->push_back(fltstate);
	auto_ptr<vector<FaultState> > statesAutoPtr(statesPtr); 

	commonTestASIMessage(statesAutoPtr, true);
}

/*******************************************
 * XML looks something like this:
 *
 * <ASI-message xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" backup="false" version="0.9" xsi:type="ASI-message">
 *  <source-name>ALARM_SYSTEM_SOURCES</source-name>
 *  <source-hostname></source-hostname>
 *  <source-timestamp seconds="" seconds=""/>
 *  <fault-states>
 *     <fault-state family="AlarmSource" member="ALARM_SOURCE_MOUNT" code="1">
 *        <descriptor>ACTIVE</descriptor>
 *        <user-properties>
 *           <property name="ASI_PREFIX_PROPERTY" value="prefix"/>
 *           <property name="ASI_SUFFIX_PROPERTY" value="suffix"/>
 *           <property name="TEST_PROPERTY" value="TEST_VALUE"/>
 *        </user-properties>
 *        <user-timestamp seconds="" seconds=""/>
 *     </fault-state>
 *  </fault-states>
 * </ASI-message>
*******************************************/
void AcsAlarmTestCase::verifyASIMessageXML(string xmlData, bool propertiesAndTimestampPopulated)
{
	try
	{
		DOMDocument* doc = parseDOM(xmlData);

		verifyASIMessageElement(doc);
		verifyASIMessageSourceNameElement(doc);
		verifyASIMessageSourceHostnameElement(doc);
		verifyASIMessageSourceTimestampElement(doc);
		verifyASIMessageFaultStatesElement(doc);

		// For each fault state in fault-states element call verifyFaultStateElement (singular) method
		DOMNodeList * faultStateNodes = doc->getElementsByTagName(FAULT_STATE_TAG_NAME);
		if(NULL != faultStateNodes)
		{
			for(XMLSize_t i = 0; i < faultStateNodes->getLength(); i++)
			{
				verifyFaultStateElement(doc, propertiesAndTimestampPopulated);
			}
		}
	}
	catch (const XMLException& toCatch)
	{
		ACS_LOG(LM_ERROR, "FaultState::toXML", (LM_ERROR, 
			"***** XMLException message: ***** \n\n%s \n *****\n", StrX(toCatch.getMessage()).localForm()))
	}
}

/**
 * Private method to orchestrate the XML parsing using DOM.
 * @param xmlData - ptr to an InMemoryXmlData object (OR NULL if we are using a file)
 * NOTE: one of the params, but not both, should be NULL for a given parse - i.e.
 * either we are parsing from a file or parsing from memory, but not both (nor neither - 
 * i.e. NULL for both params should not occur either).
 */
DOMDocument* AcsAlarmTestCase::parseDOM(string xmlData) 
{
	DOMDocument *doc = 0;
	MemBufInputSource *mbis = NULL;
	
	bool caughtException = false;
	try {
		mbis = new  MemBufInputSource((const XMLByte *) xmlData.c_str(), xmlData.length(), "ID", false);
		Wrapper4InputSource wrapper4InputSource(mbis);
		doc = parser->parse(wrapper4InputSource);
	}
	catch (const XMLException& toCatch) {
		char* message = XMLString::transcode(toCatch.getMessage());
		ACS_LOG(LM_ERROR, "AcsAlarmTestCase::parseDOM", (LM_ERROR, 
			"***** XMLException message: ***** \n\n%s \n *****\n", message))
		XMLString::release(&message);
		caughtException = true;
	}
	catch (const DOMException& toCatch) {
		char* message = XMLString::transcode(toCatch.msg);
		ACS_LOG(LM_ERROR, "AcsAlarmTestCase::parseDOM", (LM_ERROR, "***** DOMException message: %s *****", message))
		XMLString::release(&message);
		caughtException = true;
	}

	CPPUNIT_ASSERT_MESSAGE("parseDOM received an exception during parsing", (false == caughtException));

	return doc;
}

void AcsAlarmTestCase::verifyFaultStateElement(DOMDocument * doc, bool propertiesAndTimestampPopulated) 
{
	// Verify that the fault-state element exists
	DOMNodeList * faultStateNodes = doc->getElementsByTagName(FAULT_STATE_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; no fault-state element found", 
		(NULL != faultStateNodes && faultStateNodes->getLength() == 1));

	// verify that there are the expected attributes (family, member, code) on the fault-state element
	DOMNode * faultStateItem = faultStateNodes->item(0);
	if(NULL != faultStateItem) 
	{
		// verify that there are 3 attributes in total
		DOMNamedNodeMap * attributesMap = faultStateItem->getAttributes();
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; fault-state does not contain 3 attributes",
			(NULL!= attributesMap && attributesMap->getLength() == 3));

		// check that the fault-state element has a "family" attribute
		DOMNode * familyNode = attributesMap->getNamedItem(FAMILY_TAG_NAME);
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; fault-state does not contain 'family' attribute",
			(NULL!= familyNode));

		// verify that the value of family attribute is correct
		const XMLCh * familyNodeValue = familyNode->getNodeValue();
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; value of fault-state 'family' is not correct",
			(NULL != familyNodeValue && XMLString::equals(familyNodeValue, FAMILY_VALUE_XMLCH)));

		// check that the fault-state element has a "member" attribute
		DOMNode * memberNode = attributesMap->getNamedItem(MEMBER_TAG_NAME);
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; fault-state does not contain 'member' attribute",
			(NULL!= memberNode));

		// verify that the value of member attribute is correct
		const XMLCh * memberNodeValue = memberNode->getNodeValue();
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; value of fault-state 'member' is not correct",
			(NULL != memberNodeValue && XMLString::equals(memberNodeValue, MEMBER_VALUE_XMLCH)));

		// check that the fault-state element has a "code" attribute
		DOMNode * codeNode = attributesMap->getNamedItem(CODE_TAG_NAME);
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; fault-state does not contain 'code' attribute",
			(NULL!= codeNode));

		// verify that the value of code attribute is correct
		const XMLCh * codeNodeValue = codeNode->getNodeValue();
		char *codeNodeCharValue = XMLString::transcode(codeNodeValue);
		int codeNodeValueInt = atoi(codeNodeCharValue);
		XMLString::release(&codeNodeCharValue);
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; value of fault-state 'code' is not correct",
			(NULL != codeNodeValue && codeNodeValueInt == CODE_VALUE));
	}

	verifyFaultStateDescriptorElement(doc);
	if(propertiesAndTimestampPopulated)
	{
		verifyFaultStateUserPropertiesElement(doc);
		verifyFaultStateUserTimestampElement(doc);
	}
}

void AcsAlarmTestCase::verifyFaultStateDescriptorElement(DOMDocument * doc) 
{
	// Verify the descriptor element 
	DOMNodeList * descriptorNodes = doc->getElementsByTagName(DESCRIPTOR_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; no descriptor element found", 
		(NULL != descriptorNodes && descriptorNodes->getLength() == 1));

	// check value of descriptor
	DOMNode * descriptorElementNode = descriptorNodes->item(0);
	DOMNode * descriptorTextNode = descriptorElementNode->getFirstChild();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; descriptor value is not present or null",
		(NULL != descriptorTextNode));

	const XMLCh * descriptorNodeValue = descriptorTextNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; value of descriptor is not correct",
		(NULL != descriptorNodeValue && XMLString::equals(descriptorNodeValue, DESCRIPTOR_VALUE_XMLCH)));
}

void AcsAlarmTestCase::verifyFaultStateUserPropertiesElement(DOMDocument * doc) 
{
	// Verify the user-properties element 
	DOMNodeList * userPropertiesNodes = doc->getElementsByTagName(USER_PROPERTIES_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; no user-properties element found", 
		(NULL != userPropertiesNodes && userPropertiesNodes->getLength() == 1));

	// check for 3 property sub-element(s)
	DOMNodeList * propertyNodes = doc->getElementsByTagName(PROPERTY_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; did not find 3 property elements", 
		(NULL != propertyNodes && propertyNodes->getLength() == 3));

	// verify for each property element that it has the expected attributes
	for(XMLSize_t i = 0; i < propertyNodes->getLength(); i++)
	{
		DOMNamedNodeMap * attributesMap = propertyNodes->item(i)->getAttributes();
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; property element does not contain 2 attributes",
			(NULL!= attributesMap && attributesMap->getLength() == 2));

		// check that the property element has a "name" attribute
		DOMNode * familyNode = attributesMap->getNamedItem(NAME_TAG_NAME);
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; property element does not contain 'name' attribute",
			(NULL!= familyNode));

		// check that the property element has a "value" attribute
		DOMNode * valueNode = attributesMap->getNamedItem(VALUE_TAG_NAME);
		CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; property element does not contain 'value' attribute",
			(NULL!= valueNode));
	}

	// for each property, check the 'name' attribute
	DOMNamedNodeMap * firstPropAttrMap = propertyNodes->item(0)->getAttributes();
	DOMNode * prefixNameNode = firstPropAttrMap->getNamedItem(NAME_TAG_NAME);
	const XMLCh * prefixNameNodeValue = prefixNameNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; 1st property element, 'name' attribute value is not correct",
		(NULL!= prefixNameNodeValue && XMLString::equals(prefixNameNodeValue, PREFIX_NAME_VALUE_XMLCH)));
		
	DOMNamedNodeMap * secondPropAttrMap = propertyNodes->item(1)->getAttributes();
	DOMNode * suffixNameNode = secondPropAttrMap->getNamedItem(NAME_TAG_NAME);
	const XMLCh * suffixNameNodeValue = suffixNameNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; 2nd property element, 'name' attribute value is not correct",
		(NULL!= suffixNameNodeValue && XMLString::equals(suffixNameNodeValue, SUFFIX_NAME_VALUE_XMLCH)));

	DOMNamedNodeMap * thirdPropAttrMap = propertyNodes->item(2)->getAttributes();
	DOMNode * testPropNameNode = thirdPropAttrMap->getNamedItem(NAME_TAG_NAME);
	const XMLCh * testPropNameNodeValue = testPropNameNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; 3rd property element, 'name' attribute value is not correct",
		(NULL!= testPropNameNodeValue && XMLString::equals(testPropNameNodeValue, TEST_NAME_VALUE_XMLCH)));

	// for each property, check the 'value' attribute
	DOMNamedNodeMap * firstAttrMap = propertyNodes->item(0)->getAttributes();
	DOMNode * prefixValueNode = firstAttrMap->getNamedItem(VALUE_TAG_NAME);
	const XMLCh * prefixValueNodeValue = prefixValueNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; 1st property element, 'value' attribute value is not correct",
		(NULL!= prefixValueNodeValue && XMLString::equals(prefixValueNodeValue, PREFIX_VALUE_VALUE_XMLCH)));
		
	DOMNamedNodeMap * secondAttrMap = propertyNodes->item(1)->getAttributes();
	DOMNode * suffixValueNode = secondAttrMap->getNamedItem(VALUE_TAG_NAME);
	const XMLCh * suffixValueNodeValue = suffixValueNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; 2nd property element, 'value' attribute value is not correct",
		(NULL!= suffixValueNodeValue && XMLString::equals(suffixValueNodeValue, SUFFIX_VALUE_VALUE_XMLCH)));

	DOMNamedNodeMap * thirdAttrMap = propertyNodes->item(2)->getAttributes();
	DOMNode * testValueNode = thirdAttrMap->getNamedItem(VALUE_TAG_NAME);
	const XMLCh * testValueNodeValue = testValueNode->getNodeValue();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; 3rd property element, 'value' attribute value is not correct",
		(NULL!= testValueNodeValue && XMLString::equals(testValueNodeValue, TEST_VALUE_VALUE_XMLCH)));
}

void AcsAlarmTestCase::verifyFaultStateUserTimestampElement(DOMDocument * doc) 
{
	// Verify the user-timestamp element 
	DOMNodeList * userTimestampNodes = doc->getElementsByTagName(USER_TIMESTAMP_TAG_NAME);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; no user-properties element found", 
		(NULL != userTimestampNodes && userTimestampNodes->getLength() == 1));

	// verify that there are 2 attributes
	DOMNamedNodeMap * attributesMap = userTimestampNodes->item(0)->getAttributes();
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; user-timestamp element does not contain 2 attributes",
		(NULL!= attributesMap && attributesMap->getLength() == 2));

	// check for seconds attribute
	DOMNode * secondsValueNode = attributesMap->getNamedItem(SECONDS_TAG_NAME);
	const XMLCh * secondsValue = secondsValueNode->getNodeValue();
	char *secondsCharValue = XMLString::transcode(secondsValue);
	int secondsIntValue = atoi(secondsCharValue);
	XMLString::release(&secondsCharValue);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; user-timestamp element, 'seconds' attribute value is not correct",
		(NULL!= secondsValue && secondsIntValue == SECONDS_VALUE));

	// check for microseconds attribute
	DOMNode * microsecondsValueNode = attributesMap->getNamedItem(MICROSECONDS_TAG_NAME);
	const XMLCh * microsecondsValue = microsecondsValueNode->getNodeValue();
	char *microsecondsCharValue = XMLString::transcode(microsecondsValue);
	int microsecondsIntValue = atoi(microsecondsCharValue);
	XMLString::release(&microsecondsCharValue);
	CPPUNIT_ASSERT_MESSAGE("FaultState::toXML appears to be broken; user-timestamp element, 'microseconds' attribute value is not correct",
		(NULL!= microsecondsValue && microsecondsIntValue == MICROSECONDS_VALUE));

}

CPPUNIT_TEST_SUITE_REGISTRATION(AcsAlarmTestCase);

#include <cppunit/ui/text/TestRunner.h>
int main(int argc, char *argv[])
{
	char * args[1];
	args[0] = DUMMY_PROCESS_NAME;

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

	return result.wasSuccessful() ? 0 : 1;
}
