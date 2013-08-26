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
#include <ACSAlarmSystemInterfaceFactory.h>
#include <faultStateConstants.h>
#include <logging.h>
#include <loggingGenericLogger.h>
#include <acsalarmDOMErrorHandler.h>
#include <acsalarmStrX.h>

#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

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

// constants we will use when creating the fault
#define DUMMY_PROCESS_NAME "DummyClientProcess"
#define DUMMY_HOSTNAME "DummyClientProcess"
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
using acsalarm::Properties;
using acsalarm::Timestamp;
using std::string;
using std::vector;
using std::auto_ptr;

class AcsAlarmTestCase : public CPPUNIT_NS::TestFixture
{
    CPPUNIT_TEST_SUITE(AcsAlarmTestCase);
    CPPUNIT_TEST(testFaultState);
    CPPUNIT_TEST(testProps);
    CPPUNIT_TEST(testTimestamp);
    CPPUNIT_TEST_SUITE_END();

  public:
		AcsAlarmTestCase();
		~AcsAlarmTestCase();
		void setUp();
		void tearDown();

  protected:
    void testFaultState();
    void testProps();
    void testTimestamp();

	private:
		void verifyFaultStateElement(DOMDocument * doc, bool);
		void verifyDescriptorElement(DOMDocument * doc);
		void verifyUserPropertiesElement(DOMDocument * doc);
		void verifyUserTimestampElement(DOMDocument * doc);

		void verifyFaultStateXML(string xmlToVerify);
		void verifyPropertiesXML(string xmlToVerify);
		void verifyTimestampXML(string xmlToVerify);
		DOMDocument *parseDOM(string xmlToParse);

		DOMBuilder  *parser;
		acsDOMErrorHandler* errHandler;

		// Constants
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
};

AcsAlarmTestCase::AcsAlarmTestCase()
{
}

AcsAlarmTestCase::~AcsAlarmTestCase()
{
}

void AcsAlarmTestCase::testTimestamp()
{
	Timestamp timestamp;

	// test setSeconds & getSeconds
	timestamp.setSeconds(100);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::getSeconds/setSeconds appears to be broken", (100 == timestamp.getSeconds()) );

	// test setMicroSeconds & getMicroSeconds
	timestamp.setMicroSeconds(1000);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::getMicroSeconds/setMicroSeconds appears to be broken", (1000 == timestamp.getMicroSeconds()) );

	// test constructor(secs, microsecs)
	Timestamp timestamp2(100, 1000);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::Timestamp(secs, microsecs) appears to be broken", (100 == timestamp.getSeconds()) );
	CPPUNIT_ASSERT_MESSAGE("Timestamp::Timestamp(secs, microsecs) appears to be broken", (1000 == timestamp.getMicroSeconds()) );

	// test == operator
	CPPUNIT_ASSERT_MESSAGE("Timestamp:: == operator appears to be broken", (timestamp == timestamp2) );

	// test = operator
	Timestamp timestamp3 = timestamp2;
	CPPUNIT_ASSERT_MESSAGE("Timestamp:: = operator appears to be broken", (timestamp3 == timestamp2) );

	// test copy constructor
	Timestamp timestamp4(timestamp3);
	CPPUNIT_ASSERT_MESSAGE("Timestamp::Timestamp(timestamp) - copy constructor appears to be broken", (timestamp3 == timestamp4) );

	// test toXML method
	Timestamp tstamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	verifyTimestampXML(tstamp.toXML());
}

/*
 * XML should look something like this:
 *
 * <source-timestamp seconds="1129902763" microseconds="132000"/>
 */
void AcsAlarmTestCase::verifyTimestampXML(string xmlToVerify)
{
	try
	{
		DOMDocument* doc = parseDOM(xmlToVerify);
		verifyUserTimestampElement(doc);
	}
	catch (const XMLException& toCatch)
	{
		ACS_LOG(LM_ERROR, "Properties::toXML", (LM_ERROR,
			"***** XMLException message: ***** \n\n%s \n *****\n", StrX(toCatch.getMessage()).localForm()))
	}
}

void AcsAlarmTestCase::testProps()
{
	Properties properties;

	// test getProperty and setProperty methods
	string key("key");
	string value("value");
	properties.setProperty(key, value);
	CPPUNIT_ASSERT_MESSAGE("Properties::getProperty/setProperty appears to be broken", (value == properties.getProperty(key)) );

	// test propertyNames method
	string key2("key2");
	string value2("value2");
	properties.setProperty(key2, value2);
	properties.setProperty(key2, value2);
	auto_ptr< vector<string> > keys = properties.propertyNames();
	CPPUNIT_ASSERT_MESSAGE("Properties::propertyNames appears to be broken", (keys->size() == 2) );
	for(unsigned int i = 0; i < keys->size(); i++)
	{
		CPPUNIT_ASSERT_MESSAGE("Properties::propertyNames appears to be broken", (keys->at(i) == key || keys->at(i) == key2) );
	}

	// test copy constructor
	Properties properties2(properties);
	CPPUNIT_ASSERT_MESSAGE("Properties::Properties(&properties) - copy constructor appears to be broken", (value == properties2.getProperty(key)) );

	auto_ptr< vector<string> > keys2 = properties2.propertyNames();
	CPPUNIT_ASSERT_MESSAGE("Properties::Properties(&properties) - copy constructor appears to be broken", (keys2->size() == 2) );
	for(unsigned int i = 0; i < keys2->size(); i++)
	{
		CPPUNIT_ASSERT_MESSAGE("Properties::(&properties) - copy constructor appears to be broken", (keys2->at(i) == key || keys2->at(i) == key2) );
	}

	// test == operator
	CPPUNIT_ASSERT_MESSAGE("Properties:: == operator appears to be broken", (properties == properties2) );

	// test = operator
	Properties properties3 = properties2;
	CPPUNIT_ASSERT_MESSAGE("Properties:: = operator appears to be broken", (value == properties3.getProperty(key)) );

	auto_ptr< vector<string> > keys3 = properties3.propertyNames();
	CPPUNIT_ASSERT_MESSAGE("Properties:: = operator appears to be broken", (keys3->size() == 2) );
	for(unsigned int i = 0; i < keys3->size(); i++)
	{
		CPPUNIT_ASSERT_MESSAGE("Properties:: = operator appears to be broken", (keys3->at(i) == key || keys3->at(i) == key2) );
	}

	// test toXML method
	Properties properties4;
	properties4.setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, PREFIX_VALUE_VALUE);
	properties4.setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, SUFFIX_VALUE_VALUE);
	properties4.setProperty(TEST_NAME_VALUE, TEST_VALUE_VALUE);
	verifyPropertiesXML(properties4.toXML());
}

/*
 * XML should look something like this:
 *
 *  <user-properties>
 *              <property name="ASI_PREFIX" value="prefix"/>
 *              <property name="TEST_PROPERTY" value="TEST_VALUE"/>
 *              <property name="ASI_SUFFIX" value="suffix"/>
 *  </user-properties>
 */
void AcsAlarmTestCase::verifyPropertiesXML(string xmlToVerify)
{
	try
	{
		DOMDocument* doc = parseDOM(xmlToVerify);
		verifyUserPropertiesElement(doc);
	}
	catch (const XMLException& toCatch)
	{
		ACS_LOG(LM_ERROR, "Properties::toXML", (LM_ERROR,
			"***** XMLException message: ***** \n\n%s \n *****\n", StrX(toCatch.getMessage()).localForm()))
	}
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
}

void AcsAlarmTestCase::tearDown()
{
	XMLString::release(&FAULT_STATE_TAG_NAME);
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

	parser->release();
	delete errHandler;
	XMLPlatformUtils::Terminate();
}

void AcsAlarmTestCase::testFaultState()
{
	const string member(MEMBER_VALUE);
	const string family(FAMILY_VALUE);
	const string descriptor(DESCRIPTOR_VALUE);

	// create the FaultState
	auto_ptr<acsalarm::FaultState> fltstate = ACSAlarmSystemInterfaceFactory::createFaultState(family, member, CODE_VALUE);

	// test family getters
	CPPUNIT_ASSERT_MESSAGE("FaultState::getFamily appears to be broken", (family == fltstate->getFamily()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::getMember appears to be broken", (member == fltstate->getMember()) );
	CPPUNIT_ASSERT_MESSAGE("FaultState::getCode appears to be broken", (CODE_VALUE == fltstate->getCode()) );

	// test family setter
	string newfamily = "newfamily";
	fltstate->setFamily(newfamily);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setFamily appears to be broken", (newfamily == fltstate->getFamily()) );

	// restore previous value
	fltstate->setFamily(family);

	// test member setter
	string newmember = "newmember";
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
	Timestamp * tstampPtr = new Timestamp(SECONDS_VALUE, MICROSECONDS_VALUE);
	auto_ptr<Timestamp> tstampAutoPtr(tstampPtr);
	fltstate->setUserTimestamp(tstampAutoPtr);
	CPPUNIT_ASSERT_MESSAGE("FaultState::setUserTimestamp appears to be broken", (*tstampPtr == fltstate->getUserTimestamp()) );

	// test properties getters/setters
	Properties * propsPtr = new Properties();
	propsPtr->setProperty(faultState::ASI_PREFIX_PROPERTY_STRING, PREFIX_VALUE_VALUE);
	propsPtr->setProperty(faultState::ASI_SUFFIX_PROPERTY_STRING, SUFFIX_VALUE_VALUE);
	propsPtr->setProperty(TEST_NAME_VALUE, TEST_VALUE_VALUE);
	auto_ptr<Properties> propsAutoPtr(propsPtr);
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

	// test toXML method
	verifyFaultStateXML(fltstate->toXML());

	// test assignment operator
	FaultState assignedFaultState = *fltstate;
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

/*
 * XML should look something like this:
 *
 * <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
 *     <descriptor>TERMINATE</descriptor>
 *     <user-properties>
 *        <property name="ASI_PREFIX" value="prefix"/>
 *        <property name="ASI_SUFFIX" value="suffix"/>
 *        <property name="TEST_PROPERTY" value="TEST_VALUE"/>
 *     </user-properties>
 *     <user-timestamp seconds="1129902763" microseconds="105000"/>
 *  </fault-state>
 */
void AcsAlarmTestCase::verifyFaultStateXML(string xmlData)
{
	try
	{
		DOMDocument* doc = parseDOM(xmlData);
		verifyFaultStateElement(doc, true);
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

	verifyDescriptorElement(doc);
	if(propertiesAndTimestampPopulated)
	{
		verifyUserPropertiesElement(doc);
		verifyUserTimestampElement(doc);
	}
}

void AcsAlarmTestCase::verifyDescriptorElement(DOMDocument * doc)
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

void AcsAlarmTestCase::verifyUserPropertiesElement(DOMDocument * doc)
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

void AcsAlarmTestCase::verifyUserTimestampElement(DOMDocument * doc)
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
	std::cout.flush();
	CPPUNIT_NS::CompilerOutputter outputter( &result, std::cerr );
	outputter.write();

	// close the AlarmSystemInterfaceFactory
	ACSAlarmSystemInterfaceFactory::done();

	return result.wasSuccessful() ? 0 : 1;
}
