/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009 
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
* "@(#) $Id: FaultStateTest.cpp,v 1.1 2009/10/07 16:42:38 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2009-10-06  created
*/



#include "vltPort.h"

#include <cppunit/BriefTestProgressListener.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

#include <logging.h>
#include <loggingGenericLogger.h>

#include <FaultState.h>

#include <string>

static char *rcsId="@(#) $Id: FaultStateTest.cpp,v 1.1 2009/10/07 16:42:38 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

class FaultStateTestCase : public CPPUNIT_NS::TestFixture
{
	CPPUNIT_TEST_SUITE(FaultStateTestCase);
	CPPUNIT_TEST(test_FF_FM_FC);
	CPPUNIT_TEST(testInitialTimestamp);
	CPPUNIT_TEST(testInitialProps);
	CPPUNIT_TEST(testCopyConstructor);
	CPPUNIT_TEST_SUITE_END();

protected:
	void test_FF_FM_FC();
	void testInitialTimestamp();
	void testInitialProps();
	void testCopyConstructor();
};

void FaultStateTestCase::test_FF_FM_FC() {
	acsalarm::FaultState fs("FF","FM",1);
	std::string family = fs.getFamily();
	std::string member = fs.getMember();
	int code=fs.getCode();
	CPPUNIT_ASSERT_MESSAGE("Wrong FF", (family.compare("FF")==0));
	CPPUNIT_ASSERT_MESSAGE("Wrong FM", (member.compare("FM")==0));
	CPPUNIT_ASSERT_MESSAGE("Wrong FC", (code==1));
}

void FaultStateTestCase::testInitialTimestamp() {
	acsalarm::FaultState fs("FF","FM",1);
	acsalarm::Timestamp ts = fs.getUserTimestamp();
	long secs=ts.getSeconds();
	long msecs=ts.getMicroSeconds();
	CPPUNIT_ASSERT_MESSAGE("Wrong secs", (ts.getSeconds()==secs));
	CPPUNIT_ASSERT_MESSAGE("Wrong msecs", (ts.getMicroSeconds()==msecs));
}

void FaultStateTestCase::testInitialProps() {
	acsalarm::FaultState fs("FF","FM",1);
	acsalarm::Properties props=fs.getUserProperties();
	CPPUNIT_ASSERT_MESSAGE("Wrong initial properties size", (props.getSize()==0));
}

void FaultStateTestCase::testCopyConstructor() {
	// Create the FS to copy
	acsalarm::FaultState fs("FF","FM",1);
	acsalarm::Timestamp ts = fs.getUserTimestamp();
	long secs=ts.getSeconds();
	long msecs=ts.getMicroSeconds();

	acsalarm::Properties* props=new acsalarm::Properties();
	props->setProperty("Prop1", "Val1");
	props->setProperty("Prop2", "Val2");
	std::auto_ptr<acsalarm::Properties> theProperties(props);

	fs.setUserProperties(theProperties);

	std::string descriptor="desc";
	fs.setDescriptor(descriptor);

	acsalarm::FaultState copied(fs);
	CPPUNIT_ASSERT_MESSAGE("Wrong FF", (copied.getFamily().compare("FF")==0));
	CPPUNIT_ASSERT_MESSAGE("Wrong FM", (copied.getMember().compare("FM")==0));
	CPPUNIT_ASSERT_MESSAGE("Wrong FC", (copied.getCode()==1));
	acsalarm::Properties copiedProps=copied.getUserProperties();
	CPPUNIT_ASSERT_MESSAGE("Props not copied", (copiedProps.getSize()==2));

	CPPUNIT_ASSERT_MESSAGE("Wrong descriptor", (copied.getDescriptor().compare("desc")==0));

	acsalarm::Timestamp ts2 = copied.getUserTimestamp();
	CPPUNIT_ASSERT_MESSAGE("Wrong secs", (ts2.getSeconds()==secs));
	CPPUNIT_ASSERT_MESSAGE("Wrong msecs", (ts2.getMicroSeconds()==msecs));
}


CPPUNIT_TEST_SUITE_REGISTRATION(FaultStateTestCase);

int main(int argc, char *argv[])
{
	Logging::Logger::setGlobalLogger(new Logging::GenericLogger("FaultStateTestLogger"));

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

	if (result.wasSuccessful()) {
		std::cout<<"No errors in test"<<std::endl;
	} else {
		std::cout<<"The test reported errors"<<std::endl;
	}
	return result.wasSuccessful() ? 0 : 1;
}


/*___oOo___*/
