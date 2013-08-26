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
* "@(#) $Id: PropertyTest.cpp,v 1.2 2009/10/06 16:45:33 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2009-10-06  created 
*/

/************************************************************************
*   NAME
*   
* 
*   SYNOPSIS
*
*   
*   PARENT CLASS
*
* 
*   DESCRIPTION
*
*
*   PUBLIC METHODS
*
*
*   PUBLIC DATA MEMBERS
*
*
*   PROTECTED METHODS
*
*
*   PROTECTED DATA MEMBERS
*
*
*   PRIVATE METHODS
*
*
*   PRIVATE DATA MEMBERS
*
*
*   FILES
*
*   ENVIRONMENT
*
*   COMMANDS
*
*   RETURN VALUES
*
*   CAUTIONS 
*
*   EXAMPLES
*
*   SEE ALSO
*
*   BUGS   
* 
*------------------------------------------------------------------------
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

#include <Properties.h>

#include <string>

static char *rcsId="@(#) $Id: PropertyTest.cpp,v 1.2 2009/10/06 16:45:33 acaproni Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

class PropertyTestCase : public CPPUNIT_NS::TestFixture
{
	CPPUNIT_TEST_SUITE(PropertyTestCase);
	CPPUNIT_TEST(testSize);
	CPPUNIT_TEST(testPropValue);
	CPPUNIT_TEST(testOperatorEqual);
	CPPUNIT_TEST(testAssignment);
	CPPUNIT_TEST_SUITE_END();

protected:
	void testSize();
	void testPropValue();
	void testOperatorEqual();
	void testAssignment();
};

void PropertyTestCase::testSize() {
	acsalarm::Properties prop;
	CPPUNIT_ASSERT_MESSAGE("Wrong size", (prop.getSize()== 0));
	prop.setProperty("PropA","ValueA");
	CPPUNIT_ASSERT_MESSAGE("Wrong size", (prop.getSize()== 1));
	prop.setProperty("PropA","ValueA");
	CPPUNIT_ASSERT_MESSAGE("Wrong size", (prop.getSize()== 1));
	prop.setProperty("PropB","ValueB");
	CPPUNIT_ASSERT_MESSAGE("Wrong size", (prop.getSize()== 2));
}

void PropertyTestCase::testPropValue() {
	acsalarm::Properties prop;
	prop.setProperty("PropA","ValueA");
	prop.setProperty("PropB","ValueB");

	std::string valA=prop.getProperty("PropA");
	CPPUNIT_ASSERT_MESSAGE("Wrong prop val", (valA.compare("ValueA")==0));
	std::string valB=prop.getProperty("PropB");
	CPPUNIT_ASSERT_MESSAGE("Wrong prop val", (valB.compare("ValueB")==0));
}

void PropertyTestCase::testOperatorEqual() {
	acsalarm::Properties prop1;
	prop1.setProperty("PropA","ValueA");
	prop1.setProperty("PropB","ValueB");

	acsalarm::Properties prop2;
	prop2.setProperty("PropB","ValueB");
	prop2.setProperty("PropA","ValueA");

	CPPUNIT_ASSERT_MESSAGE("== does not work", (prop1==prop2));

	acsalarm::Properties prop3;
	CPPUNIT_ASSERT_MESSAGE("!= does not work", (prop3!=prop2));
}

void PropertyTestCase::testAssignment() {
	acsalarm::Properties prop1;
	prop1.setProperty("PropA","ValueA");
	prop1.setProperty("PropB","ValueB");

	acsalarm::Properties prop2=prop1;
	CPPUNIT_ASSERT_MESSAGE("Assignment does not work", (prop1==prop2));

	acsalarm::Properties prop3(prop1);
	CPPUNIT_ASSERT_MESSAGE("Copy constructor does not work", (prop1==prop3));
}

CPPUNIT_TEST_SUITE_REGISTRATION(PropertyTestCase);

int main(int argc, char *argv[])
{
	Logging::Logger::setGlobalLogger(new Logging::GenericLogger("PropertyTestLogger"));

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
