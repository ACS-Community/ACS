/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2013-05-23  created
*/
#include <iostream>
#include <string>
#include <OS.h>
#include <cppunit/CompilerOutputter.h>
#include <cppunit/extensions/TestFactoryRegistry.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestResultCollector.h>
#include <cppunit/TestRunner.h>
#include <cppunit/BriefTestProgressListener.h>

#include "testAcsDaemonUtils.h"

CPPUNIT_TEST_SUITE_REGISTRATION (TestAcsDaemonUtils);

void TestAcsDaemonUtils::setUp (void)
{
	daemonUtil_mp=new AcsDaemonUtils();
}

void TestAcsDaemonUtils::tearDown (void)
{
	delete daemonUtil_mp;
}

void TestAcsDaemonUtils::testLogDirectory(void)
{
	std::string acsdataStr(ACE_OS::getenv("ACSDATA"));
	std::string hostStr(ACE_OS::getenv("HOST"));
	std::string expectsLogDir=acsdataStr+"/logs/"+hostStr+"/";
	// Get the directory
	std::string logDir = daemonUtil_mp->getLogDirectory();
	// Check if the folder is what we expect
	CPPUNIT_ASSERT_EQUAL (expectsLogDir.compare(logDir), 0);
	// Check if the folder exists
	CPPUNIT_ASSERT_EQUAL (ACE_OS::access(logDir.c_str(),F_OK|W_OK),0);
}

void TestAcsDaemonUtils::testLogDirectoryForPlainContainer(void)
{
	// All hte logs for non hierarchical containers go in $ACSDATA/logs/<HOST>
	std::string expectedFolder=daemonUtil_mp->getLogDirectory();

	// Non hierarchical container name - folder does not exist
	std::string contName="testContainer";
	std::string logFolder=daemonUtil_mp->getLogDirectoryForContainer(contName);
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder), 0);

	// Non hierarchical container name - folder exists
	logFolder="";
	logFolder=daemonUtil_mp->getLogDirectoryForContainer(contName);
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder), 0);

	// Same as previous case, but with a trailing "/"
	std::string logFolder2=daemonUtil_mp->getLogDirectoryForContainer(contName+'/');
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder2), 0);

	// A container name with a trailing "/" (should never happen)
	std::string contName2="testContainer2/";
	std::string logFolder3=daemonUtil_mp->getLogDirectoryForContainer(contName2);
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder3), 0);
}

void TestAcsDaemonUtils::testLogDirectoryForHierarchicalContainer(void)
{
	// Non hierarchical container name - folder does not exist
	std::string contName="CONTROL/ACC/testContainer";
	std::string logFolder=daemonUtil_mp->getLogDirectoryForContainer(contName);
	std::string expectedFolder=daemonUtil_mp->getLogDirectory()+"CONTROL/ACC/";
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder), 0);

	// Non hierarchical container name - folder exists
	logFolder="";
	logFolder=daemonUtil_mp->getLogDirectoryForContainer(contName);
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder), 0);

	// Same as previous case, but with a trailing "/"
	logFolder="";
	logFolder=daemonUtil_mp->getLogDirectoryForContainer(contName+'/');
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder), 0);

	// A container name with a trailing "/" (should never happen)
	std::string contName2="CONTROL/ACC/testContainer2/";
	std::string logFolder2=daemonUtil_mp->getLogDirectoryForContainer(contName2);
	std::string expectedFolder2=daemonUtil_mp->getLogDirectory()+contName2;
	CPPUNIT_ASSERT_EQUAL (expectedFolder.compare(logFolder), 0);
}

void TestAcsDaemonUtils::testTimestamp(void)
{
	std::string timestamp=daemonUtil_mp->getTimestamp();
	// The validity of the timestamp is already tested in acsutil
	// This test only checks if it has been correctly reformatted
	std::string temp;
	for (unsigned int i=0;i<timestamp.length(); ++i) {
		if (timestamp[i]>='0' && timestamp[i]<='9') {
			temp+='#';
		} else {
			temp+=timestamp[i];
		}
	}
	std::string expected("####-##-##_##.##.##.###");
	CPPUNIT_ASSERT_EQUAL (expected.compare(temp), 0);
}

void TestAcsDaemonUtils::testSimpleContainerName(void)
{
	std::string simpleContName("containerName");
	CPPUNIT_ASSERT_EQUAL (simpleContName.compare(daemonUtil_mp->getSimpleContainerName(simpleContName)),0);

	std::string folders("CONTROL/ACC/");
	std::string hierarchicalContName(folders);
	hierarchicalContName=hierarchicalContName+simpleContName;
	std::cout<<"["<<daemonUtil_mp->getSimpleContainerName(hierarchicalContName)<<"]"<<std::endl;
	CPPUNIT_ASSERT_EQUAL (simpleContName.compare(daemonUtil_mp->getSimpleContainerName(hierarchicalContName)),0);
}

int main(int argc, char *argv[])
{

    // informs test-listener about testresults
    CPPUNIT_NS :: TestResult testresult;

    // register listener for collecting the test-results
    CPPUNIT_NS :: TestResultCollector collectedresults;
    testresult.addListener (&collectedresults);

    // register listener for per-test progress output
    CPPUNIT_NS :: BriefTestProgressListener progress;
    testresult.addListener (&progress);

    // insert test-suite at test-runner by registry
    CPPUNIT_NS :: TestRunner testrunner;
    testrunner.addTest (CPPUNIT_NS :: TestFactoryRegistry :: getRegistry ().makeTest ());
    testrunner.run (testresult);

    // output results in compiler-format
    CPPUNIT_NS :: CompilerOutputter compileroutputter (&collectedresults, std::cerr);
    compileroutputter.write ();

    // return 0 if tests were successful
    return collectedresults.wasSuccessful () ? 0 : 1;
}

/*___oOo___*/
