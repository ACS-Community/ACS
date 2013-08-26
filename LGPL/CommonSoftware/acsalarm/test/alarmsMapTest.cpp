/* @(#) $Id: alarmsMapTest.cpp,v 1.4 2011/08/31 18:37:38 acaproni
 *
 * Copyright (C) 2011
 * Associated Universities, Inc. Washington DC, USA.
 *
 * This library is free software; you can redistribute it and/or modify it
 * under the terms of the GNU Library General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
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
 * Correspondence concerning ALMA Software should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
 */


#include <cppunit/TestSuite.h>
#include <cppunit/TestCase.h>
#include <cppunit/TestResult.h>
#include <cppunit/TestCaller.h>
#include <cppunit/ui/text/TestRunner.h>
#include <cppunit/extensions/HelperMacros.h>

#include <AlarmsMap.h>

/**
 * Tests the AlarmsMap.
 *
 * The updating of AlarmsMap internal data structures is normally
 * done in a loop by the AlamsSourceImpl object.
 * As this test does not own a AlarmSourceImpl object, the method is
 * invoked explicitly.
 *
 */
class AlarmsMapTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( AlarmsMapTest );
  CPPUNIT_TEST( testRaiseClear);
  CPPUNIT_TEST( testRepeatAlarms );
  CPPUNIT_TEST_SUITE_END();
private:
  // The object to test
  acsalarm::AlarmsMap map;
public:
  AlarmsMapTest() : CppUnit::TestFixture(){}

  ~AlarmsMapTest(){}

  void setUp() {
	  map.start();
  }

  void tearDown() {
	  map.shutdown();
  }

  /**
   * Test the raising and clearing of the same alarms.
   * <P>
   * It checks:
   * - if the map reply in the correct saying if an alarm must be sent to the alarm server
   * - checks the size of the map
   * - check if the alarms are removed after the time elapses
   */
  void testRaiseClear() {

    std::string ff("FF");
    std::string fm("FM");

    // Raise 10 alarms
    for (int t=0; t<10; t++) {
    	std::string id=buildAlarmID(ff,fm,t);
    	CPPUNIT_ASSERT(map.raise(id));
    }
    map.updateInternalDataStructs();
    CPPUNIT_ASSERT(map.size()==10);
    // Clear the same alarms
    for (int t=0; t<10; t++) {
		std::string id=buildAlarmID(ff,fm,t);
		CPPUNIT_ASSERT(map.clear(id));
	}
    map.updateInternalDataStructs();
	CPPUNIT_ASSERT(map.size()==10);

	sleep(40);
	map.updateInternalDataStructs();
	CPPUNIT_ASSERT(map.size()==0);
  }

  std::string buildAlarmID(std::string faultFamily, std::string faultMember, int faultCode)
  {
  	std::ostringstream oss;
  	oss << faultFamily << ':';
  	oss << faultMember << ':';
  	oss << faultCode;
  	return oss.str();
  }

  /**
   * Issue the same alarms sevral time to check that they are
   * not send more then once to the AS.
   * The same alarms are submitted without changing their activation state
   */
  void testRepeatAlarms() {
	  std::string alRaise=buildAlarmID("raiseFF","raiseFM",12);
	  std::string alClear=buildAlarmID("clearFF","clearFM",8);

	  CPPUNIT_ASSERT(map.raise(alRaise));
	  map.updateInternalDataStructs();
	  CPPUNIT_ASSERT(map.clear(alClear));
	  map.updateInternalDataStructs();
	  CPPUNIT_ASSERT(map.size()==2);
	  for (int j=0; j<5; j++) {
		  for (int t=0; t<10; t++) {
			  CPPUNIT_ASSERT(!map.raise(alRaise));
			  CPPUNIT_ASSERT(!map.clear(alClear));
		  }
	  }
	  map.updateInternalDataStructs();
	  CPPUNIT_ASSERT(map.size()==2);

	  sleep(40);
	  map.updateInternalDataStructs();
	  CPPUNIT_ASSERT(map.size()==0);
  }

}; // End of Test Class

int main( int argc, char **argv)
{

  //maci::SimpleClient m_client;
  //m_client.init(argc,argv);
  //m_client.login();
  //ACSAlarmSystemInterfaceFactory::init(ACSAlarmSystemInterfaceFactory::getManager());
  CppUnit::TextUi::TestRunner runner;
  runner.addTest( AlarmsMapTest::suite() );
  runner.run();
  //m_client.logout();
  //m_client.disconnect();

  return 0;
}
