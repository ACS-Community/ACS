/* @(#) $Id: testRtFindKernelModules.cpp,v 1.4 2006/08/31 18:37:38 ramestic Exp
$
 *
 * Copyright (C) 2006
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

#include <ACSAlarmSystemInterfaceFactory.h>

class AlarmThrowTest : public CppUnit::TestFixture
{
  CPPUNIT_TEST_SUITE( AlarmThrowTest );
  CPPUNIT_TEST( testthrow );
  CPPUNIT_TEST_SUITE_END();
public:
  AlarmThrowTest() : CppUnit::TestFixture(){}

  ~AlarmThrowTest(){}

  void setUp() {
  }

  void testthrow(){

    std::string ff("Test");
    std::string fm("Test");

    /* Check the Exception Cases that segfaults*/
    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::createSource("ALARM_SYSTEM_SOURCES"),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

    /* Check the other Exception Cases */
    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::usingACSAlarmSystem(),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::createSource(),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);


    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::createFaultState("Test", "Test", 0),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::createFaultState(),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::createAndSendAlarm(ff, fm, 0, true, "ALARM_SYSTEM_SOURCES"),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);


    acsalarm::Properties p;
    CPPUNIT_ASSERT_THROW(ACSAlarmSystemInterfaceFactory::createAndSendAlarm(ff, fm, 0, true, p, "ALARM_SYSTEM_SOURCES"),
                         acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

   
  }
}; // End of Test Class

int main( int argc, char **argv)
{

  //maci::SimpleClient m_client;
  //m_client.init(argc,argv);
  //m_client.login();
  //ACSAlarmSystemInterfaceFactory::init(ACSAlarmSystemInterfaceFactory::getManager());
  CppUnit::TextUi::TestRunner runner;
  runner.addTest( AlarmThrowTest::suite() );
  runner.run();
  //m_client.logout();
  //m_client.disconnect();

  return 0;
}
