/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciTestTurningOnOffMonitoring.cpp,v 1.4 2006/09/26 06:26:32 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/
 
static char *rcsId="@(#) $Id: baciTestTurningOnOffMonitoring.cpp,v 1.4 2006/09/26 06:26:32 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <tao/corba.h>
#include <ace/Read_Buffer.h>

#include <string>

#include <baci.h>
#include <baciCORBA.h>
#include <baciC.h>
#include <baciS.h>
#include <baciTestC.h>
#include <baciTestS.h>
#include <logging.h>
#include <acsutil.h>
#include <baciTest.h>
#include <baciTestUtils.h>
#include <baciTestClassImpl.h>

 using namespace baci;
 using namespace BACI_TEST;

#ifdef MAKE_VXWORKS
unsigned int sleep(unsigned int);
#endif


//-------------------------------------------------------------

int main (int argc, char **argv)
{
  

  // create logging proxy
  LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
  LoggingProxy::init(m_logger);
  LoggingProxy::ProcessName(argv[0]);
  LoggingProxy::ThreadName("main");
  ACS_TEST_INIT_LOGGING;
  bool isPropertiesMonitoringActive=false;

  try
    {
        // 
        // Initialysation of CORBA, POA and related CORBA internals  
        // 
    ACE_CString g_strCmdLn;
	for (int i=argc-1; i>=0; i--)
	    g_strCmdLn = ACE_CString(argv[i])+ " " + g_strCmdLn;

	if (g_strCmdLn.find("-ORBDottedDecimalAddresses")==ACE_CString::npos)
	    g_strCmdLn += " -ORBDottedDecimalAddresses 1";

	ACE_TCHAR **m_argv = argv;
	int m_argc = argc;
	ACE_OS::string_to_argv((ACE_TCHAR*)g_strCmdLn.c_str(),
			 m_argc,
			 m_argv);

	BACI_CORBA::InitCORBA(m_argc, m_argv);
	

	std::string readIOR;

	int result = read_IOR_from_file ("BACI1", readIOR);
	if (result != 0)
	    {
	    ACS_SHORT_LOG((LM_ERROR, "Cannot read IOR from file"));
	    return -1;
	    }

	// Get an object reference from the argument string.
	CORBA::Object_var object = 
	  BACI_CORBA::getORB()->string_to_object (readIOR.c_str());
	

	if (CORBA::is_nil(object.in())) {
	  ACS_SHORT_LOG ((LM_DEBUG, "Cannot create OBJ from IOR"));
	  return -1;
	}

	// Try to narrow the object reference to a PS reference.
	BACI_TEST::BaciTestClass_var ps = BACI_TEST::BaciTestClass::_narrow (object.in ());
	

	CORBA::String_var ior =
	  BACI_CORBA::getORB()->object_to_string (ps.in ()
						  );
	
      
        ACS_SHORT_LOG((LM_INFO,"Connecting to: %s", ior.in ()));

	//---------------------------------------------------------------
      
        ACS::CBDescIn desc = {0,0,0};
	ACS_SHORT_LOG((LM_INFO,"Trying to get ROdoubleProp..."));
	ACS::RWdouble *prop = ps->RWdoubleProp();

	prop->set_sync(0.0);
	
	
	ACS_SHORT_LOG((LM_INFO,"Trying to narrow CB for ROdoubleProp... "));

	baciTestCBdouble* mcb1 = new baciTestCBdouble("ROdoubleProp1s", 60);
	CBdouble_var cb1 = mcb1->_this(); 
	
	
	ACS_SHORT_LOG((LM_INFO,"Trying to create 0.5s monitor for ROdoubleProp..."));
	desc.id_tag = 1;
	ACS::Monitordouble_var md = prop->create_monitor(cb1.in(), desc);
	
	md->set_timer_trigger(5000000);
	sleep(2); // sleep for 2sec. We should not get anything in this time

	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is suspended: %d", isPropertiesMonitoringActive));

	// We start by suspending the monitor.
        // If the monitor was never started, this should simply
        // return as if nothing was done.
        // But in the first implementation we got an exception handled by
        // the implementation of turnOffMonitoring, because
        // there was no check about the monitor beeing created before
        // trying to retrieve the thread object (not existing) and resume it.
	ACS_SHORT_LOG((LM_INFO,"Turn OFF Monitoring"));
	ps->turnOffMonitoring();	
	sleep(2); // sleep for another 2sec. We should not get anything in this time

	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is active: %d", isPropertiesMonitoringActive));

	prop->set_sync(1.0);
	ACS_SHORT_LOG((LM_INFO,"Turn ON Monitoring"));
	ps->turnOnMonitoring();	
	sleep(2); // sleep for another 2sec. Now we should receive value 1.0

	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is active: %d", isPropertiesMonitoringActive));

	ACS_SHORT_LOG((LM_INFO,"Turn OFF Monitoring"));
	ps->turnOffMonitoring();
	prop->set_sync(2.0);
	sleep(2); // sleep for another 2sec. Now we should *not* receive value 2.0
	
	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is active: %d", isPropertiesMonitoringActive));

	//let's try if we can turn it off again
	ACS_SHORT_LOG((LM_INFO,"Turn OFF Monitoring"));
	ps->turnOffMonitoring();

	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is active: %d", isPropertiesMonitoringActive));

	prop->set_sync(3.0);
	ACS_SHORT_LOG((LM_INFO,"Turn ON Monitoring"));
	ps->turnOnMonitoring();	
	sleep(2); // sleep for another 2sec. Now we should receive value 3.0

	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is active: %d", isPropertiesMonitoringActive));

	//let's try if we can turn it on again
	ACS_SHORT_LOG((LM_INFO,"Turn ON Monitoring"));
	ps->turnOnMonitoring();	

	// Checks the status of monitoring
	isPropertiesMonitoringActive=ps->isPropertiesMonitoringActive();
	ACS_SHORT_LOG((LM_INFO,"Monitoring is active: %d", isPropertiesMonitoringActive));

	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) Going to sleep..."));

	ACE_Time_Value tv(60);

	int rep=5;
	while ((rep>0) && 
	       (mcb1->getCount()>0 )
	    )
	    {
	    rep--;
	    BACI_CORBA::getORB()->run(tv); 
	    }
	if (rep<0)
	    ACS_SHORT_LOG((LM_ERROR, "Could not get all callbacks in 300s ....."));

	md->destroy();
	

	tv.sec(3);
	BACI_CORBA::getORB()->run(tv); 
 
	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) Shutting down..."));
	
	ps->shutdown();
	

	sleep(15);

	BACI_CORBA::DoneCORBA();

	// Now delete the callbacks
	delete mcb1;

	// Delete the logger last.
	delete m_logger;
	LoggingProxy::done();
	std::cout << std::flush; 
    }
  catch(CORBA::Exception &ex)
    {
        ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
        return -1;
    }
  ACE_CHECK_RETURN (-1);

  return 0;

} /* end main() */





