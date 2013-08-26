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
* "@(#) $Id: baciTestMonitor.cpp,v 1.100 2006/12/13 11:34:00 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-07-02   Added CB object for each monitor and waiting that all CBs are called spcific number of times and after that Test monitor starts with shutdown 
* msekoran 2002-01-06 Added monitor sync. test
* gchiozzi 2001-12-19 Added initialisation of standard LoggingProxy fields
* rlemke 2001-11-09 Removed baciTest global variable. Not used.
* rlemke 2001-11-09 Added missing delete at end of main for dynamically allocated objects
* rlemke 2001-11-09 Added call to new BACI_TEST::shutdown() method to get servant exiting cleanly.
* rlemke 2001-11-08 Added initialisation of ACS::CBDescIn
* rlemke   30/08/01  integrated into tat
*/
 
static char *rcsId="@(#) $Id: baciTestMonitor.cpp,v 1.100 2006/12/13 11:34:00 bjeram Exp $";
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
	  BACI_CORBA::getORB()->object_to_string (ps.in () );
	
      
        ACS_SHORT_LOG((LM_INFO,"Connecting to: %s", ior.in ()));

	//---------------------------------------------------------------
      
        ACS::CBDescIn desc = {0,0,0};
	ACS_SHORT_LOG((LM_INFO,"Trying to get ROdoubleProp..."));
	ACS::ROdouble *rb = ps->ROdoubleProp();
	
	ACS_SHORT_LOG((LM_INFO,"Trying to get RWdoubleProp with ErrorDevIO ..."));
	ACS::RWdouble *rwe = ps->RWdoubleWithErrorDevIOProp();

	//-----------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"Trying to narrow CB for ROdoubleProp... "));

	baciTestCBdouble* mcb1 = new baciTestCBdouble("ROdoubleProp1s", 60);
	CBdouble_var cb1 = mcb1->_this(); 
	
	
	ACS_SHORT_LOG((LM_INFO,"Trying to create 1s monitor for ROdoubleProp..."));
	desc.id_tag = 1;
	ACS::Monitordouble_var md = rb->create_monitor(cb1.in(), desc);
	
	md->set_timer_trigger(10000000);
	

	baciTestCBdouble* mcb5 = new baciTestCBdouble("ROdoubleProp5s", 12);
	CBdouble_var cb5 = mcb5->_this(); 
	

	ACS_SHORT_LOG((LM_INFO,"Trying to create 5s monitor for ROdoubleProp..."));
	desc.id_tag = 5;
	ACS::Monitordouble_var md2 = rb->create_monitor(cb5.in(), desc);
	
	md2->set_timer_trigger(50000000);
	

	baciTestCBdouble* mcb10 = new baciTestCBdouble("ROdoubleProp10s", 6);
	CBdouble_var cb10 = mcb10->_this(); 
	


	ACS_SHORT_LOG((LM_INFO,"Trying to create 10s monitor for ROdoubleProp..."));
	desc.id_tag = 10;
	ACS::Monitordouble_var md3 = rb->create_monitor(cb10.in(), desc);
	
	md3->set_timer_trigger(100000000);
	
	
	//---------------------------------------------------------------
	// Float monitors

	ACS_SHORT_LOG((LM_INFO,"Trying to get ROfloatProp..."));
	ACS::ROfloat *rf = ps->ROfloatProp();
	
	
	ACS_SHORT_LOG((LM_INFO,"Trying to narrow CB for ROfloatProp... "));

	baciTestCBfloat* mcbf1 = new baciTestCBfloat("ROfloatProp1s", 60);
	CBfloat_var cbf1 = mcbf1->_this(); 
	

	ACS_SHORT_LOG((LM_INFO,"Trying to create 1s monitor for ROfloatProp..."));
	desc.id_tag = 1;
	ACS::Monitorfloat_var mf = rf->create_monitor(cbf1.in(), desc);
	
	mf->set_timer_trigger(10000000);
	

	baciTestCBfloat* mcbf5 = new baciTestCBfloat("ROfloatProp5s", 12);
	CBfloat_var cbf5 = mcbf5->_this(); 
	

	ACS_SHORT_LOG((LM_INFO,"Trying to create 5s monitor for ROfloatProp..."));
	desc.id_tag = 5;
	ACS::Monitorfloat_var mf2 = rf->create_monitor(cbf5.in(), desc);
	
	mf2->set_timer_trigger(50000000);
	

	baciTestCBfloat* mcbf10 = new baciTestCBfloat("ROfloatProp10s", 6);
	CBfloat_var cbf10 = mcbf10->_this(); 
	


	ACS_SHORT_LOG((LM_INFO,"Trying to create 10s monitor for ROfloatProp..."));
	desc.id_tag = 10;
	ACS::Monitorfloat_var mf3 = rf->create_monitor(cbf10.in(), desc);
	
	mf3->set_timer_trigger(100000000);

	//---------------------------------------------------------------
	// Pattern monitors

	ACS_SHORT_LOG((LM_INFO,"Trying to get ROpatterProp..."));
	ACS::ROpattern *rp = ps->ROpatternProp();
	
	
	ACS_SHORT_LOG((LM_INFO,"Trying to narrow CB for ROpatternProp... "));

	baciTestCBpattern* mcbp1 = new baciTestCBpattern("ROpatternProp1s", 60);
	CBpattern_var cbp1 = mcbp1->_this(); 
	

	ACS_SHORT_LOG((LM_INFO,"Trying to create 1s monitor for ROpatternProp..."));
	desc.id_tag = 1;
	ACS::Monitorpattern_var mp = rp->create_monitor(cbp1.in(), desc);
	
	mp->set_timer_trigger(10000000);
	

	baciTestCBpattern* mcbp5 = new baciTestCBpattern("ROpatternProp5s", 12);
	CBpattern_var cbp5 = mcbp5->_this(); 
	

	ACS_SHORT_LOG((LM_INFO,"Trying to create 5s monitor for ROpatternProp..."));
	desc.id_tag = 5;
	ACS::Monitorpattern_var mp2 = rp->create_monitor(cbp5.in(), desc);
	
	mp2->set_timer_trigger(50000000);
	

	baciTestCBpattern* mcbp10 = new baciTestCBpattern("ROpatternProp10s", 6);
	CBpattern_var cbp10 = mcbp10->_this(); 
	


	ACS_SHORT_LOG((LM_INFO,"Trying to create 10s monitor for ROpatternProp..."));
	desc.id_tag = 10;
	ACS::Monitorpattern_var mp3 = rp->create_monitor(cbp10.in(), desc);
	
	mp3->set_timer_trigger(100000000);
	
        //--------------------------------------------------------------
        // Testing error handling in Monitor
	baciTestCBdouble* mcb_err = new baciTestCBdouble("RWdoublePropError", 4);
	CBdouble_var cb_err = mcb_err->_this(); 
	
	ACS_SHORT_LOG((LM_INFO,"Trying to create monitor for RWdoublePropWithErrorDevIO..."));
	desc.id_tag = 1;
	ACS::Monitordouble_var md_err = rwe->create_monitor(cb_err.in(), desc);

	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) Going to sleep..."));

	ACE_Time_Value tv(60);

	int rep=5;
	while ((rep>0) && 
	       (mcb1->getCount()>0 || mcb5->getCount()>0 || mcb10->getCount()>0 ||
		mcbp1->getCount()>0 || mcbp5->getCount()>0 || mcbp10->getCount()>0 ||
		mcb_err->getCount()>0 )
	    )
	    {
	    rep--;
	    BACI_CORBA::getORB()->run(tv); 
	    }
	if (rep<0)
	    ACS_SHORT_LOG((LM_ERROR, "Could not get all callbacks in 300s ....."));

	md->destroy();
	

	md2->destroy();
	

	md3->destroy();
	

	mp->destroy();
	

	mp2->destroy();
	

	mp3->destroy();
	
	md_err->destroy();

	tv.sec(3);
	BACI_CORBA::getORB()->run(tv); 
 
	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) Shutting down..."));
	
	ps->shutdown();
	

	sleep(15);

	BACI_CORBA::DoneCORBA();

	// Now delete the callbacks
	delete mcb1;
	delete mcb10;
	delete mcb5;

	delete mcbp1;
	delete mcbp10;
	delete mcbp5;

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





