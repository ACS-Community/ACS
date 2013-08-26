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
* "@(#) $Id: baciTestActions.cpp,v 1.95 2006/09/26 06:26:32 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2001-12-19 Added initialisation of standard LoggingProxy fields
* rlemke 2001-11-09 Added sleep in command loop, to make test deterministic.
* rlemke 2001-11-09 Added call to new BACI_TEST::shutdown() method to get servant exiting cleanly.
* rlemke 2001-11-08 Added initialisation of ACS::CBDescIn
* rlemke   30/08/01  integrated into tat
*/
 
static char *rcsId="@(#) $Id: baciTestActions.cpp,v 1.95 2006/09/26 06:26:32 cparedes Exp $";
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
BACI_TEST::BaciTestClass_var baciTest;

static void* worker(void *arg)
{
 
  ACE_UNUSED_ARG(arg);
 
  
  LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
  LoggingProxy::init(m_logger);
  LoggingProxy::ProcessName("");
  LoggingProxy::ThreadName("worker");
  ACS_TEST_INIT_LOGGING;

  ACS_SHORT_LOG((LM_INFO,"Entering working thread"));

  ACS::CBDescIn desc = {0,0,0};
  while(1) {
     try
      {
       baciTestCBvoid* mcbv = new baciTestCBvoid("on");
       ACS::CBvoid_var cbv = mcbv->_this();
       
       ACS_SHORT_LOG((LM_INFO,"performing on action"));
       baciTest->on(cbv.in(), desc);

       baciTestCBvoid* mcbvf = new baciTestCBvoid("off");
       ACS::CBvoid_var cbvf = mcbvf->_this();
       
       ACS_SHORT_LOG((LM_INFO,"performing off action"));
       baciTest->off(cbvf.in(), desc);

       baciTestCBvoid* mcbvr = new baciTestCBvoid("reset");
       ACS::CBvoid_var cbvr = mcbvr->_this();
       
       ACS_SHORT_LOG((LM_INFO,"performing reset action"));
       baciTest->reset(cbvr.in(), desc);

       sleep(3);

        
      }
     catch(CORBA::Exception &ex)
      {
       ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
       // I should cleanup here the callback
       }

     ACE_Thread::yield();
     ACE_OS::sleep (5);
   }
  ACS_SHORT_LOG((LM_INFO,"leaving working thread"));
   return 0;
}

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
	    ACS_SHORT_LOG ((LM_ERROR, "Cannot read IOR from file"));
	    return -1;
	    }

	// Get an object reference from the argument string.
	CORBA::Object_var object = 
	  BACI_CORBA::getORB()->string_to_object (readIOR.c_str());
	

	if (CORBA::is_nil(object.in())) {
	  ACS_SHORT_LOG ((LM_DEBUG, "Cannot create OBJ from IOR"));
	  return -1;
	}

	// Try to narrow the object reference to a BACI_TEST reference.
	baciTest = BACI_TEST::BaciTestClass::_narrow (object.in ());
	

	CORBA::String_var ior =
	  BACI_CORBA::getORB()->object_to_string (baciTest.in ()
						  );
	
      
        ACS_SHORT_LOG((LM_INFO,"Connecting to: %s", ior.in ()));

	//---------------------------------------------------------------

        // Spawns thread for command
	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) spawning thread..."));
        if(ACE_Thread::spawn((ACE_THR_FUNC)worker)==-1)
          ACS_SHORT_LOG((LM_DEBUG,"Error in spawning thread"));

	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) Going to sleep..."));

	ACE_Time_Value tv(17);
	BACI_CORBA::getORB()->run(tv); 
 
	ACS_SHORT_LOG((LM_INFO,"(baciTest1Client main thread) Shutting down..."));

	baciTest->shutdown();
	
	sleep(4);

	BACI_CORBA::DoneCORBA();
        // Delete the logger last.
        delete m_logger;
    }
  catch(CORBA::Exception &ex)
    {
        ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
        return -1;
    }
  ACE_CHECK_RETURN (-1);

  sleep(2);
  return 0;

} /* end main() */





