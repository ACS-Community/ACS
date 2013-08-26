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
* "@(#) $Id: baciTestClient.cpp,v 1.95 2006/09/26 06:26:32 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rlemke 2001-11-09 Added call to new BACI_TEST::shutdown() method to get servant exiting cleanly.
* rlemke 2001-11-08 Added initialisation of ACS::CBDescIn
*/
 
static char *rcsId="@(#) $Id: baciTestClient.cpp,v 1.95 2006/09/26 06:26:32 cparedes Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <tao/corba.h>
#include <ace/Read_Buffer.h>

#include <string>

#include <baci.h>
#include <baciCORBA.h>
#include <baciC.h>
#include <baciS.h>
#include <baciTestC.h>
#include <logging.h>
#include <baciTest.h>
#include <baciTestUtils.h>

 using namespace baci;
 using namespace BACI_TEST;

unsigned int sleep(unsigned int);

#ifdef MAKE_VXWORKS
int startBaciTestClient (int argc,  char **argv)
#else
int main (int argc, char **argv)
#endif
{
    

    try
	{
	// create logging proxy
	LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
	LoggingProxy::init(m_logger);
	LoggingProxy::ProcessName(argv[0]);
	LoggingProxy::ThreadName("main");
	ACS_TEST_INIT_LOGGING;
    
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
	

	if (CORBA::is_nil(object.in())) 
	    {
	    ACS_SHORT_LOG ((LM_DEBUG, "Cannot create OBJ from IOR"));
	    return -1;
	    }

	// Try to narrow the object reference to a PS reference.
	BACI_TEST::BaciTestClass_var ps = BACI_TEST::BaciTestClass::_narrow(object.in());
	

	  CORBA::String_var ior =
				BACI_CORBA::getORB()->object_to_string (ps.in()
									);
	
      
        ACS_SHORT_LOG((LM_INFO,"baciTestClient: Connecting to: %s", ior.in()));

	//---------------------------------------------------------------
      
        ACS::CBDescIn desc = {0,0,0};
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to get ROdoubleProp..."));
	ACS::ROdouble_var prop = ps->ROdoubleProp();
	
	
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to narrow CB for ROdoubleProp..."));

	baciTestCBdouble* mcb = new baciTestCBdouble("ROdoubleProp");
	CBdouble_var cb = mcb->_this(); 
	
	
	ACS_SHORT_LOG((LM_INFO,"baciTestClient: Trying to create monitor for ROdoubleProp..."));
	ACS::Monitordouble_var md = prop->create_monitor(cb.in(), desc);
	
	
	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: main thread, entering ORB loop to sleep..."));
	prop->get_async(cb.in(), desc);

	// Default time trigger is 5s in the test database.
        // Using 18 I am sure to get always the same number of logs
//??????
	ACE_Time_Value tv(25);

	BACI_CORBA::getORB()->run(tv); 

	ACS_SHORT_LOG((LM_INFO,"baciTestClient: main thread, shutting down..."));

	// Deleting objects allocate dynamically before going to the end
	md->destroy();
	

	ps->shutdown();
	
	sleep(10);

	BACI_CORBA::DoneCORBA();
	
	// Now delete the callback
	delete mcb;

	// Delete the logger last.
	delete m_logger;

	}
    catch(CORBA::Exception &ex)
	{
        ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
        return -1;
	}
    ACE_CHECK_RETURN (-1);

    // Wait for the servant to complete cleanup before exiting.
    sleep(2);
    return 0;

} /* end main() */





