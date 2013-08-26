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
* "@(#) $Id: baciTestErrSys.cpp,v 1.105 2010/05/31 09:38:56 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* removed getDescription instead ACSError::getDescription is used
* msekoran 2002-05-19 created
*/
 
static char *rcsId="@(#) $Id: baciTestErrSys.cpp,v 1.105 2010/05/31 09:38:56 bjeram Exp $";
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

#include <acsutilFindFile.h>

#include <acserr.h>
#include <ACSErrTypeCORBAC.h>

 using namespace baci;
 using namespace BACI_TEST;

void printCompletion(const ACSErr::Completion *c)
{

    CompletionImpl comp(*c);
    printf(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>\n");
    comp.log();
    printf("<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<\n");
    return;
}

//-------------------------------------------------------------

class baciTestCBdoubleErr: public baciTestCBdouble
{

  public:
    baciTestCBdoubleErr(std::string _prop) : baciTestCBdouble(_prop) {}

    virtual void working (
	CORBA::Double value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	 ) 
	{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBdouble::working) desc.id_tag: %u Value: %f TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
	    printCompletion(&c);
	}

    virtual void done (
	CORBA::Double value,
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	 )
	{	    
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBdouble::done) desc.id_tag: %u Value: %f TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, value, getStringifiedUTC(c.timeStamp).c_str()));
	    printCompletion(&c);
	}


}; /* end baciTestCBdouble */ 

//-------------------------------------------------------------

class baciTestCBvoidErr: public baciTestCBvoid
 {

  public:
    baciTestCBvoidErr(std::string _prop) : baciTestCBvoid(_prop) {}

    virtual void working (
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	 )
	{
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBvoid::working) desc.id_tag: %u TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, getStringifiedUTC(c.timeStamp).c_str()));
	    printCompletion(&c);
	}

    virtual void done (
	const ACSErr::Completion & c,
	const ACS::CBDescOut & desc
	 )
	{	    
	    ACS_SHORT_LOG((LM_INFO, "(%s::CBvoid::done) desc.id_tag: %u TimeStamp: %s", 
			   prop.c_str(), (unsigned)desc.id_tag, getStringifiedUTC(c.timeStamp).c_str()));
	    printCompletion(&c);
	}

}; /* end baciTestCBvoid */ 

//-------------------------------------------------------------

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

  try
    {
	ACSErr::Completion_var completion;

	ACS_SHORT_LOG((LM_INFO,"Trying to get ACS::RWdouble RWdoubleProp..."));
	ACS::RWdouble_var myRWdouble = baciTest->RWdoubleProp();
	

	ACS::CBDescIn desc = {0,0,0};

	baciTestCBdoubleErr * mcb = new baciTestCBdoubleErr("async");
	ACS::CBdouble_var cb = mcb->_this();
	

	baciTestCBvoidErr * mcbv = new baciTestCBvoidErr("async");
	ACS::CBvoid_var cbv = mcbv->_this();
	

	CORBA::Double min_value = myRWdouble->min_value();
	

	for (int n=0; n<10; n++)
	  {
	    desc.id_tag = 0;

	    // Getting the RWdoubleProp value
	    CORBA::Double RWdoubleProp_value = myRWdouble->get_sync(completion.out());
	    
	    printCompletion(completion.ptr());

	    myRWdouble->get_async(cb.in(), desc);
	    ACE_OS::sleep(1);

	    // Setting the RWdoubleProp value
	    completion = myRWdouble->set_sync(RWdoubleProp_value);
	    
	    printCompletion(completion.ptr());

	    myRWdouble->set_nonblocking(RWdoubleProp_value);
	    

	    myRWdouble->set_nonblocking(min_value-1.0);
	    
	    
	    myRWdouble->set_async(RWdoubleProp_value, cbv.in(), desc);
	    ACE_OS::sleep(1);

	    // Setting the RWdoubleProp value (out_of_bounds expected)
	    completion = myRWdouble->set_sync(min_value-1.0);
	    
	    printCompletion(completion.ptr());

	    myRWdouble->set_async(min_value-1.0, cbv.in(), desc);
	    ACE_OS::sleep(1);

	    // on - OK
	    ACS_SHORT_LOG((LM_INFO,"Invoking on()..."));
	    baciTest->on(cbv.in(), desc);
	    
	    ACE_OS::sleep(1);
	    
	    // off - error
	    ACS_SHORT_LOG((LM_INFO,"Invoking off() - simulated error..."));
	    desc.id_tag = 666;
	    baciTest->off(cbv.in(), desc);
	    
	    ACE_OS::sleep(1);

	  }//for
	
      }
     catch(CORBA::Exception &ex)
      {
       ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
       }

  // clean up here ...
     baciTest->shutdown();

     BACI_CORBA::getORB()->shutdown(); 

  ACS_SHORT_LOG((LM_INFO,"Leaving working thread"));

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
        // Initialisation of CORBA, POA and related CORBA internals  
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
	

	ACSError::init(BACI_CORBA::getORB());

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
	
        // Try to narrow the object reference to a BACI_TEST reference.
	baciTest = BACI_TEST::BaciTestClass::_narrow (object.in ());
	

	CORBA::String_var ior =
	  BACI_CORBA::getORB()->object_to_string (baciTest.in ()
						  );
	
      
        ACS_SHORT_LOG((LM_INFO,"Connecting to: %s", ior.in ()));

	//---------------------------------------------------------------

        ACS_SHORT_LOG((LM_INFO,"ACSErr::CompletionImpl error stack print-out test..."));

	ACSErrTypeCommon::FileNotFoundCompletion c1(__FILE__, __LINE__, "main");
	ACSErrTypeCommon::OutOfBoundsCompletion c2(c1, __FILE__, __LINE__, "main");
	ACSErrTypeCommon::UnknownCompletion c3(c2, __FILE__, __LINE__, "main");

	c3.log();
    
        ACS_SHORT_LOG((LM_INFO,"ACSErr::CompletionImpl error stack print-out test ended."));

	//---------------------------------------------------------------

        // Spawns thread for command
	ACS_SHORT_LOG((LM_INFO,"(baciTestErrSys main thread) spawning thread..."));
        if(ACE_Thread::spawn((ACE_THR_FUNC)worker)==-1)
          ACS_SHORT_LOG((LM_DEBUG,"Error in spawning thread"));

	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"(baciTestErrSys main thread) Going to sleep..."));
	BACI_CORBA::getORB()->run(); 


	ACS_SHORT_LOG((LM_INFO,"(baciTestErrSys main thread) Shutting down..."));

	sleep(2);

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








