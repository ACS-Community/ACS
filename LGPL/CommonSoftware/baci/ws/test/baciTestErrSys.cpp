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
* "@(#) $Id: baciTestErrSys.cpp,v 1.100 2005/02/09 14:16:01 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* removed getDescription instead ACSError::getDescription is used
* msekoran 2002-05-19 created
*/
 
static char *rcsId="@(#) $Id: baciTestErrSys.cpp,v 1.100 2005/02/09 14:16:01 bjeram Exp $";
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

NAMESPACE_USE(baci)
NAMESPACE_USE(BACI_TEST)

void printCompletion (const ACSErr::ErrorTrace * c, int level)
{
  unsigned int j;

  char *descript = ACSError::getDescription (c->errorType, c->errorCode);

  if (c->errorType!=ACSErr::ACSErrTypeOK || c->errorCode!=ACSErr::ACSErrOK || c->lineNum!=0)
    {
      ACS_SHORT_LOG ((LM_INFO, "[%s][%s, %s @ %s][%s:%d in %s] %s (stackLevel=%d, type=%d, code=%d)",
		      getStringifiedUTC(c->timeStamp).c_str(),
		      c->process.in(), c->thread.in(), c->host.in(),
		      c->file.in(), c->lineNum, c->routine.in(),
		      descript, level, c->errorType, c->errorCode));

    }
  else
    {
      ACS_SHORT_LOG ((LM_INFO, "[%s] %s (stackLevel=%d, type=%d, code=%d)",
		      getStringifiedUTC(c->timeStamp).c_str(), 
		      descript, level, c->errorType, c->errorCode));
    }//if-else

  for (j=0; j<c->data.length(); j++)  
      ACS_SHORT_LOG ((LM_INFO, "\t%s = %s", c->data[j].name.in(), c->data[j].value.in()));

  CORBA::string_free(descript);
  //delete[] descript;
}

void printCompletionStack(const ACSErr::ErrorTrace *c, int depth)
{  
  depth--;
  if (depth)
    {
      const ACSErr::ErrorTrace *nc = &c->previousError[0];
      printCompletionStack (nc, depth);
    }
  printCompletion (c, depth);
}


void printCompletionStack(const ACSErr::ErrorTrace *c)
{  
    int depth = 1; const ACSErr::ErrorTrace *tc = c;
    while (tc->previousError.length()!=0)
      {
	tc = &tc->previousError[0];
	depth++;
      }
    printCompletionStack(c, depth);
}

void printCompletion(const ACSErr::Completion *c)
{
    char *descript = ACSError::getDescription (c->type, c->code);
    int eslen = c->previousError.length();

    ACS_SHORT_LOG((LM_INFO,"[%s] %s (type: %d, code: %d, errorStack: %d)", 
		   getStringifiedUTC(c->timeStamp).c_str(), descript, c->type, c->code, eslen));
    
    if (eslen)
	printCompletionStack(&c->previousError[0]);

    //  delete[] descript;
    CORBA::string_free(descript);

    ACE_OS::printf("\n");
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
	throw (
	    CORBA::SystemException
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
	throw (
	    CORBA::SystemException
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
	throw (
	    CORBA::SystemException
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
	throw (
	    CORBA::SystemException
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

  ACE_TRY
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
     ACE_CATCHANY
      {
       ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
       }
     ACE_ENDTRY;

  // clean up here ...
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

  ACE_TRY
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

        ACS_SHORT_LOG((LM_INFO,"ACSErr::Completion error stack print-out test..."));
	ACSError * error = new ACS_ERROR(ACSErr::ACSErrTypeCommon, ACSErrTypeCommon::FileNotFound, "main");
	error = new ACS_ERROR(error, ACSErr::ACSErrTypeCommon, ACSErrTypeCommon::OutOfBounds, "main");
	error = new ACS_ERROR(error, ACSErr::ACSErrTypeCORBA, ACSErrTypeCommon::Unknown, "main");
	printCompletionStack(error->returnErrorTrace());
	
	error = new ACS_ERROR();
	printCompletionStack(error->returnErrorTrace());
        ACS_SHORT_LOG((LM_INFO,"ACSErr::Completion error stack print-out test ended."));

	//---------------------------------------------------------------

        // Spawns thread for command
	ACS_SHORT_LOG((LM_INFO,"(baciTestErrSys main thread) spawning thread..."));
        if(ACE_Thread::spawn((ACE_THR_FUNC)worker)==-1)
          ACS_SHORT_LOG((LM_DEBUG,"Error in spawning thread"));

	//---------------------------------------------------------------

	ACS_SHORT_LOG((LM_INFO,"(baciTestErrSys main thread) Going to sleep..."));

	BACI_CORBA::getORB()->run(); 

 
	ACS_SHORT_LOG((LM_INFO,"(baciTestErrSys main thread) Shutting down..."));

	baciTest->shutdown();
	
	sleep(10);

	BACI_CORBA::DoneCORBA();

        // Delete the logger last.
        delete m_logger;
    }
  ACE_CATCHANY
    {
        ACE_PRINT_EXCEPTION (ACE_ANY_EXCEPTION,"Error!");
        return -1;
    }
  ACE_ENDTRY;
  ACE_CHECK_RETURN (-1);

  sleep(2);
  return 0;

} /* end main() */








