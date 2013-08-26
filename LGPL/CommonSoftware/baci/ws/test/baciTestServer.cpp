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
* "@(#) $Id: baciTestServer.cpp,v 1.122 2011/02/17 18:25:39 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* msekoran 2002-02-07 Fixed destruction.
* gchiozzi 2002-02-06 Fixed bug in call to DBConnector::initDB()
* gchiozzi 2001-12-19 Added initialisation of standard LoggingProxy fields
*/
 
static char *rcsId="@(#) $Id: baciTestServer.cpp,v 1.122 2011/02/17 18:25:39 rtobar Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <vltPort.h>
#include <acsutil.h> 
#include <acsutilPorts.h>

#include <baci.h>
#include <baciDB.h>

#include "baciTestClassImpl.h"
#include "baciTest.h"
#include "baciTestUtils.h"
#include "baciTestContainerServices.h"



#include "maciS.h"

#ifdef MAKE_VXWORKS
#	include <acsutilArgUnpack.h>
#else
#include "ACSAlarmSystemInterfaceFactory.h"
#endif

//--------------------------------------
// do not use ACS ! 
// ACS:: prefix is obligatory for VxWorks 
// (for all CORBA objects, object declared in an IDL, e.g. ACS::Callback, maci::Manager)
//  using namespace ACS;
 using namespace baci;

/**
 * Hardcoded !
 */ 
static int   devCount = 2;
static char *devices[] = {"BACI1", "BACI2"};
static char *idlTypes[] = {"IDL:alma/Control/IFProc", "IDL:alma/Control/DRX"};
static bool shutting_down = false;
static bool signaled = false;
static TestContainerServices *cs_p[]={0, 0};
static LoggingProxy *g_logger = 0;
LoggingProxy *threadLogger = 0;

/// The mutual exclusion mechanism which is required to use the
/// <condition_>.
static ACE_SYNCH_MUTEX mutex;

/**
 * Condition used to wait until finalize method has finished
 */
static ACE_SYNCH_CONDITION condition(mutex);

/*
 * Thread initializer and thread done functions
 *
 */

void initThread(const char * threadName)
{
    ACS_CHECK_LOGGER;
//    threadLogger = new LoggingProxy(0, 0, 31, 0);
    LoggingProxy::init(g_logger);
    LoggingProxy::ThreadName(threadName);
}//initThread

void doneThread()
{
    LoggingProxy::done();
 //    if (threadLogger) delete threadLogger;
}//doneThread

/******************************************************************************************/

void finalize()
{
    

    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Destroying CharacteristicComponents."));
	
	for (int n=0; n < devCount; n++)
	  {

	  try
	    {
		
	      ACS_SHORT_LOG((LM_INFO,"baciTestServer: Destroying \"%s\"", devices[n]));

	      cs_p[n]->getThreadManager()->stopAll();

	      PortableServer::ObjectId_var id =
		PortableServer::string_to_ObjectId(devices[n]);
	    
	      BACI_CORBA::getPOA()->deactivate_object(id.in());

	    }
	  catch(...)
	    {
	      ACS_SHORT_LOG((LM_INFO, 
		     "baciTestServer: Exceptions raised while destroying Component"));
	      // do not bail out just because of one Component destruction failure 
	      // ACE_CHECK_RETURN (-1);
	    }
	  }

    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Closing CORBA."));

    try
	{
	  BACI_CORBA::getORB()->shutdown(false);
	
	}
    catch(CORBA::Exception &ex)
	{
	ACE_PRINT_EXCEPTION(ACE_ANY_EXCEPTION, "finalize");
	}

    if (CDBPropertySet::getInstance() != 0)
	{
	CDBPropertySet * propSet_p = CDBPropertySet::getInstance();
	delete propSet_p;
	}

    ACSError::done();
    DBConnector::closeDB();
    BACI_CORBA::DoneCORBA();
}


void finalizeOtherThread()
{
    if (shutting_down) return;
    shutting_down=true;
    
    // initialize logger (this is a new thread)
    if (g_logger)
      {
	LoggingProxy::init(g_logger);
	LoggingProxy::ThreadName("finalization");
	ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !"));
      }

    finalize();

    sleep(2);
    LoggingProxy::done(); 
    std::cout << std::flush; 

    signaled = true;
    condition.signal();
}


void TerminationSignalHandler(int)
{
    finalizeOtherThread();
}


#ifdef MAKE_VXWORKS

int startBaciTestServer(char *szCmdLn)
{
  int  argc;
  char *argv[100];

  ACE_OS_Object_Manager ace_os_object_manager;
  ACE_Object_Manager ace_object_manager;

  argc = argUnpack(szCmdLn, argv);
  argv[0] = "baciTestServer";

#else
int main(int argc, char* argv[])
{ 
#endif


    CORBAShutdown::setShutdownFunction(&finalizeOtherThread);
    bool monitoring=true;

       
    // create logging proxy
    g_logger = new LoggingProxy(0, 0, 31, 0);
    LoggingProxy::init(g_logger);
    LoggingProxy::ProcessName(argv[0]);
    LoggingProxy::ThreadName("main");
    ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !"));
    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Starting"));

    if ( argc>=2 )
	{
	monitoring = atoi(argv[1]);
	ACS_SHORT_LOG((LM_INFO,"baciTestServer: monitoring flag: %d passed from cmd line", monitoring));
	}

      // Installs termination signal handlers, to be able
    // to terminate cleanly
    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Installing signal handlers"));
    ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
    ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

    BACIThread::setInitializers(initThread, doneThread);
    
    ACS_SHORT_LOG((LM_INFO,"baciTestServer: CharacteristicComponent IDs ready"));
    if (devCount<1) {
       ACS_SHORT_LOG((LM_INFO,"No devices found for %s", argv[0]));
      return -1;
    }
    
    CORBA::String_var ior[devCount];
    CORBA::String_var name[devCount];
    CORBA::String_var type[devCount];

    int count = 0;
        
    try {

     BACI_CORBA::InitCORBA(argc, argv);

    if (!ACSError::init (BACI_CORBA::getORB()))
      {
	ACS_LOG(LM_RUNTIME_CONTEXT, "baciTestServer",
		(LM_INFO, "Failed to initialize ACSError."));
	return -1;
      }

    // read parameters
    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Init DB"));
    if (!DBConnector::initDB(":Appl_data:alma", argc, argv, BACI_CORBA::getORB()))
      {
	ACS_SHORT_LOG((LM_INFO,"Error creating DB..."));
	return -1;
      }

#ifndef MAKE_VXWORKS
	// Init the Alarm factory      
    ACSAlarmSystemInterfaceFactory::init(maci::Manager::_nil());
#endif

     // Instantiate the CBDPropertySet singleton
     if (DBConnector::getDBTable())
	 CDBPropertySet::createInstance(BACI_CORBA::getORB(), 
					BACI_CORBA::getPOAManager(),
					BACI_CORBA::getPOARoot());

     for (int n=0; n < devCount; n++) {
 
        ACS_SHORT_LOG((LM_INFO,"baciTestServer: Exporting \"%s\"", devices[n]));
        
	ACE_CString compName(devices[n]);
	ACE_CString compType(idlTypes[n]);

	cs_p[n] = new TestContainerServices(compName,compType,
					 BACI_CORBA::getPOA(), BACI_CORBA::getORB());

        BaciTestClassImpl* ps = new BaciTestClassImpl(devices[n], 
						      cs_p[n],
						      monitoring);

	CORBA::Object_var ps_obj = BACI_CORBA::ActivateCORBAObject(ps, devices[n]);
	if (CORBA::is_nil(ps_obj.in()))
	  {
	    ACS_SHORT_LOG((LM_ERROR,"baciTestServer: Failed to activate \"%s\"", devices[n]));
	    delete ps;
	    delete cs_p[n];
	    finalize();
	    throw CORBA::NO_RESOURCES();
	  }
	
	// remove reference to servant, so that only POA nows owns it
	// when POA will release servent's reference, ref. 
        // count will drop to 0 and servant will be deleted
	ps->_remove_ref();
	
        ior[count] = BACI_CORBA::getORB()->object_to_string (ps_obj.in());
        
        name[count] = CORBA::string_dup(devices[n]);
        type[count] = CORBA::string_dup("BaciTestClass");
        count++;
 
        ACS_SHORT_LOG((LM_INFO, "baciTestServer: %s exported", devices[n]));
        
      }
    }
    catch (...) {
      ACS_SHORT_LOG((LM_INFO, "baciTestServer: Exceptions raised while creating Component"));
      return -1;
      //ACE_CHECK_RETURN (-1);
    }
    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Writing IORS to file..."));
 
    write_iors_to_file(count, name, type, ior);
 
 	
    ACS_SHORT_LOG((LM_INFO,"baciTestServer: Running ORB."));
    BACI_CORBA::getORB()->run();
    
    

    // check if already shut down using signals
    if (shutting_down)
      {
        // "strange" thing, if server is killed using 
        // 'killall SIGTERM baciTestServer' it causes call of signal handles,
        // but if CTRL-C is pressed all threads receive signal 
        // (including CORBA ORB main thread) and run() method terminates
        // this makes the main program to terminate before initialization() is done
        if (!signaled) condition.wait();
      }
    else
	finalize();
 
    LoggingProxy::done(); 
    std::cout << std::flush; 

    return 0;
}








