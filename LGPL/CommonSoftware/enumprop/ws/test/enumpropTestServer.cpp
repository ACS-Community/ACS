/***************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
*    MA 02111-1307  USA
*
* "@(#) $Id: enumpropTestServer.cpp,v 1.56 2011/10/16 08:43:58 hsommer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-08-15 changed argUnpack.h to acsutilArgUnpack.h
* bjeram 2001-12-03 created 
*/


#include "vltPort.h"

static char *rcsId="@(#) $Id: enumpropTestServer.cpp,v 1.56 2011/10/16 08:43:58 hsommer Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <iostream>

#include <acsutil.h> 
#include <acsutilPorts.h>

#include <baci.h>
#include <baciDB.h>
#include <maciContainerServices.h>

#include "enumpropTestDeviceImpl.h"

#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"
#else
#include "ACSAlarmSystemInterfaceFactory.h"
#endif

using namespace ACS;
using namespace baci;

// We need an implementation of the ContainerServices here because
// 1. the component fails if the pointer to the ContainerServices it receives
//    in the constructor is NULL
// 2. the smart pointer used to store the ContainerServices deletes all
//    the not-NULL instances of the Container Services
// This clla implements all the methods of the interface but does nothing
class TestContainerServices : public maci::ContainerServices {
 public:
    CORBA::ORB_var m_orb;

    TestContainerServices(ACE_CString& compName, PortableServer::POA_ptr poa, CORBA::ORB_ptr orb):
        maci::ContainerServices(compName,poa), m_orb(CORBA::ORB::_duplicate(orb)) {
            ACS_SHORT_LOG((LM_INFO,"TestContainerServices built"));
				m_componentType = ACE_CString("IDL:ENUMPROP_TEST/enumpropTestDevice:1.0");
        }
        
        virtual ~TestContainerServices() {
            ACS_SHORT_LOG((LM_INFO,"TestContainerServices destroyed"));
        }
    
        CORBA::Object* getCORBAComponent(const char* name)
	    throw (maciErrType::CannotGetComponentExImpl)
        {
            return (CORBA::Object*)NULL;
        }
        
        CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
	    throw(maciErrType::IncompleteComponentSpecExImpl, 
		  maciErrType::InvalidComponentSpecExImpl, 
		  maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
		  maciErrType::CannotGetComponentExImpl)
        {
            return (CORBA::Object*)NULL;
        }
        
        CORBA::Object* getCORBADefaultComponent(const char* idlType)
	    throw (maciErrType::NoDefaultComponentExImpl, 
		   maciErrType::CannotGetComponentExImpl)
        {
            return (CORBA::Object*)NULL;
        }

        CORBA::Object* getCORBACollocatedComponent(maci::ComponentSpec, bool, const char*)
	    throw(maciErrType::IncompleteComponentSpecExImpl, 
		  maciErrType::InvalidComponentSpecExImpl, 
		  maciErrType::ComponentSpecIncompatibleWithActiveComponentExImpl, 
		  maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)NULL;
	}

        CORBA::Object* getCORBAComponentNonSticky(const char*)
	   throw (maciErrType::CannotGetComponentExImpl)
	{
	    return (CORBA::Object*)NULL;
	}
        


    public:
    
        maci::ComponentInfo getComponentDescriptor(const char* componentName)
          throw (acsErrTypeContainerServices::GettingCompInfoExImpl)
          {
            maci::ComponentInfo temp;
            return temp;
          }
        
        ACE_CString_Vector findComponents(const char *nameWilcard, const char *typeWildcard)
        {
          return ACE_CString_Vector();
        }
    
        void releaseComponent(const char *name)
	    throw (maciErrType::CannotReleaseComponentExImpl)  
	{}
      
        void releaseAllComponents(){}
    
        CDB::DAL_ptr getCDB()
	    throw (acsErrTypeContainerServices::CanNotGetCDBExImpl)
        {
	    ACE_TCHAR corbalocRef[230];
	    ACE_TCHAR * envRef = ACE_OS::getenv ("DAL_REFERENCE");

	    if (envRef && *envRef)
		{
		ACS_LOG(0, "TestContainerServices::getCDB",
			(LM_INFO, "CDB obtained via environment: '%s'", envRef));
		strcpy(corbalocRef, envRef);
		}
	    else
		{
	    // corbaloc::<hostname>:<port>/CDB
	    const char* hostname = 0;
	    hostname = ACSPorts::getIP();
	    if (hostname==0)
		return (CDB::DAL *)0;
	    
	    ACE_TCHAR corbalocRef[230];
	    ACE_OS::sprintf(corbalocRef, "corbaloc::%s:%s/CDB", hostname, ACSPorts::getCDBPort().c_str());
	    
	    ACS_LOG(0, "TestContainerServices::getCDB",
		    (LM_INFO, "CDB reference generated using localhost address: '%s'", corbalocRef));
		}//if-else
	    
	    CDB::DAL_var dalObj = CDB::DAL::_nil();
	    CORBA::Object_var obj = m_orb->string_to_object(corbalocRef);
	    
	    if (!CORBA::is_nil(obj.in()))
		{
		dalObj = CDB::DAL::_narrow(obj.in());
		if (CORBA::is_nil(dalObj.in())) 
		    {
		    ACS_SHORT_LOG((LM_INFO, "TestContainerServices::getCDB() - Failed to narrow CDB"));
		    return (CDB::DAL *)0;
		    }
		}
	    
	    return dalObj._retn();
        }
        
        PortableServer::POA_var getOffShootPOA()
        {
          return NULL;
        }
    
        ACS::OffShoot_ptr activateOffShoot(PortableServer::Servant cbServant)
        {
          return NULL;
        }
      
        void deactivateOffShoot(PortableServer::Servant cbServant)
          throw (
           acsErrTypeContainerServices::OffShootDeactivationExImpl,
           acsErrTypeContainerServices::OffShootPOAExImpl){}
      
        PortableServer::POA_var createOffShootPOA()
        {
          return NULL;
        }
        
        maci::ComponentStateManager* getComponentStateManager()
        {
          return NULL;
        }
        acsalarm::AlarmSource* getAlarmSource()
        {
          return NULL;
        }
};// class TestContainerServices

/****************************************************************************************/

bool EPshutting_down = false;
LoggingProxy *loggerProxy=0;

/*
 * Thread initializer and thread done functions
 *
 */

void initThread(const char * threadName)
{
    ACS_CHECK_LOGGER;
    LoggingProxy::init(loggerProxy);
    LoggingProxy::ThreadName(threadName);
}//initThread

void doneThread()
{
    LoggingProxy::done();
}//doneThread

/******************************************************************************************/

void TerminationSignalHandler(int)
{
    if (EPshutting_down) return;
    EPshutting_down=true;
    
    // initialize logger (this is a new thread)
    LoggingProxy logProx(0, 0, 31, 0);
    LoggingProxy::init (&logProx);

    ACS_LOG(0, "termination", (LM_INFO, "Termination signal detected."));
    
    
    try
	{
	// false - avoid deadlock; true would try to wait for all requests
	// to complete before returning, but because we are calling it from within
	// a request, we would be blocking it from 
	  BACI_CORBA::getORB()->shutdown(false);
	  
	}
    catch( CORBA::Exception &ex )
	{
	ACE_PRINT_EXCEPTION( ex, "TerminationSignalHandler");
	}
}


int main(int l_argc, char* l_argv[]) 
{
    // create logging proxy 
    loggerProxy = new  LoggingProxy(0, 0, 31, 0);
    LoggingProxy::init (loggerProxy);
    LoggingProxy::ProcessName(l_argv[0]);
    ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !"));


    ACS_SHORT_LOG((LM_INFO, "enumpropTestServer: Starting"));
    if (l_argc < 2) {
       ACS_SHORT_LOG((LM_INFO,"Usage: enumpropTestServer <device_name>"));
      return -1;
    }

    // Installs termination signal handlers, to be able
    // to terminate cleanly
    ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Installing signal handlers"));
    ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
    ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

    int   devCount = l_argc-1;

    CORBA::String_var ior;
    enumpropTestDeviceImpl *ep_servant = NULL;
    ENUMPROP_TEST::enumpropTestDevice_var ep;

    int count = 0;
    char fileName[64];
    FILE *IOR_file;
 
    try {

    BACIThread::setInitializers(initThread, doneThread);

    // Initialize the ORB.
    BACI_CORBA::InitCORBA(l_argc, l_argv);

    if (!ACSError::init (BACI_CORBA::getORB()))
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baciTestServer",
		(LM_INFO, "Failed to initialize ACSError."));
	return -1;
	}

#ifndef MAKE_VXWORKS
	// Init the Alarm factory      
    ACSAlarmSystemInterfaceFactory::init(maci::Manager::_nil());
#endif


    ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Init DB"));

    DBConnector::initDB(":Appl_data:alma");

    // Instantiate the CBDPropertySet singleton
    if (DBConnector::getDBTable())
	CDBPropertySet::createInstance(BACI_CORBA::getORB(), 
				       BACI_CORBA::getPOAManager(),
				       BACI_CORBA::getPOARoot());


    for (int n=1; n <= devCount; n++)
	{
 
	ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Exporting \"%s\"", l_argv[n]));
    
    // Build the implementation of the containerServices to pass to the constructor 
    // of the component (the component will free its memory)
    ACE_CString name(l_argv[n]);
    TestContainerServices* pCS = new TestContainerServices(name,BACI_CORBA::getPOA(),BACI_CORBA::getORB());
	ep_servant = new enumpropTestDeviceImpl(name,pCS);
	ACS_SHORT_LOG((LM_INFO, "enumpropTestDevice: %s created", l_argv[n]));
	ep = ep_servant->_this();
	ep_servant->_remove_ref();
	 
	ior = BACI_CORBA::getORB()->object_to_string (ep.ptr());
	
 
	ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Writing IOR for %s into file...", l_argv[n]));
	sprintf(fileName, "%s.ior", name.c_str());
	IOR_file = ACE_OS::fopen (fileName, "w");
	if (IOR_file == 0) 
    {
	   ACS_SHORT_LOG ((LM_ERROR,
		  "Cannot open output files for writing IOR: %s", fileName));
	   return  -1;
	}
	
	int result = ACE_OS::fprintf (IOR_file, "%s",  ior.in());
	if (result < 0)
	    {
	    ACS_SHORT_LOG ((LM_ERROR,
			    "ACE_OS::fprintf failed while writing %s into %s\n", ior.in(), fileName));
	    return  -1;
	    }
	ACE_OS::fclose (IOR_file);
	ACS_SHORT_LOG((LM_INFO, "enumpropTestDevice: %s exported", l_argv[n]));

	count++;

//	PortableServer::ObjectId_var oid = BACI_CORBA::getPOA()->reference_to_id(ep.ptr());
//	BACI_CORBA::getPOA()->deactivate_object(oid.in());
	

	}//for

    ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Waiting for requests ... "));
    BACI_CORBA::getORB()->run();

    ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Exiting."));
    
    ep_servant->_remove_ref(); 
 

    ACS_SHORT_LOG((LM_INFO,"enumpropTestServer: Releasing CORBA."));

    BACI_CORBA::DoneCORBA();
    
    DBConnector::closeDB();
    
    //delete t;

    
    LoggingProxy::done();
    delete loggerProxy;
    
    std::cout << std::flush;  // shold be done by: LoggingProxy::done()

    return 0;  

    }catch(CORBA::Exception &ex) {
      ACS_SHORT_LOG((LM_INFO, "enumpropTestServer: Exceptions raised while creating Component"));
      return -1;
    }
    return  0;
}     


#ifdef MAKE_VXWORKS

int startEnumpropTestServer (char *szCmdLn)
{
  int  l_argc;
  char *l_argv[100];

//  ACE_MAIN_OBJECT_MANAGER;

  l_argc = argUnpack(szCmdLn, l_argv);
  l_argv[0] = "enumpropTestServer";

  return ace_main_i(l_argc, l_argv);
}
#endif













