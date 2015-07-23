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
* "@(#) $Id: acscomponentTestServer.cpp,v 1.21 2012/01/24 01:00:04 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rcirami 2002-09-24  created
*/
 
static char *rcsId="@(#) $Id: acscomponentTestServer.cpp,v 1.21 2012/01/24 01:00:04 tstaig Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <vltPort.h>
#include <acsutil.h> 

#include "acscomponentTestImpl.h"
#include "acscomponentTestC.h"
#include <maciC.h>

#define ACS_TEST_INIT_CORBA \
{ \
   try \
    { \
      ACS_DEBUG("ACS_TEST_INIT_CORBA", "Initialising ORB ... "); \
      orb = CORBA::ORB_init (argc, argv, 0); \
      ACS_DEBUG ("ACS_TEST_INIT_CORBA", "ORB initialised !"); \
    } \
  catch( CORBA::Exception &ex ) \
    { \
      ex._tao_print_exception("Failed to initalise ORB");	\
      return -1; \
    } \
}


// We need an implementation of the ContainerServices here because
// 1. the component fails if the pointer to the ContainerServices it receives
//    in the constructor is NULL
// 2. the smart pointer used to store the ContainerServices deletes all
//    the not-NULL instances of the Container Services
// This clla implements all the methods of the interface but does nothing
class TestContainerServices : public maci::ContainerServices {
 public:

    TestContainerServices(ACE_CString& compName, PortableServer::POA_ptr poa):
        maci::ContainerServices(compName,poa) {}
        
        virtual ~TestContainerServices() {}
    
        CORBA::Object* getCORBAComponent(const char* name)
	    throw (maciErrType::CannotGetComponentExImpl)
        {
            return (CORBA::Object*)NULL;
        }
        
        CORBA::Object* getCORBADynamicComponent(maci::ComponentSpec compSpec, bool markAsDefault)
	    throw (maciErrType::IncompleteComponentSpecExImpl, 
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
          return NULL;
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
};

#ifdef __CYGWIN__
extern __declspec( dllimport ) CORBA::ORB_var orb;
#else
extern CORBA::ORB_var orb;
#endif

void TerminationSignalHandler(int)
{
     try
	{
	// false - avoid deadlock; true would try to wait for all requests
	// to complete before returning, but because we are calling it from within
	// a request, we would be blocking it from 
	  orb->shutdown(false);
	  
	}
    catch( CORBA::Exception &ex )
	{
	ex._tao_print_exception("TerminationSignalHandler");
	}
}


#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"

int acscomponentTestServer (char *szCmdLn)
{
  int  argc;
  char *argv[100];

  argc = argUnpack(szCmdLn, argv);
  argv[0] = "acscomponentTestServer";
#else
int main(int argc, char* argv[]) 
{
#endif // defined( MAKE_VXWORKS )

 
  // creating ORB
  ACS_TEST_INIT_CORBA;


  try
    {
    ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
    ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

      //Get a reference to the RootPOA
      CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
      
      PortableServer::POA_var root_poa = PortableServer::POA::_narrow(obj.in());
      
   
      PortableServer::POAManager_var poa_manager = root_poa->the_POAManager();
      

      // The component will throw an exception if receives a NULL ContainerServices
      // in the constructor.
      // We pass a value only to avoid that i throws the exception. On the other 
      // hand the test doe not use the ContainerServices (remember: ContainerServices
      // does not yet exist at this point!!!!)
      ACE_CString compName("TEST");
      ACSComponentTestClassImpl mytestImpl(compName.c_str(), new TestContainerServices(compName,NULL));
      ACSCOMPONENT_TEST::ACSComponentTestClass_var mytest = mytestImpl._this ();;
      


      poa_manager->activate ();
      
      ACS_DEBUG ("acscomponentTestServer","POA Manager -> activate");

      ACS_DEBUG ("acscomponentTestServer", "Writing ior to the file: ACSCOMPONENTTEST1");
      char* ior =  orb->object_to_string (mytest.in());
      
      
      char fileName[64];
      sprintf(fileName, "%s.ior", "ACSCOMPONENTTEST1");
      FILE *output_file = ACE_OS::fopen (fileName, "w");
      if (output_file == 0) {
	ACS_SHORT_LOG((LM_ERROR, "Cannot open output files for writing IOR: ior.ior"));
	return -1;
      }

      int result = ACE_OS::fprintf (output_file, "%s", ior);
      if (result < 0) {
	ACE_ERROR_RETURN ((LM_ERROR, "ACE_OS::fprintf failed while writing %s to ior.ior", ior), -1);
      }

      ACE_OS::fclose (output_file);
      
      ACS_DEBUG ("acscomponentTestServer", "Waiting for requests ...");
      orb->run ();
      
    }

  catch( CORBA::Exception &_ex )
    {
      _ex._tao_print_exception("EXCEPTION CAUGHT");
      return -1;
    }

  //    orb->shutdown(true); //wait until all requests have completed

  return 0;


}






