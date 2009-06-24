/*******************************************************************************
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
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: errorServer.cpp,v 1.14 2009/06/24 15:19:51 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-08-15 changed argUnpack.h to acsutilArgUnpack.h
* almamgr  20/06/01  created
*/

static char *rcsId="@(#) $Id: errorServer.cpp,v 1.14 2009/06/24 15:19:51 javarias Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "ESTestImpl.h"
#include <orbsvcs/CosNamingC.h>
#include <logging_idlC.h>
#include <acsutilPorts.h>

#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"

int acslogErrorServer (char *szCmdLn){
  int  argc;
  char *argv[100];

  argc = argUnpack(szCmdLn, argv);
  argv[0] = " acslogErrorServer";
#else
  int acslogErrorServer (int argc, char *argv[]){
#endif // defined( MAKE_VXWORKS )

  CORBA::ORB_var orb;
  

  if (argc<2){
    ACE_OS::printf ("usage: acslogErrorServer <server_name> [destination_server_name] \n");
    return -1;
  }
  
  try
    {
      // Initialize the ORB
      ACS_DEBUG(" acslogErrorServer", "Initialising ORB ... ");
      orb = CORBA::ORB_init (argc, argv, 0);
      ACS_DEBUG ("acslogErrorServer", "ORB initialsed !");
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "Failed to initalise ORB");
      return -1;
    }

  // create logging proxy
  ACS_DEBUG (" acslogErrorServer", "Creating logging proxy ... ");
  LoggingProxy *m_logger = new LoggingProxy(0, 0, 31, 0);
  LoggingProxy::init (m_logger);  
  ACS_DEBUG (" acslogErrorServer", "Logging proxy successfully created !");

  try
    {

      //Naming Service 
      ACS_DEBUG (" acslogErrorServer", "Resolving  Naming service ... ");
      ACE_CString nameService;
      nameService +="corbaloc::";
      nameService += ACSPorts::getIP();
      nameService += ":"; 
      nameService += ACSPorts::getNamingServicePort().c_str();
      nameService += "/"; 
      nameService += acscommon::NAMING_SERVICE_NAME;

      CORBA::Object_var naming_obj = orb->string_to_object(nameService.c_str());
      if (!CORBA::is_nil (naming_obj.in ()))
	{
	  CosNaming::NamingContext_var naming_context =
	    CosNaming::NamingContext::_narrow (naming_obj.in ());
	  ACS_DEBUG ("acslogErrorServer", "Naming Service resolved !");

	  CosNaming::Name name;
	  name.length(1);
	  name[0].id = CORBA::string_dup("Log");
	  CORBA::Object_var log_obj = naming_context->resolve(name);

	  if (!CORBA::is_nil (log_obj.in()))
	    {
	      Logging::AcsLogService_var logger = Logging::AcsLogService::_narrow(log_obj.in());
	      ACS_DEBUG ("acslogErrorServer", "Logging Service resolved !");

	      m_logger->setCentralizedLogger(logger.in());
	    }
	  else
	    {
	      ACS_DEBUG ("acslogErrorServer", "Failed to initialise the Logging Service!");
	    }

	}
      else
	{
	  ACS_DEBUG ("acslogErrorServer", "Failed to initialise the NameService!");
	}//if-else
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION(ex, "Failed to get and set the centralized logger");
    }

  try
    {
      //Get a reference to the RootPOA
      CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
      
      PortableServer::POA_var root_poa = PortableServer::POA::_narrow(obj.in());
      
   
      PortableServer::POAManager_var poa_manager = root_poa->the_POAManager();
      
      /* 
      CORBA::PolicyList policy_list;
      policy_list.length(5);
      policy_list[0] = root_poa->create_request_processing_policy (PortableServer::USE_DEFAULT_SERVANT);
     policy_list[1] =  root_poa->create_id_uniqueness_policy (PortableServer::MULTIPLE_ID);
      policy_list[2] = root_poa->create_id_assignment_policy(PortableServer::USER_ID); 
      policy_list[3] = root_poa->create_servant_retention_policy(PortableServer::NON_RETAIN); 
      policy_list[4] =  root_poa->create_lifespan_policy (PortableServer::PERSISTENT);
      printf("policies are created !\n");

      PortableServer::POA_var poa = root_poa->create_POA("cdbSrv", poa_manager.in(), policy_list);
      printf("Poa created !\n");

      for (CORBA::ULong i = 0; i < policy_list.length (); ++i) {
	CORBA::Policy_ptr policy = policy_list[i];
	policy->destroy ();
      }
      printf("Policies are destructed !\n");
      */


#ifdef MAKE_VXWORKS      
      ACSError::processName (szCmdLn);
#else 
      char *buf;
      ACE_OS::argv_to_string (argv, buf);
      ACSError::processName (buf);
      delete[] buf;
#endif      
      ACS_DEBUG ("acslogErrorServer", "Creating test object ...");
      ESTest_var dest;
      
      if (argc>2){
	ACS_DEBUG ("acslogErrorServer", "Getting object reference ... ");
	char refName[64];
	sprintf(refName, "file://%s.ior", argv[2]);
	CORBA::Object_var destObj = orb->string_to_object (refName);
	

	ACS_DEBUG ("acslogErrorServer", "Narrowing it .... ");
	dest = ESTest::_narrow (destObj.in());
      }//if

      ESTestImpl  esTest (dest.in(), argv[1]);
      ESTest_var testObj = esTest._this ();
      
           
      poa_manager->activate ();
      
      ACS_DEBUG ("acslogErrorServer","POA Manager -> activate");
      
      ACS_DEBUG_PARAM ("acslogErrorServer", "Writing ior to the file: %s .... ", argv[1]);
      char* ior =  orb->object_to_string (testObj.in());
      
      
      char fileName[64];
      sprintf(fileName, "%s.ior", argv[1]);
      FILE *output_file = ACE_OS::fopen (fileName, "w");
      if (output_file == 0) {
	ACS_SHORT_LOG ((LM_ERROR,
			   "Cannot open output files for writing IOR: ior.ior"));
	return  -1;
      }

      int result = ACE_OS::fprintf (output_file, "%s", ior);
      if (result < 0) {
	ACS_SHORT_LOG ((LM_ERROR,
			   "ACE_OS::fprintf failed while writing %s to ior.ior\n", ior));
	return  -1;
      }

      ACE_OS::fclose (output_file);
      
      ACS_DEBUG ("acslogErrorServer", "Waiting for requests ...");
      orb->run ();
      
      ACS_DEBUG ("acslogErrorServer", "ORB -> run");
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
    }
  
  return 0;
}//startErrorServer


#ifndef MAKE_VXWORKS
int main(int argc, char *argv[])
{
  return acslogErrorServer (argc, argv); 
}
#endif














