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
* "@(#) $Id: acsLogSvc.cpp,v 1.22 2009/06/09 00:03:30 javarias Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* gchiozzi 2003-04-03 Fixed ACE_OS::string_to_argv for new ACE 
* bjeram 2002-04-10 added -silent command line options
* bjeram 2002-04-10 added terminal signal handler
* bjeram 2002-04-10 added -o options whic write ior also to the file
* bjeram 2002-04-10 added registration with Naming Service as ACSLogSvc
* bjeram 2001-09-20 change of resolving Log Svc
* bjeram 2001-09-11 created
*/

static char *rcsId="@(#) $Id: acsLogSvc.cpp,v 1.22 2009/06/09 00:03:30 javarias Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include <acslogSvcImpl.h>
#include <orbsvcs/CosNamingC.h>
#include <tao/IORTable/IORTable.h>
#include <maciHelper.h>
#include <acserr.h>
#include <acsutilPorts.h>

CORBA::ORB_var orb;

void TerminationSignalHandler(int)
{
    ACS_SHORT_LOG((LM_INFO, "ACSLogSvc is going down (like Bobby Brown :-))  ...  "));
    orb->shutdown (true);
}//TerminationSignalHandler





int main(int argc, char *argv[])
{
 
  CosNaming::NamingContext_var naming_context; 
  int  nargc=0;
  char **nargv=0;
  const char *hn=ACSPorts::getIP();
  ACE_CString iorFile;

 if (argc>=2 && !ACE_OS_String::strcmp(argv[1], "-?")){
     ACE_OS::printf ("USAGE: acsLogSvc  [-ORBInitRef NameService=iiop://yyy:xxxx/NameService] [-ORBEndpoint iiop://ip:port] [-o iorfile] [-silent]\n");
     return -1;
  }
 
 // here we have to unset ACS_LOG_CENTRAL that we prevent filtering of log messages
 char *logCent=getenv("ACS_LOG_CENTRAL");
 if (logCent)
     {
     unsetenv("ACS_LOG_CENTRAL");
     printf("Unset ACS_LOG_CENTRAL which was previously set to %s\n", logCent);
     }//if

// create logging proxy
  LoggingProxy::ProcessName(argv[0]);
  ACE_Log_Msg::instance()->local_host(hn);
  LoggingProxy m_logger (0, 0, 31, 0);
  LoggingProxy::init (&m_logger);  
  ACS_SHORT_LOG((LM_INFO, "Logging proxy successfully created !"));

  ACE_CString argStr;
 
  for(int i=1; i<argc; i++)
      {
      argStr += argv[i];
      argStr += " ";

      if (!ACE_OS_String::strcmp(argv[i], "-o") && (i+1)<argc)
	  {
	  iorFile = argv[i+1];
	  }//if
      }//for

  if (argStr.find ("-ORBEndpoint")==ACE_CString::npos)
      {
      argStr = argStr + "-ORBEndpoint iiop://" + hn + ":" + ACSPorts::getLogPort().c_str();
      }

  ACS_SHORT_LOG((LM_INFO, "New command line is: %s", argStr.c_str()));

  ACE_OS::string_to_argv ((ACE_TCHAR*)argStr.c_str(), nargc, nargv);
 
  ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
  ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

  try
    {
      // Initialize the ORB
      ACE_OS::printf ("Initialising ORB ... \n"); 
      orb = CORBA::ORB_init (nargc, nargv, 0);
      ACE_OS::printf ("ORB initialsed !\n");
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "Failed to initalise ORB");
      return -1;
    }


  if (!ACSError::init(orb.in()))
      {
      ACS_SHORT_LOG ((LM_ERROR, "Failed to initalise the ACS Error System"));
      return -1;
      }

 // resolve naming service
  try
      {
      ACS_SHORT_LOG((LM_INFO, "Trying to connect to the Naming Service ...."));
      CORBA::Object_var naming_obj = orb->resolve_initial_references ("NameService");

      if (!CORBA::is_nil (naming_obj.in ()))
	  {
	  naming_context = CosNaming::NamingContext::_narrow (naming_obj.in ());
	  ACS_SHORT_LOG((LM_INFO, "Connected to the Name Service"));
	  }
      else
	  {
	  ACS_SHORT_LOG((LM_ERROR, "Could not connect the Name Service!"));
	  return -1;
	  }//if-else
      }
  catch( CORBA::Exception &_ex )
      {
      ACS_SHORT_LOG((LM_ERROR, "Could not connect the Name Service!"));
      return -1;
      }


  // logging service
  try
    {
      CORBA::Object_var log_obj;
/* 
      if (argStr.find ("-ORBInitRef NameService=")!=ACE_CString::npos)
	{
	// Initialize the ORB
	ACE_OS::printf ("Initialising ORB ... \n"); 
	orb = CORBA::ORB_init (nargc, nargv, 0);
	ACE_OS::printf ("ORB initialsed !\n");
        //Naming Service 
	ACE_OS::printf ("Resolving  Naming service ... \n");
	CORBA::Object_var naming_obj =
	  orb->resolve_initial_references ("NameService");
	if (!CORBA::is_nil (naming_obj.in ()))
	  {
	    CosNaming::NamingContext_var naming_context =
	      CosNaming::NamingContext::_narrow (naming_obj.in ());
	    ACE_OS::printf ( "Naming Service resolved !\n");
*/	    
	
    ACE_OS::printf ( "Resolving Logging Service from Naming service ...\n");
    CosNaming::Name name;
    name.length(1);
    name[0].id = CORBA::string_dup("Log");
    log_obj = naming_context->resolve(name);
/*	
  }
	else
	  {
	    ACS_LOG(LM_SOURCE_INFO, "main", (LM_ERROR,  "Failed to initialise the Name Service!"));
	  }
	}//if naming Service

      else
	{
	ACE_OS::printf ("Getting Log from the Manager... \n");
	CORBA::ULong status;
	maci::Manager_ptr manager = maci::MACIHelper::resolveManager (orb.in(), nargc, nargv, 0, 0);


	log_obj = manager->get_COB(0, "Log", true,  status);
	}
*/
      if (!CORBA::is_nil (log_obj.in()))
	  {
	  Logging::AcsLogService_var logger = Logging::AcsLogService::_narrow(log_obj.in());
	  ACE_OS::printf ( "Logging Service resolved !\n");
	  
	  m_logger.setCentralizedLogger(logger.in());
	  }
      else
	  {
	  ACS_LOG(LM_SOURCE_INFO, "main", (LM_ERROR, "Failed to initialise the Logging Service!"));
	  return -1;
	  }//if-else
    }
  catch( CORBA::Exception &__ex )
    {
      ACE_PRINT_EXCEPTION(__ex, "Failed to get and set the centralized logger");
      return -1;
    }


  try
    {
    //Get a reference to the RootPOA
    ACE_OS::printf("Creating POA ... \n");
      CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
      
      PortableServer::POA_var root_poa = PortableServer::POA::_narrow(obj.in());
      
      
      PortableServer::POAManager_var poa_manager = root_poa->the_POAManager();
      

      CORBA::PolicyList policy_list;
      policy_list.length(5);
      policy_list[0] = root_poa->create_request_processing_policy (PortableServer::USE_DEFAULT_SERVANT);
      
      policy_list[1] =  root_poa->create_id_uniqueness_policy (PortableServer::MULTIPLE_ID);
      
      policy_list[2] = root_poa->create_id_assignment_policy(PortableServer::USER_ID); 
      
      policy_list[3] = root_poa->create_servant_retention_policy(PortableServer::NON_RETAIN); 
      
      policy_list[4] =  root_poa->create_lifespan_policy (PortableServer::PERSISTENT);
      
      printf("policies are created !\n");

      PortableServer::POA_var poa = root_poa->create_POA("ACSLogSvc", poa_manager.in(), policy_list);
      
      ACE_OS::printf("POA created !\n");

      for (CORBA::ULong i = 0; i < policy_list.length (); ++i) {
	CORBA::Policy_ptr policy = policy_list[i];
	policy->destroy ();
	
      }
      printf("Policies are destroyed !\n");

      ACSLogImpl acsLogSvc (m_logger);
      poa->set_servant (&acsLogSvc);
      

      PortableServer::ObjectId_var oid = PortableServer::string_to_ObjectId ("ACSLogSvc");
      obj = poa->create_reference_with_id (oid.in(),  acsLogSvc._interface_repository_id());
      

      CORBA::String_var ior = orb->object_to_string (obj.in());
      

      // if ther is file name write ior in it
      if (iorFile.length()!=0)
	  {
	  FILE *output_file = ACE_OS::fopen (iorFile.c_str(), "w");
	  if (output_file == 0) 
	      {
	      ACS_SHORT_LOG ((LM_ERROR,
				 "Cannot open output files for writing IOR: ior.ior"));
	      return  -1;
	      }//if

	  int result = ACE_OS::fprintf (output_file, "%s", ior.in());
	  if (result < 0) 
	      {
	      ACS_SHORT_LOG ((LM_ERROR,
				 "ACE_OS::fprintf failed while writing %s to ior.ior", ior.in()));
	      return  -1;
	      }//if
	  ACE_OS::fclose (output_file);
	  ACS_SHORT_LOG((LM_INFO, "ACSLogSvc's IOR has been written into: %s", iorFile.c_str()));
	  }//if


// adding ACSLog to NamingService
      if (!CORBA::is_nil (naming_context.in ()))
	  {
	  // register cdb server in Naming service
	  CosNaming::Name name (1);
	  name.length (1);
	  name[0].id = CORBA::string_dup ("ACSLogSvc");
	  naming_context->rebind (name, obj.in ());
	  ACS_SHORT_LOG((LM_INFO, "ACSLogSvc service registered with Naming Services")); 
	  }//if

      CORBA::Object_var table_object =
        orb->resolve_initial_references ("IORTable");
      
      IORTable::Table_var adapter =
        IORTable::Table::_narrow (table_object.in ());
      
      if (CORBA::is_nil (adapter.in ()))
        {
          ACS_SHORT_LOG ((LM_ERROR, "Nil IORTable"));
        }
      else
        {
          adapter->bind ("ACSLogSvc", ior.in ());
        }

      poa_manager->activate ();
      

      ACS_SHORT_LOG((LM_INFO, "ACSLogSvc is waiting for incoming log messages ..."));
      if (argStr.find ("-ORBEndpoint")!=ACE_CString::npos)
	  m_logger.setStdio(31);
      orb->run ();
      
      ACSError::done();
      LoggingProxy::done();
    }
  catch( CORBA::Exception &ex )
    {
      ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
    }
  
  return 0;
}













