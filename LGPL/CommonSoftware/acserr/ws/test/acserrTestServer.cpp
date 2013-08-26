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
* "@(#) $Id: acserrTestServer.cpp,v 1.45 2006/10/26 13:36:33 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-08-15 changed argUnpack.h to acsutilArgUnpack.h
* bjeram 2002-06-04 added  orb->shutdown(true); //wait until all requests have completed
* bjeram 2002-01-23 added initialization of logging system since its initialization is taken out of ACSError::init
* almamgr  20/06/01  created
* rlemke   30/08/01  integrated into tat 
*/

static char *rcsId="@(#) $Id: acserrTestServer.cpp,v 1.45 2006/10/26 13:36:33 bjeram Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrTestImpl.h"
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include "acserrTest.h"
#include <iostream>

CORBA::ORB_var orb;

void TerminationSignalHandler(int)
{
    ACE_OS::printf("Server goes down ...  \n");
//    ACSError::done();
    orb->shutdown (true);
}


#ifdef MAKE_VXWORKS
#	include "rebootLib.h"
#	include "acsutilArgUnpack.h"

int acserrTestServer (char *szCmdLn){
    ACE_OS_Object_Manager ace_os_object_manager;
    ACE_Object_Manager ace_object_manager;
  int  argc;
  char *argv[100];

  argc = argUnpack(szCmdLn, argv);
  argv[0] = "errorServer";
#else
  int acserrTestServer (int argc, char *argv[]){
#endif // defined( MAKE_VXWORKS )

  

  if (argc<2){
    ACE_OS::printf ("usage: errorServer <server_name> [destination_server_name] \n");
    return -1;
  }

  ACE_OS::signal(SIGINT,  TerminationSignalHandler);  // Ctrl+C
  ACE_OS::signal(SIGTERM, TerminationSignalHandler);  // termination request

  // create logging proxy
  LoggingProxy m_logger (0, 0, 31, 0);
  LoggingProxy::init (&m_logger); 

  // creating ORB
  ACS_TEST_INIT_CORBA;

  // init ACS error system (and inside also logging)
//  ACSError::init (orb.ptr());

  try
  {
      //Get a reference to the RootPOA
      CORBA::Object_var obj = orb->resolve_initial_references("RootPOA");
      
      PortableServer::POA_var root_poa = PortableServer::POA::_narrow(obj.in());
      
   
      PortableServer::POAManager_var poa_manager = root_poa->the_POAManager();
      

#ifdef MAKE_VXWORKS      
      ErrorTraceHelper::setProcessName (szCmdLn);
#else 
      char *buf;
      ACE_OS::argv_to_string (argv, buf);
      ACSError::setProcessName (buf); // = ErrorTraceHelper::setProcessName (buf);
      delete[] buf;
#endif      
      ACS_DEBUG ("errorServer", "Creating test object ...");
      acserrTest_var dest;
      
      if (argc>2){
	ACS_DEBUG ("errorServer", "Getting object reference ... ");
	char refName[64];
	sprintf(refName, "file://%s.ior", argv[2]);
	CORBA::Object_var destObj = orb->string_to_object (refName);
	

	ACS_DEBUG ("errorServer", "Narrowing it .... ");
	dest = acserrTest::_narrow (destObj.in());
      }//if

      acserrTestImpl  esTest (dest.in(), argv[1]);
      acserrTest_var testObj = esTest._this ();
      
           
      poa_manager->activate ();
      
      ACS_DEBUG ("errorServer","POA Manager -> activate");
      
      ACS_DEBUG_PARAM ("errorServer", "Writing ior to the file: %s .... ", argv[1]);
      char* ior =  orb->object_to_string (testObj.in());
      
      
      char fileName[64];
      sprintf(fileName, "%s.ior", argv[1]);
      FILE *output_file = ACE_OS::fopen (fileName, "w");
      if (output_file == 0) {
	ACS_SHORT_LOG((LM_ERROR,
			   "Cannot open output files for writing IOR: ior.ior"));
	return -1;
      }

      int result = ACE_OS::fprintf (output_file, "%s", ior);
      if (result < 0) {
	ACS_SHORT_LOG ((LM_ERROR,
			   "ACE_OS::fprintf failed while writing %s to ior.ior", ior));
	return  -1;
      }

      ACE_OS::fclose (output_file);
      
      ACS_DEBUG ("errorServer", "Waiting for requests ...");
      orb->run ();
      
  }
  catch( CORBA::Exception &ex )
  {
    ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
    return -1;
  }

//  orb->shutdown(true); //wait until all requests have completed

  LoggingProxy::done();
  std::cout << std::flush;
  sleep(3);
  return 0;
}//startErrorServer


#ifndef MAKE_VXWORKS
int main(int argc, char *argv[])
{
  return acserrTestServer (argc, argv); 
}
#endif






