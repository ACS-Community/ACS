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
* "@(#) $Id: acserrTestCheckDepth.cpp,v 1.39 2005/09/21 08:53:00 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-01-23 added initialization of logging system since its initialization is taken out of ACSError::init
* rlemke   01/09/01   created  
*/

static char *rcsId="@(#) $Id: acserrTestCheckDepth.cpp,v 1.39 2005/09/21 08:53:00 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrTestC.h"
#include "acserr.h"
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include "acserrTest.h"

int main(int argc, char *argv[])
{

  if (argc<4){
    ACE_OS::printf ("usage: testClient <server_name> <depth> <isError> [iteration]\n");
    return -1;
  }//if

  

  // create logging proxy
  LoggingProxy m_logger (0, 0, 31, 0);
  LoggingProxy::init (&m_logger); 

  CORBA::ORB_var orb;
  ACS_TEST_INIT_CORBA;

  // init ACS error system
  ACSError::init (orb.ptr());


  /**************************************/
  acserrTest_var test;
  int depth;
  sscanf (argv[2], "%d", &depth);
  bool isErr = *argv[3]-'0';
  int iteration=1, i=1, idepth = 1;
  if (argc>4)
	sscanf (argv[4], "%d", &iteration); 
  
  ACS_DEBUG("main", "****** Check Depth of Stack *****");
 
  try
  {
    ACS_DEBUG("acserrTestClient", "Getting object reference ... ");
    char fileName[64];
    sprintf(fileName, "file://%s.ior", argv[1]);
    CORBA::Object_var testObj = orb->string_to_object (fileName);

    ACS_DEBUG("acserrTestClient", "Narrowing it .... ");
    test = acserrTest::_narrow (testObj.in());

    while ( depth >= idepth ) {
      ACS_SHORT_LOG((LM_INFO, "checking depth (%d/%d)", idepth, depth));
      while( iteration >= i ){
        ACS_SHORT_LOG((LM_INFO, "Performing (remote call)... (%d/%d)", i, iteration));
        test->test (idepth, isErr);
        i++;
      }
      i = 1;
      idepth++;
    }
    ACS_SHORT_LOG((LM_INFO, "Test performed."));

    test->shutdown ();
    ACE_OS::sleep(5);
  }
  catch( CORBA::Exception &ex )
  {    
    ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
    return -1;
  }

  LoggingProxy::done();

  return 0;
}




