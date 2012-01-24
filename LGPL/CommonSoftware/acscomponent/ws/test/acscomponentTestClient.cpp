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
* "@(#) $Id: acscomponentTestClient.cpp,v 1.6 2012/01/24 01:00:04 tstaig Exp $"
*
* who       when        what
* --------  --------    ----------------------------------------------
* rcirami   2005-09-26  created
*/

static char *rcsId="@(#) $Id: acscomponentTestClient.cpp,v 1.6 2012/01/24 01:00:04 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acscomponentTestImpl.h"
#include "acscomponentTestC.h"

#define ACS_TEST_INIT_CORBA \
{ \
   try \
    { \
      ACS_DEBUG("ACS_TEST_INIT_CORBA", "Initialising ORB ... "); \
      orb = CORBA::ORB_init (argc, argv, 0); \
      ACS_DEBUG ("ACS_TEST_INIT_CORBA", "ORB initialised !"); \
    } \
  catch( CORBA::Exception &ex) \
    { \
      ACE_PRINT_EXCEPTION (ex, "Failed to initalise ORB"); \
      return -1; \
    } \
}

#ifdef __CYGWIN__
extern __declspec( dllimport ) CORBA::ORB_var orb;
#else
extern CORBA::ORB_var orb;
#endif

int main(int argc, char *argv[])
{

  

  ACS_TEST_INIT_CORBA;

  ACS_DEBUG("main", "****** Test Block *****");
  
  try
    {
      ACS_DEBUG("acscomponentTestClient", "Getting object reference ... ");

      char fileName[64];
      sprintf(fileName, "file://%s.ior", "ACSCOMPONENTTEST1");
      CORBA::Object_var mytest = orb->string_to_object (fileName);

      ACS_DEBUG("acscomponentTestClient", "Narrowing it .... ");
      ACSCOMPONENT_TEST::ACSComponentTestClass_var mytestClient = ACSCOMPONENT_TEST::ACSComponentTestClass::_narrow (mytest.in());
      mytestClient->shutdown();
    }

  catch ( CORBA::Exception &_ex )
    {    
      ACE_PRINT_EXCEPTION (_ex, "EXCEPTION CAUGHT");
      return -1;
    }
  ACS_SHORT_LOG((LM_INFO, "Test performed."));
  
  orb->shutdown(1);
  orb->destroy();
  ACE_OS::sleep(5);

  return 0;
}



