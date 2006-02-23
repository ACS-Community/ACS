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
* "@(#) $Id: acserrOldTestClient.cpp,v 1.4 2005/09/21 08:53:00 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-01-23 added initialization of logging system since its initialization is taken out of ACSError::init
* gchiozzi 2002-01-18 Added sleep before terminaiton, to give time for shutdown to the server
* almamgr  20/06/01  created
* rlemke   30/08/01  integrated into tat 
*/

static char *rcsId="@(#) $Id: acserrOldTestClient.cpp,v 1.4 2005/09/21 08:53:00 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrOldTestC.h"
#include "acserr.h"
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include "acserrOldTest.h"

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
  acserrOldTest_var test;
  int depth;
  sscanf (argv[2], "%d", &depth);
  bool isErr = *argv[3]-'0';
  int iteration=1, i=1;
  const int size = 20;  // max value 1.84 x 10^19
  char printBuf[size+1];

  if (argc>4)
	sscanf (argv[4], "%d", &iteration); 
  
  ACS_DEBUG("main", "****** Test Block *****");
 
  try
    {
      ACS_DEBUG("acserrOldTestClient", "Getting object reference ... ");
      char fileName[64];
      sprintf(fileName, "file://%s.ior", argv[1]);
      CORBA::Object_var testObj = orb->string_to_object (fileName);

      ACS_DEBUG("acserrOldTestClient", "Narrowing it .... ");
      test = acserrOldTest::_narrow (testObj.in());

      unsigned long long numToPrint; 
      while( iteration >= i ){
	ACS_SHORT_LOG((LM_INFO, "Performing test1 (remote call)... (%d/%d)", i, iteration));
	ACSErr::ErrorTrace *c = test->test (depth, isErr);
	
	ACSError error(c, true);
	ACS_SHORT_LOG((LM_INFO, "Stack depth: %d", error.getDepth()));
	error.log();

	//ACSError *error = new ACSError (c, true) -> new ACS_ERROR (c, true);
	while (c!=NULL){
	  ACS_SHORT_LOG((LM_INFO, "FileName:   \"%s\"",error.getFileName()));
	  ACS_SHORT_LOG((LM_INFO, "LineNumber: \"%d\"",error.getLineNumber()));  
	  ACS_SHORT_LOG((LM_INFO, "Routine:    \"%s\"",error.getRoutine()));
          ACS_SHORT_LOG((LM_INFO, "HostName:   \"%s\"",error.getHostName()));
	  ACS_SHORT_LOG((LM_INFO, "Process:    \"%s\"",error.getProcess()));
          ACS_SHORT_LOG((LM_INFO, "Thread:     \"%s\"",error.getThread()));
          for (int ii = 0; ii < size; ii++) printBuf[ii] = ' ';
          printBuf[size] = '\0';
          numToPrint = error.getTimeStamp(); 
          for (int ii = size - 1; ii >= 0; ii--) {
                 printBuf[ii] = numToPrint % 10 + '0';
                 numToPrint /= 10;
                 if (numToPrint == 0)
                  break;
             }
	  ACS_SHORT_LOG((LM_INFO, "TimeStamp:  \"%s\"",printBuf));
	  ACS_SHORT_LOG((LM_INFO, "ErrorType:  \"%d\"",error.getErrorType()));
	  ACS_SHORT_LOG((LM_INFO, "ErrorCode:  \"%d\"",error.getErrorCode()));
	  ACS_SHORT_LOG((LM_INFO, "Severity:   \"%d\"", error.getSeverity()));
          ACS_SHORT_LOG((LM_INFO, "Description: \"%s\"",error.getDescription()));
	  c = error.getNext();
	  }	
	i++;
      }//while
      
   
    }
  catch( CORBA::Exception &ex)
    {    
      ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
    }
  ACS_SHORT_LOG((LM_INFO, "Test1 performed."));

//test2
  i=1;
  while (i<=iteration){
    try
      {
	ACS_SHORT_LOG((LM_INFO, 
              "Performing test2 (remote call - exceptions) ... (%d/%d)", i, iteration));
	test->testExceptions (depth, isErr);
      }
    catch (ACSErr::ACSException &ex)
      {
	ACS_SHORT_LOG((LM_INFO, "Catch ACSException !"));
	ACSError exception (ex); // put CORBA exception (ACSException) into ACSError wrapper
	exception.log();
      }
    catch( CORBA::Exception &_ex)
      {    
	ACE_PRINT_EXCEPTION (_ex, "EXCEPTION CAUGHT");
	return -1;
      }
    
    i++;
  }//while

  ACS_SHORT_LOG((LM_INFO, "Test2 performed."));

// test3 (no error)
  i=1;
  while (i<=iteration){
    try
      {
	ACS_SHORT_LOG((LM_INFO, 
              "Performing test3 (no error) ... (%d/%d)", i, iteration));
	test->testNoError ();
      }
    catch( CORBA::Exception &_ex)
      {    
	ACE_PRINT_EXCEPTION (_ex, "EXCEPTION CAUGHT");
	return -1;
      }
    
    i++;
  }//while

  ACS_SHORT_LOG((LM_INFO, "Test3 performed."));

  test->shutdown();
  ACE_OS::sleep(5);
  LoggingProxy::done();

  return 0;
}




