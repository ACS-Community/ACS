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
* "@(#) $Id: acserrTestClient.cpp,v 1.53 2011/03/24 16:53:35 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-01-23 added initialization of logging system since its initialization is taken out of ACSError::init
* gchiozzi 2002-01-18 Added sleep before terminaiton, to give time for shutdown to the server
* almamgr  20/06/01  created
* rlemke   30/08/01  integrated into tat 
*/

static char *rcsId="@(#) $Id: acserrTestClient.cpp,v 1.53 2011/03/24 16:53:35 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrTestC.h"
#include "acserr.h"
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/orbsvcs/DsLogAdminC.h>
#include "acserrTest.h"
#include "ACSErrTypeTest.h"

using namespace ACSErrTypeTest;



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
  int iteration=1, i=1;
  const int size = 20;  // max value 1.84 x 10^19
  char printBuf[size+1];

  if (argc>4)
	sscanf (argv[4], "%d", &iteration); 
  
  ACS_DEBUG("main", "****** Test Block *****");
 
  try
  {
    ACS_DEBUG("acserrTestClient", "Getting object reference ... ");
    char fileName[64];
    sprintf(fileName, "file://%s.ior", argv[1]);
    CORBA::Object_var testObj = orb->string_to_object (fileName);

    ACS_DEBUG("acserrTestClient", "Narrowing it .... ");
    test = acserrTest::_narrow (testObj.in());

    unsigned long long numToPrint; 
    while( iteration >= i )
    {
      ACS_SHORT_LOG((LM_INFO, "Performing test1 (remote call)... (%d/%d)", i, iteration));
      ACSErr::ErrorTrace *corbaErrorTrace=0;
      ErrorTraceHelper *errorTrace=0;
      // here is also test for converting Completion_var to CompletionImpl
      CompletionImpl comp;
      ACSErr::Completion_var tc = test->test (depth, isErr);

      comp = tc;


      if (comp.isErrorFree())
      {
        ACS_SHORT_LOG((LM_INFO, "Completion does not contain an error trace"));
        comp.log();
        return 0;
      }

      errorTrace = comp.getErrorTraceHelper();
      ACS_SHORT_LOG((LM_INFO, "Stack depth: %d", errorTrace->getDepth()));
      comp.log();
      ACE_OS::printf( "%s", errorTrace->toString().c_str() );
	
      corbaErrorTrace = &(errorTrace->getErrorTrace());

      do
      {
	  ACS_SHORT_LOG((LM_INFO, "FileName:   \"%s\"",errorTrace->getFileName()));
	  ACS_SHORT_LOG((LM_INFO, "LineNumber: \"%d\"",errorTrace->getLineNumber()));  
	  ACS_SHORT_LOG((LM_INFO, "Routine:    \"%s\"",errorTrace->getRoutine()));
          ACS_SHORT_LOG((LM_INFO, "HostName:   \"%s\"",errorTrace->getHostName()));
	  ACS_SHORT_LOG((LM_INFO, "Process:    \"%s\"",errorTrace->getProcessName()));
          ACS_SHORT_LOG((LM_INFO, "Thread:     \"%s\"",errorTrace->getThread()));
    
          for (int ii = 0; ii < size; ii++) printBuf[ii] = ' ';
   
          printBuf[size] = '\0';
          numToPrint = errorTrace->getTimeStamp(); 
   
          for (int ii = size - 1; ii >= 0; ii--) {
                 printBuf[ii] = numToPrint % 10 + '0';
                 numToPrint /= 10;
                 if (numToPrint == 0)
                  break;
          }
	  ACS_SHORT_LOG((LM_INFO, "TimeStamp:  \"%s\"",printBuf));
	  ACS_SHORT_LOG((LM_INFO, "ErrorType:  \"%d\"",errorTrace->getErrorType()));
	  ACS_SHORT_LOG((LM_INFO, "ErrorCode:  \"%d\"",errorTrace->getErrorCode()));
	  ACS_SHORT_LOG((LM_INFO, "Severity:   \"%d\"", errorTrace->getSeverity()));
          ACS_SHORT_LOG((LM_INFO, "Description: \"%s\"",errorTrace->getDescription()));
      } while (errorTrace->getNext()!=NULL);	

      i++;
    } // while iterator >= i
       
  }
  catch( CORBA::Exception &ex )
  {    
    ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
    return -1;
  }
  ACS_SHORT_LOG((LM_INFO, "Test1 performed."));

//test2
  i=1;
 
  while (i<=iteration)
  {
    try
    {
      ACS_SHORT_LOG((LM_INFO, 
              "Performing test2 (remote call - exceptions) ... (%d/%d)", i, iteration));
      test->testExceptions (depth, isErr);
    }
    catch (ACSErr::ACSException &acse)
    {
	ACS_SHORT_LOG((LM_INFO, "Catch ACSException !"));
	ACSError exception (acse); // put CORBA exception (ACSException) into ACSError wrapper
	exception.log();
    }
    catch (ACSErrTypeTest::ACSErrTest0Ex &_ex)
    {
      ACS_SHORT_LOG((LM_INFO, "Catch ACSErrTest0Ex !"));
      ACSErrTest0ExImpl exorg(_ex);
      ACE_CString buf = exorg.getMember3();
      ACS_SHORT_LOG((LM_INFO, "Members of the caught exception are: %d, %f, %s, %d", 
            	       exorg.getMember1(),
		       exorg.getMember2(),
		       buf.c_str(), exorg.getMember4()));
	
      ACSErrTest0ExImpl *ex = new ACSErrTest0ExImpl(_ex, __FILE__, __LINE__, "testClient::main");
      ex->log();
//	ACSErrTest0Completion c(_ex.errorTrace,  __FILE__, __LINE__, "testClient::main-convertion");
//	c.log();
	delete ex;
    }
    catch(CORBA::Exception &__ex)
    {    
      ACE_PRINT_EXCEPTION (__ex, "EXCEPTION CAUGHT");
      return -1;
    }
    
    i++;
  }//while

  ACS_SHORT_LOG((LM_INFO, "Test2 performed."));

// test3 (no error)
  i=1;
  while (i<=iteration)
  {
    try 
    {
      ACS_SHORT_LOG((LM_INFO, 
                     "Performing test3 (no error) ... (%d/%d)", i, iteration));
      CompletionImpl comp = test->testNoError ();
      comp.log();
      // test CompletionImpl copy constructor where there is no error
      CompletionImpl c1(comp);
      c1.log();
      
    }
    catch( CORBA::Exception &ex )
    {    
      ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
    }
    
    i++;
  }//while

  ACS_SHORT_LOG((LM_INFO, "Test3 performed."));

// test4 (default error and operator=)
  try
  {
    ACS_SHORT_LOG((LM_INFO, 
          "Performing test4 (default error and operator=)" ));
    CompletionImpl defaultComp = test->testDefaultError ();
    defaultComp.log();
    CompletionImpl OKComp = test->testNoError ();
    OKComp.log();
    defaultComp = OKComp;
    defaultComp.log();
  }
  catch( CORBA::Exception &ex )
  {    
    ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
  }
    
  ACS_SHORT_LOG((LM_INFO, "Test4 performed."));

// test5 ( error completion, assignment and copy constructor)
  try
  {
    ACS_SHORT_LOG((LM_INFO, "Performing test5" ));
    CompletionImpl comp = test->test(depth, isErr);
    comp.log();

    ACS_SHORT_LOG((LM_INFO, "Performing test5  - copy constructor" ));
 // test CompletionImpl copy constructor where there is no error
    CompletionImpl c1(comp);
    c1.log();

    ACS_SHORT_LOG((LM_INFO, "Performing test5  - assignment constructor" ));
    CompletionImpl OKComp = test->testNoError ();

    comp = OKComp;
    comp.log();
    
    comp = c1;
    comp.log();
  }
  catch( CORBA::Exception &ex )
  {    
    ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
  }
    
  ACS_SHORT_LOG((LM_INFO, "Test5 performed."));


// test 6 (completion out)
  try
  {
    ACS_SHORT_LOG((LM_INFO, "Performing test6 (completion out)" ));

    ACSErr::CompletionImpl comp;
    ACSErr::Completion_var c; // CORBA completion

    test->testCompletionOut(depth, isErr, c.out());
    
    comp = c;
    comp.log();
  }
  catch( CORBA::Exception &ex )
  {    
    ACE_PRINT_EXCEPTION (ex, "EXCEPTION CAUGHT");
      return -1;
  }
    
  ACS_SHORT_LOG((LM_INFO, "Test6 performed (completion out)."));



  test->shutdown();
  ACE_OS::sleep(5);
  LoggingProxy::done();

  return 0;
}




