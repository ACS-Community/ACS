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
* "@(#) $Id: acserrTestImpl.cpp,v 1.56 2011/03/24 16:53:35 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-02-13 changed orb->shutdown()
* almamgr  20/06/01  created 
* rlemke   30/08/01  integrated into tat
*/

static char *rcsId="@(#) $Id: acserrTestImpl.cpp,v 1.56 2011/03/24 16:53:35 tstaig Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrTestImpl.h"
#include "ACSErrTypeTest.h"
#include "acserr.h"

using namespace ACSErrTypeTest;

extern CORBA::ORB_var orb;

acserrTestImpl::acserrTestImpl(acserrTest* dest, const char* sn){
  this->dest = dest;
  srvName = sn;
}

ACSErr::Completion * acserrTestImpl::testNoError (  )
{
    AUTO_TRACE("acserrTestImpl::testNoError");

    ACSErrTestOKCompletion *er = new ACSErrTestOKCompletion();
    return er->returnCompletion();
}//acserrTestImpl::testNoError 

ACSErr::Completion* acserrTestImpl::testDefaultError (  )
{
    AUTO_TRACE("acserrTestImpl::testDefaultError");

// here we create complation on stack but could be created on heap as well (see other examples)
    CompletionImpl er; //  CompletionImpl *er = new CompletionImpl();

// because er is allocated on stack and will be automatically deleted, returnCompletion should not delete it !!!
    return er.returnCompletion(false);  
}//acserrTestImpl::testDefaultError 

ACSErr::Completion * acserrTestImpl::test ( CORBA::Long depth, CORBA::Boolean err )
{
    AUTO_TRACE("acserrTestImpl::test");
  this->depth = depth;
  
  CompletionImpl *e = f1 (depth-1, err);

  // we could create comp on the stach as well
  ACSErrTest0Completion *comp= new ACSErrTest0Completion(e, __FILE__, __LINE__, "acserrTestImpl::test", ACSErr::Alert);
  comp->log(); // noramlly we do not log here (just for test purpose)
  
  return comp->returnCompletion();
}  

// Here we return CORBA completion as output parameter
void acserrTestImpl::testCompletionOut ( CORBA::Long depth, CORBA::Boolean err, ACSErr::Completion_out c ) 
{
    AUTO_TRACE("acserrTestImpl::testCompletionOut");
    CompletionImpl *e = f1 (depth-1, err);

    ACSErrTest0Completion comp(e, __FILE__, __LINE__, "acserrTestImpl::testCompletionOut");
    comp.log(); // noramlly we do not log here (just for test purpose)
    
    c = comp.outCompletion();
}//testCompletionOut


// error is reported as CORBA exception
// throw ACSErrTypeTest::ACSErrTest0Ex, ACSErr::ACSException
void acserrTestImpl::testExceptions ( CORBA::Long depth, CORBA::Boolean err)
{
  this->depth = depth;
  try
    {
      f2 (depth-1, err);
      
    }
  catch (ACSErr::ACSException &ex)
    {

       throw ACS_EXCEPTION(ex, ACSErr::ACSErrTypeTest, ACSErrTypeTest::ACSErrTest2, "acserrTestImpl::testException");
    }
  catch (ACSErrTest1ExImpl &_ex)
      {
      _ex.log();
      ACSErrTest0ExImpl ex(_ex, __FILE__,  __LINE__, "acserrTestImpl::testExceptions");
      ex.setMember1(3);
      ex.setMember2(3.33);
      ex.setMember3("stringMember");
      ex.setMember4(true);
//      char *strB = ex.getMember3();

      ACS_SHORT_LOG((LM_INFO, "Exception members are: %d | %d, %f, %s, %d", 
		     ex.getMember1(),
		     ex.getMemberValue<int>("Member1"),
		     ex.getMember2(),
		     ex.getMember3().c_str()/*strB*/,ex.getMember4()));
//      delete[] strB;
      throw ex.getACSErrTest0Ex();
      }// tr-catch
}//acserrTestImpl::testExceptions


CompletionImpl*  acserrTestImpl::f1 (int depth, bool iserr){
  char errString[64];
  char routine[32];
  CompletionImpl *er=0, *res=0;

  if (depth>0)
      {
      sprintf (routine, "acserrTestImpl::f1 (%d)", depth);
      sprintf (errString, "error %d", depth);
      er = f1 (--depth, iserr) ;
      res = new ACSErrTest2Completion(er, __FILE__, __LINE__, routine); 

      res->addData ("depth", depth);
      res->addData ("isErr", iserr ? "true" : "false");
      return res;
      }
  else
      {
      if (dest.in()!=NULL )
	  {
	  //res = new ACSError (dest->test (this->depth, iserr), true);
	  res->addData ("remote call", "");
	  return res;
	  }
      else
          {

	  res = new ACSErrTest1Completion(__FILE__, __LINE__, "acserrTestImpl::f1 (0)");
	  res->addData ("End of call's chain", "");
	  return res;
	  }
      }
}//f1
    
void acserrTestImpl::f2(int depth, bool isErr){
  char errString[64];

  if (depth>0) {

    try
    {
      f2 (depth-1, isErr) ;
    }
    catch (ACSErr::ACSException &ex)
    {

      if (isErr) 
      {
        sprintf (errString, "exception %d", depth);
        ACSError res = ACS_ERROR(ex, ACSErr::ACSErrTypeTest, ACSErrTypeTest::ACSErrTest1, "acserrTestImpl::f2");
        res.addData ("depth", depth);
        res.addData ("isErr", isErr ? "true" : "false");
      
        throw ACS_EXCEPTION(res); 
      }
      else
      {
        //ACSErrTest0ExImpl (__FILE__, __LINE__, "acserrTestImpl::f2");
        ACSError er(ex);	
        throw ACS_EXCEPTION(er);
      }//if-else  

    } // catch ACSException
    catch (ACSErrTest1ExImpl &ex)
    {

      char strDepth[3]; sprintf (strDepth, "%d", depth);
      ACE_CString routine =  "acserrTestImpl::f2 ("; routine+= strDepth; 
      routine+= ")";

      ACSErrTest1ExImpl res(ex, __FILE__,  __LINE__, routine.c_str());
      res.addData ("depth", depth);
      res.addData ("isErr", isErr ? "true" : "false");
      
      throw res;
  
    } // catch ACSErrTest1ExImpl
  }
  else
  {
    if (dest.in()!=NULL )
    {
      try
      {
        dest->testExceptions (this->depth, isErr);
      }
      catch (ACSErr::ACSException &ex)
      {
        ACSError res (ex);
        res.addData ("remote call", "");
	throw ACS_EXCEPTION(res);
      }
      
    }
    else
    {
      if (isErr)
      {
        char routine[32];
        sprintf (routine, "acserrTestImpl::f2 (%d)", depth);
        throw  ACSErrTest1ExImpl(__FILE__,  __LINE__, routine);
      }//if isErr
    } // else if dest.in()
  } //else if  depth
}//f2


void acserrTestImpl::shutdown ()
{
    ACS_TRACE("acserr TestImpl Shutdown"); 
    ACSError::done();
    orb->shutdown (false);
}//shutdown

/*___oOo___*/






