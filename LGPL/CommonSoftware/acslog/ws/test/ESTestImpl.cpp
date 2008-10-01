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
* "@(#) $Id: ESTestImpl.cpp,v 1.17 2008/10/01 04:41:17 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr  20/06/01  created 
*/

static char *rcsId="@(#) $Id: ESTestImpl.cpp,v 1.17 2008/10/01 04:41:17 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "ESTestImpl.h"
#include "acserr.h"
#include "ACSErrTypeTest.h"

using namespace ACSErrTypeTest;

ESTestImpl::ESTestImpl(ESTest* dest, const char* sn){
  this->dest = dest;
  srvName = sn;
}

ACSErr::Completion * ESTestImpl::test ( CORBA::Long depth, CORBA::Boolean err )
{
  this->depth = depth;

 CompletionImpl *e = f1 (depth-1, err);

  ACSErrTest0Completion *comp= new ACSErrTest0Completion(e, __FILE__, __LINE__, "acserrTestImpl::test", ACSErr::Alert);
  comp->log(LM_DEBUG);
  
  return comp->returnCompletion();
}//test  

CompletionImpl* ESTestImpl::f1 (int depth, bool iserr)
{
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
      }//if-else
}//f1
    
void ESTestImpl::testExceptions ( CORBA::Long depth, CORBA::Boolean err)
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

	throw ex.getACSErrTest0Ex();
	}
}//testException

void ESTestImpl::f2(int depth, bool isErr)
{

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
/*___oOo___*/





