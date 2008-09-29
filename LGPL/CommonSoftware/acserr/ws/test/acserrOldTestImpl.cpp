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
* "@(#) $Id: acserrOldTestImpl.cpp,v 1.7 2008/09/29 08:38:53 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2002-02-13 changed orb->shutdown()
* almamgr  20/06/01  created 
* rlemke   30/08/01  integrated into tat
*/

static char *rcsId="@(#) $Id: acserrOldTestImpl.cpp,v 1.7 2008/09/29 08:38:53 cparedes Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "acserrOldTestImpl.h"
#include "acserr.h"


extern CORBA::ORB_var orb;

acserrOldTestImpl::acserrOldTestImpl(acserrOldTest* dest, const char* sn)
{
  this->dest = dest;
  srvName = sn;
} 

ACSErr::ErrorTrace* acserrOldTestImpl::testNoError ()
{
#ifndef MAKE_VXWORKS
    ACSError *er = new ACS_ERROR();
#else
    // for some reason expansion of ACS_ERROR macro w/o arguments does not work for VxWorks 6.2
    // last comma in macro with variable list of parameters is not eaten if arg list is empty
    ACSError *er = new ACSError(__FILE__, __LINE__ );
#endif

  return er->returnErrorTrace();
}  

ACSErr::ErrorTrace * acserrOldTestImpl::test ( CORBA::Long depth, CORBA::Boolean err )
{
  this->depth = depth;
  ACSError *e = f1 (depth-1, err);
  ACSError *er = new ACS_ERROR(e, ACSErr::ACSErrOldTypeTest, ACSErr::ACSErrTest2, "acserrOldTestImpl::test", ACSErr::Alert);
//  ACE_OS::printf ("-------------------------------------------------------\n");
//  ACE_OS::printf ("Error log for %s: \n", srvName);
  er->log();
//  ACE_OS::printf ("-------------------------------------------------------\n");

  return er->returnErrorTrace();
}  

ACSError* acserrOldTestImpl::f1 (int depth, bool iserr){
  char errString[64];
  ACSError *er, *res;

  if (depth>0)
      {
      sprintf (errString, "error %d", depth);
      er = f1 (--depth, iserr) ;
      res = new ACS_ERROR(er, ACSErr::ACSErrOldTypeTest, ACSErr::ACSErrTest2, "acserrOldTestImpl::f1");
// old
//    res = ((er->isOK() && !iserr) ?   er : new ACS_ERROR(er, ACSErr::ACSErrTypeTest, ACSErr::ACSErrTest2, "acserrOldTestImpl::f1") );
      res->addData ("depth", depth);
      res->addData ("isErr", iserr ? "true" : "false");
      return res;
      }
  else
      {
      if (dest.in()!=NULL )
	  {
//      ACE_OS::printf ("Makeing remote call ... \n");
	  res = new ACSError (dest->test (this->depth, iserr), true);
	  res->addData ("remote call", "");
	  return res;
	  }
      else
	  {
	  res = (iserr ? new ACS_ERROR(ACSErr::ACSErrOldTypeTest, ACSErr::ACSErrTest1, "acserrOldTestImpl::f1") : new ACS_ERROR("acserrOldTestImpl::f1"));
	  res->addData ("End of call's chain", "");
	  return res;
	  }
      }
}
    
void acserrOldTestImpl::testExceptions ( CORBA::Long depth, CORBA::Boolean err)
{

  this->depth = depth;
  try
    {
      f2 (depth-1, err);
      
    }
  catch( ACSErr::ACSException &ex)
    {
     throw ACS_EXCEPTION(ex, ACSErr::ACSErrOldTypeTest, ACSErr::ACSErrTestException2, "acserrOldTestImpl::testException");
    }
}

void acserrOldTestImpl::f2(int depth, bool isErr){
  char errString[64];

 if (depth>0) {

     try
      {
	f2 (depth-1, isErr) ;
      }
    catch( ACSErr::ACSException &ex)
      {
	if (isErr) {
	  sprintf (errString, "exception %d", depth);
	  ACSError res = ACS_ERROR(ex, ACSErr::ACSErrOldTypeTest, ACSErr::ACSErrTestException1, "acserrOldTestImpl::f2");
	  res.addData ("depth", depth);
	  res.addData ("isErr", isErr ? "true" : "false");

          throw ACS_EXCEPTION(res);
	}
	else{
	  ACSError er(ex);	
          throw ACS_EXCEPTION(er);
	}//if-else  
      }
  }
 else{
   if (dest.in()!=NULL ){
//     ACE_OS::printf ("Makeing remote call ... \n");
       try
       {
	 dest->testExceptions (this->depth, isErr);
       }
     catch( ACSErr::ACSException &ex)
       {
	 ACSError res (ex);
	 res.addData ("remote call", "");
         throw ACS_EXCEPTION(res);
       }

   }else{
     if (isErr){
     //ACSError res = ACS_ERROR_BEGIN(ACSErr::ACSErrTypeTest, ACSErr::ACSErrTestException0, "acserrOldTestImpl::f2");
       ACSError res = ACS_ERROR(ACSErr::ACSErrOldTypeTest, ACSErr::ACSErrTestException0, "acserrOldTestImpl::f2");
       res.addData ("End of call's chain", "");
       throw ACS_EXCEPTION(res);
     }//if
   }
 }
}

void acserrOldTestImpl::shutdown ()
{
    ACS_SHORT_LOG((LM_INFO, "acserr TestImpl Shutdown")); 
    ACSError::done();
    orb->shutdown (false);
}

/*___oOo___*/






