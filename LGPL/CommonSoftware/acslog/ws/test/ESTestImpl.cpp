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
* "@(#) $Id: ESTestImpl.cpp,v 1.14 2005/09/21 14:17:24 vwang Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr  20/06/01  created 
*/

static char *rcsId="@(#) $Id: ESTestImpl.cpp,v 1.14 2005/09/21 14:17:24 vwang Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

#include "ESTestImpl.h"
#include "acserr.h"
#include "ACSErrTypeTest.h"

using namespace ACSErrTypeTest;

ESTestImpl::ESTestImpl(ESTest* dest, const char* sn){
  this->dest = dest;
  srvName = sn;
}

ACSErr::ErrorTrace * ESTestImpl::test ( CORBA::Long depth, CORBA::Boolean err )
    throw ( CORBA::SystemException)
{
  this->depth = depth;
  ACSError *er = f1 (depth-1, err);
  ACE_OS::printf ("-------------------------------------------------------\n");
  ACE_OS::printf ("Error log for %s: \n", srvName);
  er->log();
  ACE_OS::printf ("-------------------------------------------------------\n");

  return er->returnErrorTrace();
}  

ACSError* ESTestImpl::f1 (int depth, bool iserr){
  char errString[64];
  ACSError *er, *res;

  if (depth>0) {
    sprintf (errString, "error %d", depth);
    er = f1 (--depth, iserr) ;
    res = new ACS_ERROR(er, ACSErr::ACSErrTypeTest, ACSErrTest1, "ESTestImpl::f1");
    //res = ((er->isOK() && !iserr) ?   er : new ACS_ERROR(er, ACSErrTypeTest, 1, "ESTestImpl::f1") );
    sprintf (errString, "%d", depth);
    res->addData ("depth", errString);
    res->addData ("isErr", iserr ? "true" : "false");
    return res;
    //return ((er->isOK() && !iserr) ?   er : new ACSError (er, errString, 1, __FILE__, __LINE__) );
  }
  else{
    if (dest.in()!=NULL ){
      ACE_OS::printf ("Makeing remote call ... \n");
      res = new ACSError (dest->test (this->depth, iserr), true);
      res->addData ("remote call", "");
      return res;
    }
    else
    {
    res = new ACS_ERROR(ACSErr::ACSErrTypeTest, ACSErrTest0, "ESTestImpl::f1");
//      res = (iserr ? new ACS_ERROR_BEGIN("error 0", 1, "ESTestImpl::f1") : new ACS_NO_ERROR("ESTestImpl::f1"));
      res->addData ("End of call's chain", "");
      return res;
    }
  }
}
    
void ESTestImpl::testExceptions ( CORBA::Long depth, CORBA::Boolean err)
    throw ( CORBA::SystemException, ACSErr::ACSException )
{

  this->depth = depth;
  try
    {
      f2 (depth-1, err);
      
    }
  catch (ACSErr::ACSException &ex)
    {
      ACE_OS::printf("---------------------------------------------------\n");
      ACSError res = ACS_ERROR(ex, ACSErr::ACSErrTypeTest, ACSErrTest1, "ESTestImpl::testException");
      ACE_OS::printf ("Error log (exceptions) for %s: \n", srvName);
      res.log();
      ACE_OS::printf ("-------------------------------------------------------\n"); 
      throw ACS_EXCEPTION(res);
    }
}

void ESTestImpl::f2(int depth, bool isErr){
  char errString[64];

 if (depth>0) {

    try
      {
	f2 (depth-1, isErr) ;
      }
    catch (ACSErr::ACSException &ex)
      {
	if (isErr) {
	  sprintf (errString, "exception %d", depth);
	  ACSError res = ACS_ERROR(ex, ACSErr::ACSErrTypeTest, ACSErrTest1, "ESTestImpl::f2");
	  sprintf (errString, "%d", depth);
	  res.addData ("depth", errString);
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
     ACE_OS::printf ("Makeing remote call ... \n");
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

   }else{
     if (isErr){
       ACSError res = ACS_ERROR(ACSErr::ACSErrTypeTest, ACSErrTest3, "ESTestImpl::f2");
       res.addData ("End of call's chain", "");
       throw ACS_EXCEPTION(res); 
     }//if
   }
 }
}
/*___oOo___*/





