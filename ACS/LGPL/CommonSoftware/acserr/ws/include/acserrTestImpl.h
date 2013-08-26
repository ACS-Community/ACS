#ifndef _ACSERRTEST_IMPL_H_
#define _ACSERRTEST_IMPL_H_
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
* "@(#) $Id: acserrTestImpl.h,v 1.42 2008/09/29 08:38:53 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr  20/06/01  created
* rlemke   30/08/01  integrated into tat
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "acserrTestS.h"
#include "acserr.h"

class acserrTestImpl : public POA_acserrTest {
public:
    acserrTestImpl(acserrTest* dest, const char *sn);

    ACSErr::Completion * testNoError ();
  
    ACSErr::Completion * testDefaultError ();
  
    ACSErr::Completion * test (CORBA::Long depth, CORBA::Boolean err ); 
   
    /*
    * \throw ACSErrTypeTest::ACSErrTest0Ex
    * \throw ACSErr::ACSException
    */
    void testExceptions (CORBA::Long depth, CORBA::Boolean err); 

    void testCompletionOut ( CORBA::Long depth, CORBA::Boolean err, ACSErr::Completion_out c ); 

    // ACSError
  CompletionImpl* f1 (int depth, bool iserr);
  
  void f2(int depth, bool isErr);


  void shutdown( ); 

protected:
  acserrTest_var dest;
  int depth;
  const char *srvName;
};

#endif /*!_H*/






