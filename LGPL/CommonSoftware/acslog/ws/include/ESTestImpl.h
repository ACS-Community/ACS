#ifndef _ESTEST_IMPL_H_
#define _ESTEST_IMPL_H_
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
* "@(#) $Id: ESTestImpl.h,v 1.12 2008/10/01 04:41:17 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almamgr  20/06/01  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "ESTestS.h"
#include "acserr.h"

class ESTestImpl : public POA_ESTest {
public:
  ESTestImpl(ESTest* dest, const char *sn);
  ACSErr::Completion * test (  CORBA::Long depth,
			       CORBA::Boolean err
      ) ;
  
  /*
  * @throw ACSErrTypeTest::ACSErrTest0Ex
  * @throw ACSErr::ACSException
  */
  void testExceptions ( CORBA::Long depth,
			CORBA::Boolean err);
    
    CompletionImpl* f1 (int depth, bool iserr);
  
    void f2(int depth, bool isErr);
protected:
  ESTest_var dest;
  int depth;
  const char *srvName;
};

#endif /*!_H*/






