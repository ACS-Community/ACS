#ifndef TestMaxMsgSizeImpl_h
#define TestMaxMsgSizeImpl_h
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
*
* "@(#) $Id: TestMaxMsgSizeImpl.h,v 1.3 2008/10/08 01:58:12 cparedes Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscomponentImpl.h>
#include "stresstestS.h"

class TestMaxMsgSizeImpl: 
    public virtual acscomponent::ACSComponentImpl, 
    public virtual POA_TEST_MAX_MSG_SIZE::TestMaxMsgSize
{
  public:
    
    TestMaxMsgSizeImpl(PortableServer::POA_ptr poa, const ACE_CString &name);
    virtual ~TestMaxMsgSizeImpl();
    virtual void sendSequence(const TEST_MAX_MSG_SIZE::TestMaxMsgSize::CharacterSequence& inSeq);

  private:
    
    void operator=(const TestMaxMsgSizeImpl&);
};

#endif /*!TestMaxMsgSizeImpl_h*/
