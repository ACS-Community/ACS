/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
*
* "@(#) $Id: TestMaxMsgSizeImpl.cpp,v 1.2 2008/10/08 01:58:12 cparedes Exp $"
*
*/
 
#include <TestMaxMsgSizeImpl.h>
#include <iostream>
using namespace std;

/* ----------------------------------------------------------------*/
TestMaxMsgSizeImpl::TestMaxMsgSizeImpl(PortableServer::POA_ptr poa, const ACE_CString &_name) :
    ACSComponentImpl(poa, _name)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::TestMaxMsgSizeImpl::TestMaxMsgSizeImpl");
}
/* ----------------------------------------------------------------*/
TestMaxMsgSizeImpl::~TestMaxMsgSizeImpl()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::TestMaxMsgSizeImpl::~TestMaxMsgSizeImpl");
    ACS_DEBUG_PARAM("::TestMaxMsgSizeImpl::~TestMaxMsgSizeImpl", "Destroying %s...", name());
}

/* --------------------- [ CORBA interface ] ----------------------*/
void TestMaxMsgSizeImpl::sendSequence(const TEST_MAX_MSG_SIZE::TestMaxMsgSize::CharacterSequence & inSeq) 

{
    cout << "sendSequence received sequence of length: " << inSeq.length() << endl; 
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TestMaxMsgSizeImpl)
/* ----------------------------------------------------------------*/


/*___oOo___*/


