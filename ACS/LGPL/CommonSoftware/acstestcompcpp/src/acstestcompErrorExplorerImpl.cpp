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
*
* "@(#) $Id: acstestcompErrorExplorerImpl.cpp,v 1.2 2008/10/01 05:33:43 cparedes Exp $"
*
*/


#include <acstestcompErrorExplorerImpl.h>

ACE_RCSID(acstestcomp, acstestcompErrorExplorerImpl, "$Id: acstestcompErrorExplorerImpl.cpp,v 1.2 2008/10/01 05:33:43 cparedes Exp $")

using namespace baci;

/////////////////////////////////////////////////
// ErrorExplorerImpl
/////////////////////////////////////////////////

ErrorExplorerImpl::ErrorExplorerImpl( 
			 const ACE_CString &name,
			 maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name,containerServices),
    m_explorerDoubleProperty_sp(new RWdouble(name+":explorerDoubleProperty", getComponent()),this)
{
    ACS_TRACE("::ErrorExplorerImpl::ErrorExplorerImpl"); 
}


ErrorExplorerImpl::~ErrorExplorerImpl()
{
    ACS_TRACE("::ErrorExplorerImpl::~ErrorExplorerImpl");
}

ACS::RWdouble_ptr
ErrorExplorerImpl::explorerDoubleProperty()
{
    if (m_explorerDoubleProperty_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}
    
    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_explorerDoubleProperty_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ErrorExplorerImpl)


