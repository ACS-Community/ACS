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
*
* who       when      what
* --------  --------  ----------------------------------------------
*/

#include "acstestcompcppTestCDBCompImpl.h"  
#include <baciDB.h>

using namespace baci;

TestCDBComp::TestCDBComp( 
	     ACE_CString _name,
	     maci::ContainerServices * containerServices) : 
    CharacteristicComponentImpl(_name, containerServices),
    m_testatt_sp(new ROdouble(_name+":testatt", getComponent()),this)
{
    ACS_TRACE("::TestCDBComp::TestCDBComp");
}

TestCDBComp::~TestCDBComp()
{
}
ACS::ROdouble_ptr
TestCDBComp::testatt ()
{
    if (m_testatt_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_testatt_sp->getCORBAReference());
    return prop._retn();
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TestCDBComp)
/* ----------------------------------------------------------------*/





