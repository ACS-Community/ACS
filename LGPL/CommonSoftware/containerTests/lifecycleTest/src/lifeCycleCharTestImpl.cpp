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
* "@(#) $Id: lifeCycleCharTestImpl.cpp,v 1.2 2008/10/07 09:45:30 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-26 Creation
*/

#include <string>

#include <baciDB.h>
#include <maciContainerServices.h>
#include <lifeCycleCharTestImpl.h>
#include <cdbDALC.h>

using namespace baci;

TestLifeCycleCharComp::TestLifeCycleCharComp( const ACE_CString& name, maci::ContainerServices * containerServices):
CharacteristicComponentImpl(name,containerServices),
m_value(this)
{
    ACS_TRACE("TestLifeCycleCharComp::TestLifeCycleCharComp");
    
}

TestLifeCycleCharComp::~TestLifeCycleCharComp(){
    ACS_TRACE("TestLifeCycleCharComp::~TestLifeCycleCharComp");   
}

void TestLifeCycleCharComp::on(){
    
    }
    
void TestLifeCycleCharComp::off(){
    
    }
ACS::RWlong_ptr TestLifeCycleCharComp::value()
{
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_value->getCORBAReference());
    return prop._retn();
    
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TestLifeCycleCharComp)
/* ----------------------------------------------------------------*/
