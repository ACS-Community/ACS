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
* "@(#) $Id: acsexmplCalendarImpl.cpp,v 1.100 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use smart pointers for properties
* msekoran 2002-07-05 created
*/

#include <acsexmplCalendarImpl.h>

ACE_RCSID(acsxmpl, acsexmplCalendarImpl, "$Id: acsexmplCalendarImpl.cpp,v 1.100 2008/10/01 04:30:47 cparedes Exp $")

using namespace baci;

/////////////////////////////////////////////////
// Calendar
/////////////////////////////////////////////////
Calendar::Calendar(const ACE_CString &name,
		   maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name,containerServices),
    m_yearAttributes_sp(new RWpattern(name+":yearAttributes", getComponent()),this),
    m_day_sp(new RWEnumImpl<ACS_ENUM_T(acsexmplCalendar::DaysEnum),  POA_acsexmplCalendar::RWDaysEnum> (name+":day", getComponent()),this),
    m_month_sp(new RWEnumImpl<ACS_ENUM_T(acsexmplCalendar::MonthEnum), POA_acsexmplCalendar::RWMonthEnum> (name+":month", getComponent()),this),
    m_state_sp(new ROEnumImpl<ACS_ENUM_T(acsexmplCalendar::StateEnum), POA_acsexmplCalendar::ROStateEnum> (name+":state", getComponent()),this)
{    
    ACS_TRACE("::Calendar::Calendar");
    
}

Calendar::~Calendar()
{
    ACS_TRACE("::Calendar::~Calendar");
}

/* --------------------- [ CORBA interface ] ----------------------*/
::acsexmplCalendar::RWDaysEnum_ptr
Calendar::day ()
{
    if (m_day_sp == 0)
	{
	return ::acsexmplCalendar::RWDaysEnum::_nil();
	}
    
    ::acsexmplCalendar::RWDaysEnum_var prop = ::acsexmplCalendar::RWDaysEnum::_narrow(m_day_sp->getCORBAReference());
    return prop._retn();
}
  
::acsexmplCalendar::RWMonthEnum_ptr
Calendar::month ()
{
    if (m_month_sp == 0)
	{
	return ::acsexmplCalendar::RWMonthEnum::_nil();
	}
    
    ::acsexmplCalendar::RWMonthEnum_var prop = ::acsexmplCalendar::RWMonthEnum::_narrow(m_month_sp->getCORBAReference());
    return prop._retn();
};

::ACS::RWpattern_ptr
Calendar::yearAttributes ()
{
    if (m_yearAttributes_sp == 0)
	{
	return ::ACS::RWpattern::_nil();
	}
    
    ::ACS::RWpattern_var prop = ::ACS::RWpattern::_narrow(m_yearAttributes_sp->getCORBAReference());
    return prop._retn();
}

::acsexmplCalendar::ROStateEnum_ptr
Calendar::state ()
{
    if (m_state_sp == 0)
	{
	return ::acsexmplCalendar::ROStateEnum::_nil();
	}
    
    ::acsexmplCalendar::ROStateEnum_var prop = ::acsexmplCalendar::ROStateEnum::_narrow(m_state_sp->getCORBAReference());
    return prop._retn();
}

/////////////////////////////////////////////////
// MACI DLL support functions
/////////////////////////////////////////////////
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Calendar)

