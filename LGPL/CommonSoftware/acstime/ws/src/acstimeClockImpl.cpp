/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: acstimeClockImpl.cpp,v 1.22 2011/10/28 15:12:04 hsommer Exp $"
*/
#include "acstimeClockImpl.h"
#include "acstimeTimeUtil.h"
#include "acstimeEpochHelper.h"
//------------------------------------------------------------------------------
 using namespace baci;
//------------------------------------------------------------------------------
ClockImpl::ClockImpl(
		     const ACE_CString& name,
		     maci::ContainerServices * containerServices) :
    CharacteristicComponentImpl(name, containerServices),
    m_array2TAI(0),
    m_TAI2UTC(0),
    m_now(0),
    m_now_dev(0)
{   
    // create array2TAI Property and set its default value (10**-7 seconds)
    m_array2TAI = new RWlong(name + ":array2TAI", getComponent());
    CHARACTERISTIC_COMPONENT_PROPERTY(array2TAI, m_array2TAI);
    
    // create TAI2UTC Property and set its default value (seconds)
    m_TAI2UTC = new RWlong(name + ":TAI2UTC", getComponent());
    CHARACTERISTIC_COMPONENT_PROPERTY(TAI2UTC, m_TAI2UTC);
    
    m_now_dev = new DevIOTime();
    m_now = new ROuLongLong(name + ":now", getComponent(), m_now_dev);
    CHARACTERISTIC_COMPONENT_PROPERTY(now, m_now);
}
//------------------------------------------------------------------------------
ClockImpl::~ClockImpl()
{   
    // delete threads
    if (getComponent() != 0)
	{
	getComponent()->stopAllThreads();
	}
    
    // properties
    if (m_array2TAI != 0) 
	{ 
	m_array2TAI->destroy();    
	m_array2TAI = 0; 
	}

    if (m_TAI2UTC != 0)   
	{ 
	m_TAI2UTC->destroy();      
	m_TAI2UTC = 0; 
	}

    if (m_now != 0)
	{ 
	m_now->destroy();      
	m_now = 0; 
	}
    if (m_now_dev != 0)   
	{ 
	delete m_now_dev;      
	m_now_dev = 0; 
	}
}
//------------------------------------------------------------------------------
acstime::Duration
ClockImpl::getTimeInterval(const acstime::Epoch &prevEpoch)
{
    acstime::Duration retValue;
    ACSErr::Completion_var completion;

    retValue.value = m_now->get_sync(completion.out()) - prevEpoch.value;
    return retValue;
}
//------------------------------------------------------------------------------
ACS::RWlong_ptr 
ClockImpl::array2TAI()
{
    if (m_array2TAI == 0)
	{
	return ACS::RWlong::_nil();
	}
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_array2TAI->getCORBAReference());
    return prop._retn();
}
//------------------------------------------------------------------------------
ACS::RWlong_ptr 
ClockImpl::TAI2UTC()
{
    if (m_TAI2UTC == 0)
	{
	return ACS::RWlong::_nil();
	}
    ACS::RWlong_var prop = ACS::RWlong::_narrow(m_TAI2UTC->getCORBAReference());
    return prop._retn();
}
//------------------------------------------------------------------------------
ACS::ROuLongLong_ptr 
ClockImpl::now()
{
    if (m_now == 0)
	{
	return ACS::ROuLongLong::_nil();
	}
    ACS::ROuLongLong_var prop = ACS::ROuLongLong::_narrow(m_now->getCORBAReference());
    return prop._retn();
}
//------------------------------------------------------------------------------
acstime::Epoch 
ClockImpl::fromISO8601(acstime::TimeSystem ts,
		       const char *iso)
{
    EpochHelper *e_p = new EpochHelper();
    e_p->fromString(ts, iso);
    acstime::Epoch eVal = e_p->value();
    delete e_p;
    return eVal;
}
//------------------------------------------------------------------------------
char*
ClockImpl::toISO8601(acstime::TimeSystem ts,
		     const acstime::Epoch &timeValue)
{
    ACS::Time timestamp;
    EpochHelper e_p(timeValue);

    try
	{
	std::string tmpStr  = e_p.toString(ts, 
					   "", 
					   m_array2TAI->getDevIO()->read(timestamp), 
					   m_TAI2UTC->getDevIO()->read(timestamp));
	
	return CORBA::string_dup(tmpStr.c_str());
	
	}
    catch(ACSErr::ACSbaseExImpl &_ex)
	{
	ACSTimeError::ArgErrorExImpl ex(_ex, __FILE__, __LINE__, 
					"ClockImpl::toISO8601");
	throw ex.getArgErrorEx();
	}
 }
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ClockImpl)

