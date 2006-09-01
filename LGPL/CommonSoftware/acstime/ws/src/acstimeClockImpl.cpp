/* @(#) $Id: acstimeClockImpl.cpp,v 1.19 2006/09/01 02:20:54 cparedes Exp $
 *
 * Copyright (C) 2001
 * Associated Universities, Inc. Washington DC, USA.
 *
 * Produced for the ALMA project
 *
 * This library is free software; you can redistribute it and/or modify it it 
 * under the terms of the GNU Library General Public License as published by 
 * the Free Software Foundation; either version 2 of the License, or (at your
 * option) any later version.
 *
 * This library is distributed in the hope that it will be useful but WITHOUT 
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or 
 * FITNESS FOR A PARTICULAR PURPOSE.  See the GNU Library General Public 
 * License for more details.
 *
 * You should have received a copy of the GNU Library General Public License 
 * along with this library; if not, write to the Free Software Foundation, 
 * Inc., 675 Massachusetts Ave, Cambridge, MA, 02139, USA.
 *
 * Correspondence concerning ALMA should be addressed as follows:
 * Internet email: alma-sw-admin@nrao.edu
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
    throw (CORBA::SystemException)
{
    acstime::Duration retValue;
    ACSErr::Completion_var completion;

    retValue.value = m_now->get_sync(completion.out()) - prevEpoch.value;
    return retValue;
}
//------------------------------------------------------------------------------
ACS::RWlong_ptr 
ClockImpl::array2TAI()
    throw (CORBA::SystemException)
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
    throw (CORBA::SystemException)
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
    throw (CORBA::SystemException)
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
    throw (CORBA::SystemException, ACSTimeError::ArgErrorEx)
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
    throw (CORBA::SystemException, ACSTimeError::ArgErrorEx)
{
    int errcode;
    ACS::Time timestamp;
    EpochHelper *e_p = new EpochHelper(timeValue);

    std::string tmpStr  = e_p->toString(ts, 
				      "", 
				      m_array2TAI->getDevIO()->read(errcode, timestamp), 
				      m_TAI2UTC->getDevIO()->read(errcode, timestamp));
    
    delete e_p;
    return CORBA::string_dup(tmpStr.c_str());
}
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ClockImpl)

