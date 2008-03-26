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
* "@(#) $Id: baciTestImpl.cpp,v 1.8 2008/03/26 14:18:29 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-03-17 created
 
*/


#include <baciTestImpl.h>
#include <testComponentC.h>

ACE_RCSID(acsexmpl, baciTestImpl, "$Id: baciTestImpl.cpp,v 1.8 2008/03/26 14:18:29 acaproni Exp $")

using namespace baci;

BaciPropTest::BaciPropTest(ACE_CString name, maci::ContainerServices * containerServices) : 
    CharacteristicComponentImpl(name, containerServices),
    m_testDoubleVar_sp(new ROdouble(name+":testDoubleVar", getComponent()),this),
    m_testPatternVar_sp(new ROpattern(name+":testPatternVar", getComponent()),this),
    m_testEnumVar_sp(new ROEnumImpl<ACS_ENUM_T(alarmsystemPropTest::AlarmEnum), POA_alarmsystemPropTest::ROAlarmEnum>(name+":testEnumVar",getComponent()),this)
{
    ACS_TRACE("::BaciPropTest::BaciPropTest");
}

BaciPropTest::~BaciPropTest()
{
}

void BaciPropTest::execute() throw (ACSErr::ACSbaseExImpl)
{
	ACS::Time timestamp;
	
	// init properties to avoid they send an alarm
	m_testDoubleVar_sp->getDevIO()->write(0, timestamp);
	m_testEnumVar_sp->getDevIO()->write(alarmsystemPropTest::Ok, timestamp);
	m_testPatternVar_sp->getDevIO()->write(0, timestamp);
}

/* --------------------- [ CORBA interface ] ----------------------*/
ACS::ROdouble_ptr BaciPropTest::testDoubleVar () throw (CORBA::SystemException)
{
    if (m_testDoubleVar_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_testDoubleVar_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROpattern_ptr BaciPropTest::testPatternVar () throw (CORBA::SystemException)
{
    if (m_testPatternVar_sp == 0)
	{
	return ACS::ROpattern::_nil();
	}

    ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_testPatternVar_sp->getCORBAReference());
    return prop._retn();
}

::alarmsystemPropTest::ROAlarmEnum_ptr BaciPropTest::testEnumVar () throw (CORBA::SystemException)
{
    if (m_testEnumVar_sp == 0)
	{
	return ::alarmsystemPropTest::ROAlarmEnum::_nil();
	}

    ::alarmsystemPropTest::ROAlarmEnum_var prop = ::alarmsystemPropTest::ROAlarmEnum::_narrow(m_testEnumVar_sp->getCORBAReference());
    return prop._retn();
}

void BaciPropTest::setDoubleVar(CORBA::Float val) throw (CORBA::SystemException) {
	ACS::Time timestamp;
	    
	m_testDoubleVar_sp->getDevIO()->write(val, timestamp);
}

void BaciPropTest::setPatternVar(CORBA::Long val) throw (CORBA::SystemException) {
	ACS::Time timestamp;
	m_testPatternVar_sp->getDevIO()->write(val, timestamp);
}

void BaciPropTest::setEnumVar(alarmsystemPropTest::AlarmEnum val) throw (CORBA::SystemException) {
	ACS::Time timestamp;
	m_testEnumVar_sp->getDevIO()->write(val, timestamp);
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BaciPropTest)
/* ----------------------------------------------------------------*/





