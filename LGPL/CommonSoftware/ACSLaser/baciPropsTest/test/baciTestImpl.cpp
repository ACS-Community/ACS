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
* "@(#) $Id: baciTestImpl.cpp,v 1.11 2009/10/13 10:27:19 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-03-17 created
 
*/


#include <baciTestImpl.h>
#include <testComponentC.h>

ACE_RCSID(acsexmpl, baciTestImpl, "$Id: baciTestImpl.cpp,v 1.11 2009/10/13 10:27:19 acaproni Exp $")

using namespace baci;

BaciPropTest::BaciPropTest(ACE_CString name, maci::ContainerServices * containerServices) : 
    CharacteristicComponentImpl(name, containerServices),
    m_testDoubleVar_sp(new ROdouble(name+":testDoubleVar", getComponent()),this),
    m_testBooleanVar_sp(new ROboolean(name+":testBooleanVar", getComponent()),this),
    m_testPatternVar_sp(new ROpattern(name+":testPatternVar", getComponent()),this),
    m_testEnumVar_sp(new ROEnumImpl<ACS_ENUM_T(alarmsystemPropTest::AlarmEnum), POA_alarmsystemPropTest::ROAlarmEnum>(name+":testEnumVar",getComponent()),this)
{
    ACS_TRACE("::BaciPropTest::BaciPropTest");
}

BaciPropTest::~BaciPropTest()
{
}

void BaciPropTest::execute() 
{
	ACS::Time timestamp;
	
	// init properties to avoid they send an alarm
	m_testDoubleVar_sp->getDevIO()->write(0, timestamp);
	m_testEnumVar_sp->getDevIO()->write(alarmsystemPropTest::Ok, timestamp);
	m_testPatternVar_sp->getDevIO()->write(0, timestamp);
}

/* --------------------- [ CORBA interface ] ----------
 *
 */
ACS::ROdouble_ptr BaciPropTest::testDoubleVar () 
{
    if (m_testDoubleVar_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_testDoubleVar_sp->getCORBAReference());
    return prop._retn();
}

ACS::ROboolean_ptr BaciPropTest::testBooleanVar()
{
	if (m_testDoubleVar_sp == 0)
	{
	return ACS::ROboolean::_nil();
	}

	ACS::ROboolean_var prop = ACS::ROboolean::_narrow(m_testBooleanVar_sp->getCORBAReference());
	return prop._retn();
}

ACS::ROpattern_ptr BaciPropTest::testPatternVar () 
{
    if (m_testPatternVar_sp == 0)
	{
	return ACS::ROpattern::_nil();
	}

    ACS::ROpattern_var prop = ACS::ROpattern::_narrow(m_testPatternVar_sp->getCORBAReference());
    return prop._retn();
}

::alarmsystemPropTest::ROAlarmEnum_ptr BaciPropTest::testEnumVar () 
{
    if (m_testEnumVar_sp == 0)
	{
	return ::alarmsystemPropTest::ROAlarmEnum::_nil();
	}

    ::alarmsystemPropTest::ROAlarmEnum_var prop = ::alarmsystemPropTest::ROAlarmEnum::_narrow(m_testEnumVar_sp->getCORBAReference());
    return prop._retn();
}

void BaciPropTest::setDoubleVar(CORBA::Float val) {
	ACS::Time timestamp;
	    
	m_testDoubleVar_sp->getDevIO()->write(val, timestamp);
}

void BaciPropTest::setBooleanVar(CORBA::Boolean val) {
	ACS::Time timestamp;

	m_testBooleanVar_sp->getDevIO()->write(val, timestamp);
}

void BaciPropTest::setPatternVar(CORBA::Long val) {
	ACS::Time timestamp;
	m_testPatternVar_sp->getDevIO()->write(val, timestamp);
}

void BaciPropTest::setEnumVar(alarmsystemPropTest::AlarmEnum val) {
	ACS::Time timestamp;
	m_testEnumVar_sp->getDevIO()->write(val, timestamp);
}

void  BaciPropTest::setDoubleVarComplete(CORBA::Float val, const char* faultFamily, const char* faultMember) {
	m_testDoubleVar_sp->setAlarmFaultFamily(faultFamily);
	m_testDoubleVar_sp->setAlarmFaultMember(faultMember);
	ACS::Time timestamp;
	m_testDoubleVar_sp->getDevIO()->write(val, timestamp);
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(BaciPropTest)
/* ----------------------------------------------------------------*/





