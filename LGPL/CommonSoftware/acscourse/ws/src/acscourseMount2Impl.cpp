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
* "@(#) $Id: acscourseMount2Impl.cpp,v 1.9 2008/10/02 08:58:19 cparedes Exp $"
*
*/
 
#include <acscourseMount2Impl.h>

/* ----------------------------------------------------------------*/
Mount2Impl::Mount2Impl(const ACE_CString &_name, maci::ContainerServices *containerServices) :
    CharacteristicComponentImpl(_name, containerServices),
    m_cmdAz_sp(new baci::RWdouble(_name+":cmdAz", getComponent()),this),
    m_cmdEl_sp(new baci::RWdouble(_name+":cmdEl", getComponent()),this),
    m_actAz_sp(new baci::ROdouble(_name+":actAz", getComponent()),this),
    m_actEl_sp(new baci::ROdouble(_name+":actEl", getComponent()),this)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount2Impl::Mount2Impl");
}

/* ----------------------------------------------------------------*/
Mount2Impl::~Mount2Impl()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount2Impl::~Mount2Impl");
}
/* --------------------- [ CORBA interface ] ----------------------*/
void 
Mount2Impl::objfix (CORBA::Double az,
		    CORBA::Double elev)
{
    ACS_TRACE("::Mount2Impl::objfix");
    ACS::Time timestamp;

    try
	{
	m_cmdAz_sp->getDevIO()->write(az, timestamp);
	m_cmdEl_sp->getDevIO()->write(elev, timestamp);
	}
    catch (...) 
	{
	// Here we have to better handle errors!
	ACS_SHORT_LOG((LM_ERROR,"Error accessing devIO"));
	}
    
    ACS_SHORT_LOG((LM_INFO,"Received objfix command. Az: %f El: %f", az, elev));
}

ACS::RWdouble_ptr
Mount2Impl::cmdAz ()
{
    if (m_cmdAz_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}

    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_cmdAz_sp->getCORBAReference());
    return prop._retn();
}


ACS::RWdouble_ptr
Mount2Impl::cmdEl ()
{
    if (m_cmdEl_sp == 0)
	{
	return ACS::RWdouble::_nil();
	}

    ACS::RWdouble_var prop = ACS::RWdouble::_narrow(m_cmdEl_sp->getCORBAReference());
    return prop._retn();
}


ACS::ROdouble_ptr
Mount2Impl::actAz ()
{
    if (m_actAz_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}

    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actAz_sp->getCORBAReference());
    return prop._retn();
}


ACS::ROdouble_ptr
Mount2Impl::actEl ()
{
    if (m_actEl_sp == 0)
	{
	return ACS::ROdouble::_nil();
	}
    
    ACS::ROdouble_var prop = ACS::ROdouble::_narrow(m_actEl_sp->getCORBAReference());
    return prop._retn();
}

/*
 * ATTENTION:
 * We do not put here the macro used to instantiate components.
 * Because the class Mount2ImplLopp will be derived from Mount2Impl, but
 * we also want to be able to activate a PS object on it's own, we must separate the DLL 
 * functions used by Container from the *Impl.cpp file
 * and put it them in the acscourseMount2ImplDLL.cpp file 
 */

// #include "acscourseMount2Impl.h"

/* --------------- [ MACI DLL support functions ] -----------------*/
//  #include <maciACSComponentDefines.h>
//  MACI_DLL_SUPPORT_FUNCTIONS(Mount2Impl)
/* ----------------------------------------------------------------*/

/*___oOo___*/



