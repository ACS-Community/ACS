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
* "@(#) $Id: acscourseMount1Impl.cpp,v 1.5 2008/10/02 08:58:19 cparedes Exp $"
*
*/
 
#include <acscourseMount1Impl.h>
#include <iostream>
using namespace std;

/* ----------------------------------------------------------------*/
Mount1Impl::Mount1Impl(const ACE_CString &name, maci::ContainerServices *containerServices) :
    acscomponent::ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount1Impl::Mount1Impl");
}
/* ----------------------------------------------------------------*/
Mount1Impl::~Mount1Impl()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::Mount1Impl::~Mount1Impl");
    ACS_DEBUG_PARAM("::Mount1Impl::~Mount1Impl", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void 
Mount1Impl::objfix (CORBA::Double az,
	CORBA::Double elev)
{
    ACS_TRACE("::Mount1Impl::~Mount1Impl");
    ACS_SHORT_LOG((LM_INFO,"Received objfix command. Az: %f El: %f", az, elev));
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Mount1Impl)
/* ----------------------------------------------------------------*/


/*___oOo___*/


