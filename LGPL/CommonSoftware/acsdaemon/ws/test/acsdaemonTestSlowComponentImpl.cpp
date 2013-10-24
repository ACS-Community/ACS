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
* "@(#) $Id: $"
*
* who      when       what
* -------- ---------  ----------------------------------------------
* rbourtem 2013-10-11 created
*/
 
#include <acsdaemonTestSlowComponentImpl.h>
#include <iostream>

//ACE_RCSID(acsexmpl, acsexmplHelloWorldImpl, "$Id: acsexmplHelloWorldImpl.cpp,v 1.95 2008/10/01 04:30:47 cparedes Exp $")

/* ----------------------------------------------------------------*/
SlowDestructionComponent::SlowDestructionComponent( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::SlowDestructionComponent::SlowDestructionComponent");
}
/* ----------------------------------------------------------------*/
SlowDestructionComponent::~SlowDestructionComponent()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::SlowDestructionComponent::~SlowDestructionComponent");
    ACS_DEBUG_PARAM("::SlowDestructionComponent::~SlowDestructionComponent", "Destroying %s... Sleeping 7 seconds", name());
   sleep(7);
}
/* --------------------- [ CORBA interface ] ----------------------*/
/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(SlowDestructionComponent)
/* ----------------------------------------------------------------*/


/*___oOo___*/



