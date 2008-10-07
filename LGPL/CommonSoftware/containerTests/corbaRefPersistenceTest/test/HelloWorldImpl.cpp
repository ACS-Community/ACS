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
* "@(#) $Id: HelloWorldImpl.cpp,v 1.3 2008/10/07 09:47:17 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2008-041-01 created 
*/
 
#include <HelloWorldImpl.h>
#include <ACSErrTypeCommon.h>
#include <iostream>

ACE_RCSID(corbaRefPeristenceTest, HelloWorld, "$Id: HelloWorldImpl.cpp,v 1.3 2008/10/07 09:47:17 cparedes Exp $")

/* ----------------------------------------------------------------*/
HelloWorld::HelloWorld( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::HelloWorld::HelloWorld");
}
/* ----------------------------------------------------------------*/
HelloWorld::~HelloWorld()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::HelloWorld::~HelloWorld");
    ACS_DEBUG_PARAM("::HelloWorld::~HelloWorld", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
HelloWorld::displayMessage ()
{
    std::cout << "Hello World" << std::endl; 
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(HelloWorld)
/* ----------------------------------------------------------------*/


/*___oOo___*/



