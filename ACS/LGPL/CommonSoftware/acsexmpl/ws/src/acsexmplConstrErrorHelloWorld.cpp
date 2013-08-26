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
* "@(#) $Id: acsexmplConstrErrorHelloWorld.cpp,v 1.2 2011/04/06 17:26:51 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rbourtem 2010-10-22 created 
*/
 
#include <acsexmplConstrErrorHelloWorld.h>
#include <ACSErrTypeCommon.h>
#include <acsexmplErrTest.h>
#include <iostream>

ACE_RCSID(acsexmpl, acsexmplConstrErrorCompImpl, "$Id: acsexmplConstrErrorHelloWorld.cpp,v 1.2 2011/04/06 17:26:51 msekoran Exp $")

/* ----------------------------------------------------------------*/
ConstrErrorHelloWorld::ConstrErrorHelloWorld( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::ConstrErrorHelloWorld::ConstrErrorHelloWorld");
    // we do not throw CORBA exception here
    throw (acsexmplErrTest::ConstructionFailureExImpl(__FILE__, __LINE__, "ConstrErrorHelloWorld::ConstrErrorHelloWorld"));
}
/* ----------------------------------------------------------------*/
ConstrErrorHelloWorld::~ConstrErrorHelloWorld()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::ConstrErrorHelloWorld::~ConstrErrorHelloWorld");
    ACS_DEBUG_PARAM("::ConstrErrorHelloWorld::~ConstrErrorHelloWorld", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
ConstrErrorHelloWorld::displayMessage ()
{
    std::cout << "Hello World" << std::endl; 
}
/* ----------------------------------------------------------------*/
void 
ConstrErrorHelloWorld::badMethod() 
{
    throw (ACSErrTypeCommon::UnknownExImpl(__FILE__, __LINE__, "ConstrErrorHelloWorld::badMethod").getUnknownEx());
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(ConstrErrorHelloWorld)
/* ----------------------------------------------------------------*/


/*___oOo___*/



