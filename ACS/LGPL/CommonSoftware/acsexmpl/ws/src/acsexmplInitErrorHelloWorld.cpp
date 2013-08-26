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
* "@(#) $Id: acsexmplInitErrorHelloWorld.cpp,v 1.2 2011/04/06 17:26:51 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* rbourtem 2010-10-22 created 
*/
 
#include <acsexmplInitErrorHelloWorld.h>
#include <ACSErrTypeCommon.h>
#include <acsexmplErrTest.h>
#include <iostream>

ACE_RCSID(acsexmpl, acsexmplInitErrorCompImpl, "$Id: acsexmplInitErrorHelloWorld.cpp,v 1.2 2011/04/06 17:26:51 msekoran Exp $")

/* ----------------------------------------------------------------*/
InitErrorHelloWorld::InitErrorHelloWorld( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::InitErrorHelloWorld::InitErrorHelloWorld");
}
/* ----------------------------------------------------------------*/
InitErrorHelloWorld::~InitErrorHelloWorld()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::InitErrorHelloWorld::~InitErrorHelloWorld");
    ACS_DEBUG_PARAM("::InitErrorHelloWorld::~InitErrorHelloWorld", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
void
InitErrorHelloWorld::displayMessage ()
{
    std::cout << "Hello World" << std::endl; 
}
/* ----------------------------------------------------------------*/
void 
InitErrorHelloWorld::badMethod() 
{
    throw (ACSErrTypeCommon::UnknownExImpl(__FILE__, __LINE__, "InitErrorHelloWorld::badMethod").getUnknownEx());
}

/* --------------------- [ Life cycle methods ] -------------------- */

 void InitErrorHelloWorld::initialize()
{
    ACS_TRACE("::InitErrorHelloWorld::initialize");
    // we do not throw CORBA exception here
    throw (acsexmplErrTest::InitializationFailureExImpl(__FILE__, __LINE__, "InitErrorHelloWorld::initialize"));
}


/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(InitErrorHelloWorld)
/* ----------------------------------------------------------------*/


/*___oOo___*/



