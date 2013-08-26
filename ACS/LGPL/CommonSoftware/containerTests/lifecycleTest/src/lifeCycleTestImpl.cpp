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
* "@(#) $Id: lifeCycleTestImpl.cpp,v 1.1 2008/03/24 07:43:35 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* eallaert  2007-11-05  initial version
*
*/
 
#include <lifeCycleTestImpl.h>
#include <ACSErrTypeCommon.h>
#include <loggingLogLevelDefinition.h>
#include <loggingLogger.h>
#include "loggingGetLogger.h"
#include <iostream>

ACE_RCSID(lifeCycleTest, lifeCycleTestImpl, "$Id: lifeCycleTestImpl.cpp,v 1.1 2008/03/24 07:43:35 cparedes Exp $")

/* ----------------------------------------------------------------*/
TestLifeCycleComp::TestLifeCycleComp( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::TestLifeCycleComp::TestLifeCycleComp");
}
/* ----------------------------------------------------------------*/
TestLifeCycleComp::~TestLifeCycleComp()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::TestLifeCycleComp::~TestLifeCycleComp");
    ACS_DEBUG_PARAM("::TestLifeCycleComp::~TestLifeCycleComp", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/

void TestLifeCycleComp::dummyInterface(){
    ACS_TRACE("TestLifeCycleComp::dummyInterface");
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TestLifeCycleComp)
/* ----------------------------------------------------------------*/


/*___oOo___*/



