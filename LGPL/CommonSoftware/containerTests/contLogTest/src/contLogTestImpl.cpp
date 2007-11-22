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
* "@(#) $Id: contLogTestImpl.cpp,v 1.3 2007/11/22 09:54:25 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* eallaert  2007-11-05  initial version
*
*/
 
#include <contLogTestImpl.h>
#include <ACSErrTypeCommon.h>
#include <iostream>

ACE_RCSID(contLogTest, contLogTestImpl, "$Id: contLogTestImpl.cpp,v 1.3 2007/11/22 09:54:25 cparedes Exp $")

/* ----------------------------------------------------------------*/
LogLevels::LogLevels( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::LogLevels::LogLevels");
}
/* ----------------------------------------------------------------*/
LogLevels::~LogLevels()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::LogLevels::~LogLevels");
    ACS_DEBUG_PARAM("::LogLevels::~LogLevels", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
::contLogTest::LongSeq*
LogLevels::getLevels ()
    throw (CORBA::SystemException, ACSErrTypeCommon::CouldntPerformActionEx)
{
    std::cout << "Hi there!" << std::endl; 
	::contLogTest::LongSeq_var level = new ::contLogTest::LongSeq(5);
	level->length(5);

    // need the equivalent of Java's logConfig.getMinLogLevel() etc. in C++
    for (int i = 0; i <5 ; i++)
    	level[i] = static_cast< CORBA::Long >(i);
    std::cout << "Done filling levels." << std::endl; 
    return level._retn();
}

void 
LogLevels::logDummyMessages (const ::contLogTest::LongSeq & levels)
{
	for (CORBA::ULong t=0; t<levels.length(); t++){
		ACE_Log_Priority p = LoggingProxy::m_LogEntryCast[levels[t]];
		ACS_SHORT_LOG((p, "dummy log message for core level %d", levels[t]));
	}
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(LogLevels)
/* ----------------------------------------------------------------*/


/*___oOo___*/



