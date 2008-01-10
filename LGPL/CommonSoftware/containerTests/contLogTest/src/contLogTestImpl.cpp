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
* "@(#) $Id: contLogTestImpl.cpp,v 1.9 2008/01/10 09:32:20 eallaert Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* eallaert  2007-11-05  initial version
*
*/
 
#include <contLogTestImpl.h>
#include <ACSErrTypeCommon.h>
#include <loggingLogLevelDefinition.h>
#include <loggingLogger.h>
#include "loggingGetLogger.h"
#include <iostream>

ACE_RCSID(contLogTest, contLogTestImpl, "$Id: contLogTestImpl.cpp,v 1.9 2008/01/10 09:32:20 eallaert Exp $")

/* ----------------------------------------------------------------*/
TestLogLevelsComp::TestLogLevelsComp( 
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices) :
    ACSComponentImpl(name, containerServices)
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::TestLogLevelsComp::TestLogLevelsComp");
}
/* ----------------------------------------------------------------*/
TestLogLevelsComp::~TestLogLevelsComp()
{
    // ACS_TRACE is used for debugging purposes
    ACS_TRACE("::TestLogLevelsComp::~TestLogLevelsComp");
    ACS_DEBUG_PARAM("::TestLogLevelsComp::~TestLogLevelsComp", "Destroying %s...", name());
}
/* --------------------- [ CORBA interface ] ----------------------*/
::contLogTest::LongSeq*
TestLogLevelsComp::getLevels ()
    throw (CORBA::SystemException, ACSErrTypeCommon::CouldntPerformActionEx)
{
    Logging::Logger *l = getLogger();
    ::contLogTest::LongSeq_var level = new ::contLogTest::LongSeq(5);
	level->length(5);

    // need the equivalent of Java's logConfig.getMinLogLevel() etc. in C++
    for (int i = 0; i <5 ; i++)
    	level[i] = static_cast< CORBA::Long >(i);

    level[3] = static_cast< CORBA::Long >(l->getRemoteLevel());
    level[4] = static_cast< CORBA::Long >(l->getLocalLevel());
    return level._retn();
}

void 
TestLogLevelsComp::logDummyMessages (const ::contLogTest::LongSeq & levels)
{
	ACE_Log_Priority p;
	CORBA::ULong t=0;
	for (t=0; t<levels.length(); t++){
		p = LogLevelDefinition::getACELogPriority(levels[t]);
		LogLevelDefinition lld = LogLevelDefinition::fromInteger(levels[t]);
		ACS_SHORT_LOG((p, "dummy log message for core level %d/%s", lld.getValue(), lld.getName().c_str()));
	}
	// log last message always at highest, non-OFF level (so it should get always through,
	// unless the central level is put to OFF).
	p = LogLevelDefinition::getACELogPriority(AcsLogLevels::EMERGENCY_VAL);
	ACS_SHORT_LOG((p, "===last log message==="));
	
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TestLogLevelsComp)
/* ----------------------------------------------------------------*/


/*___oOo___*/



