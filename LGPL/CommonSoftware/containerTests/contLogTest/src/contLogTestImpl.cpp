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
* "@(#) $Id: contLogTestImpl.cpp,v 1.15 2008/11/19 15:23:24 eallaert Exp $"
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

ACE_RCSID(contLogTest, contLogTestImpl, "$Id: contLogTestImpl.cpp,v 1.15 2008/11/19 15:23:24 eallaert Exp $")

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
{
    Logging::Logger *l = getLogger();
    ::contLogTest::LongSeq_var level = new ::contLogTest::LongSeq(5);
	level->length(5);

    // need a way to retrieve default/hardcoded settings, i.e. the
    // equivalent of Java's logConfig.getMinLogLevel() etc. in C++.
	// Hardcode these values for the time being ;-)
	level[0] = static_cast< CORBA::Long >(2);
	level[1] = static_cast< CORBA::Long >(2);
	
    level[3] = static_cast< CORBA::Long >(LogLevelDefinition::fromACEPriority(ACE_Log_Priority(l->getRemoteLevel())));
    level[4] = static_cast< CORBA::Long >(LogLevelDefinition::fromACEPriority(ACE_Log_Priority(l->getLocalLevel())));
    
    level[2] = (level[3] < level[4] ? level[3] : level[4]);

    return level._retn();
}

void 
TestLogLevelsComp::logDummyMessages (const ::contLogTest::LongSeq & levels)
{
	ACE_Log_Priority p;
	CORBA::ULong t=0;
	// Give client time to start waiting for logs
	usleep(100000);
	for (t=0; t<levels.length(); t++){
		p = LogLevelDefinition::getACELogPriority(levels[t]);
		LogLevelDefinition lld = LogLevelDefinition::fromInteger(levels[t]);
		ACS_SHORT_LOG((p, "dummy log message for core level %d/%s", lld.getValue(), lld.getName().c_str()));
	}
	// log last message always at highest, non-OFF level (so it should get always through,
	// unless the central level is put to OFF).
	p = LogLevelDefinition::getACELogPriority(AcsLogLevels::EMERGENCY_VAL);
	ACS_SHORT_LOG((p, "===last log message==="));

	// Note that these tests may be running in a container with default
	// settings, i.e. whereby " immediateDispatchLevel" can be anything,
	// and dispatchPacketSize as well. Empirically, in such case, 
	// C++ seems to sends logs in packets of 5 logs, so add 4 messages to
	// ensure all the above logs get sent across right now.
	for (int i = 0; i < 4; i++) {
		ACS_SHORT_LOG((p, "===packet fill-up message==="));
	}
   
	
}

/* --------------- [ MACI DLL support functions ] -----------------*/
#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(TestLogLevelsComp)
/* ----------------------------------------------------------------*/


/*___oOo___*/



