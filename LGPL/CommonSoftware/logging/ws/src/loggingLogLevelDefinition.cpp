/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
* "@(#) $Id: loggingLogLevelDefinition.cpp,v 1.1 2007/11/30 11:04:31 cparedes Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/
//---------------------------------------------------------------------------------------
#include <loggingLogLevelDefinition.h>

LogLevelDefinition::LogLevelDefinition(int val, std::string name ){
    m_value = val;
    m_name = name;
}

std::string LogLevelDefinition::getName(){
    return m_name;
}

int LogLevelDefinition::getValue(){
    return m_value;    
}

LogLevelDefinition LogLevelDefinition::fromInteger(int val){

  switch(val){
    case AcsLogLevels::TRACE_VAL:
        return LogLevelDefinition(AcsLogLevels::TRACE_VAL,AcsLogLevels::TRACE_NAME);
        break;
    case AcsLogLevels::DEBUG_VAL:
        return LogLevelDefinition(AcsLogLevels::DEBUG_VAL,AcsLogLevels::DEBUG_NAME);
        break;
    case AcsLogLevels::INFO_VAL:
        return LogLevelDefinition(AcsLogLevels::INFO_VAL,AcsLogLevels::INFO_NAME);
        break;
    case AcsLogLevels::NOTICE_VAL:
        return LogLevelDefinition(AcsLogLevels::NOTICE_VAL,AcsLogLevels::NOTICE_NAME);
        break;
    case AcsLogLevels::WARNING_VAL:
        return LogLevelDefinition(AcsLogLevels::WARNING_VAL,AcsLogLevels::WARNING_NAME);
        break;
    case AcsLogLevels::ERROR_VAL:
        return LogLevelDefinition(AcsLogLevels::ERROR_VAL,AcsLogLevels::ERROR_NAME);
        break;
    case AcsLogLevels::CRITICAL_VAL:
        return LogLevelDefinition(AcsLogLevels::CRITICAL_VAL,AcsLogLevels::CRITICAL_NAME);
        break;
    case AcsLogLevels::ALERT_VAL:
        return LogLevelDefinition(AcsLogLevels::ALERT_VAL,AcsLogLevels::ALERT_NAME);
        break;
    case AcsLogLevels::EMERGENCY_VAL:
        return LogLevelDefinition(AcsLogLevels::EMERGENCY_VAL,AcsLogLevels::EMERGENCY_NAME);
        break;
    default:
        return LogLevelDefinition(AcsLogLevels::OFF_VAL,AcsLogLevels::OFF_NAME);
	
	}
}

LogLevelDefinition LogLevelDefinition::fromName(std::string name){

    if(name == AcsLogLevels::TRACE_NAME)
        return LogLevelDefinition(AcsLogLevels::TRACE_VAL,AcsLogLevels::TRACE_NAME);
    else if (name == AcsLogLevels::DEBUG_NAME)
        return LogLevelDefinition(AcsLogLevels::DEBUG_VAL,AcsLogLevels::DEBUG_NAME);
    else if(name == AcsLogLevels::INFO_NAME)
        return LogLevelDefinition(AcsLogLevels::INFO_VAL,AcsLogLevels::INFO_NAME);
    else if(name == AcsLogLevels::NOTICE_NAME)
        return LogLevelDefinition(AcsLogLevels::NOTICE_VAL,AcsLogLevels::NOTICE_NAME);
    else if (name == AcsLogLevels::WARNING_NAME)
        return LogLevelDefinition(AcsLogLevels::WARNING_VAL,AcsLogLevels::WARNING_NAME);
    else if(name == AcsLogLevels::ERROR_NAME)
        return LogLevelDefinition(AcsLogLevels::ERROR_VAL,AcsLogLevels::ERROR_NAME);
    else if(name == AcsLogLevels::CRITICAL_NAME)
        return LogLevelDefinition(AcsLogLevels::CRITICAL_VAL,AcsLogLevels::CRITICAL_NAME);
    else if(name == AcsLogLevels::ALERT_NAME)
        return LogLevelDefinition(AcsLogLevels::ALERT_VAL,AcsLogLevels::ALERT_NAME);
    else if(name == AcsLogLevels::EMERGENCY_NAME)
        return LogLevelDefinition(AcsLogLevels::EMERGENCY_VAL,AcsLogLevels::EMERGENCY_NAME);
   else 
        return LogLevelDefinition(AcsLogLevels::OFF_VAL,AcsLogLevels::OFF_NAME);
}

ACE_Log_Priority LogLevelDefinition::m_LogEntryCast[] =
{
    LM_TRACE,		// not in specs
    LM_TRACE, 	// not in specs
    LM_TRACE,
    LM_DEBUG,
    LM_INFO,
    LM_NOTICE,
    LM_WARNING,
    LM_TRACE,		// not in specs
    LM_ERROR,
    LM_CRITICAL,
    LM_ALERT,
    LM_EMERGENCY
};

ACE_Log_Priority LogLevelDefinition::getACELogPriority(int p){
    if(p >= 0 && p < 12 )
        return m_LogEntryCast[p];
    else return LM_TRACE;
}

//---------------------------------------------------------------------------------------
//---------------------------------------------------------------------------------------

