#ifndef loggingLogLevelDefinition_H
#define loggingLogLevelDefinition_H

/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
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
* who       when        what
* --------  ----------  ----------------------------------------------
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "logging_idlC.h"
#include "loggingExport.h"
#include <string>
#include <ace/Log_Msg_Callback.h>
#include <ace/Log_Priority.h>

class logging_EXPORT LogLevelDefinition {
  public:
    LogLevelDefinition(int val, std::string name);
    ~LogLevelDefinition(){}
    static LogLevelDefinition fromInteger(int val);
    static LogLevelDefinition fromName(std::string name);
    static ACE_Log_Priority getACELogPriority(int p);
    static int fromACEPriority(ACE_Log_Priority p);
    int getValue();
    std::string getName();
  private:

    static ACE_Log_Priority m_LogEntryCast[];
    int m_value;
    std::string m_name;
};

#endif /*!loggingLogLevelDefinition_H*/
