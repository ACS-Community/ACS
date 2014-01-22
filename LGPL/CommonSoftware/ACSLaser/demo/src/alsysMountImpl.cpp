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
*/


#include <alsysMountImpl.h>

#include "ACSAlarmSystemInterfaceFactory.h"
#include "AlarmSystemInterface.h"
#include "FaultState.h"
#include "faultStateConstants.h"
#include "Timestamp.h"
#include "Properties.h"

using namespace acscomponent;
using namespace acsalarm;

Mount::Mount(const ACE_CString &name,maci::ContainerServices * containerServices) : 
    ACSComponentImpl(name, containerServices)
{
    ACS_TRACE("::Mount::Mount");
     
}

Mount::~Mount()
{
}

void Mount::faultMount(){
	getContainerServices()->getAlarmSource()->raiseAlarm("Mount","ALARM_SOURCE_MOUNTCPP",1);
}

void Mount::terminate_faultMount(){
	getContainerServices()->getAlarmSource()->clearAlarm("Mount","ALARM_SOURCE_MOUNTCPP",1);
}


/* --------------- [ MACI DLL support functions ] -----------------*/

#include <maciACSComponentDefines.h>
MACI_DLL_SUPPORT_FUNCTIONS(Mount)
/* ----------------------------------------------------------------*/

