/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 * "@(#) $Id: baciROpattern.cpp,v 1.116 2010/04/20 15:29:12 bjeram Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * pcolomer  2014/10/13  First version
 */
#include "baciAlarmSystemMonitorBoolean.h"
#include "baciAlarmSystemMonitor_T.i"

namespace baci {

AlarmSystemMonitorBoolean::AlarmSystemMonitorBoolean(ROboolean* property,EventDispatcher * eventDispatcher)
	: AlarmSystemMonitor<ROboolean>(property, eventDispatcher)
{}

AlarmSystemMonitorBoolean::~AlarmSystemMonitorBoolean()
{}

void AlarmSystemMonitorBoolean::updateAlarm(bool enable)
{
	if(enable == ENABLE)
	{
		sendAlarm(1, true);
		alarmRaised_m = ALARM_RAISED;
	} else if(enable == DISABLE) {
		sendAlarm(1, false);
		alarmRaised_m = ALARM_NOT_RAISED;
	}
}

void AlarmSystemMonitorBoolean::check(BACIValue &val,
								const ACSErr::Completion & c,
								const ACS::CBDescOut & desc)
{
    ACE_UNUSED_ARG(c);
    ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(faultStructMutex_m);

    CORBA::Boolean value = val.getValue(static_cast<CORBA::Boolean*>(0));

    if(alarmRaised_m != ALARM_NOT_RAISED && value != property_mp->alarm_on())
    {
    	updateAlarm(DISABLE);
    } else if(alarmRaised_m == ALARM_NOT_RAISED && value == property_mp->alarm_on()) {
    	updateAlarm(ENABLE);
    }
}

}
