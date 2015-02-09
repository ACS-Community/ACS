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
#include "baciAlarmSystemMonitorBooleanSeq.h"
#include "baciAlarmSystemMonitor_T.i"
#include <string>
#include <sstream>

namespace baci {

AlarmSystemMonitorBooleanSeq::AlarmSystemMonitorBooleanSeq(RObooleanSeq* property,
		EventDispatcher * eventDispatcher)
	: AlarmSystemMonitor<RObooleanSeq>(property, eventDispatcher)
{
	ACS_TRACE("AlarmSystemMonitorBooleanSeq::AlarmSystemMonitorBooleanSeq");
}

AlarmSystemMonitorBooleanSeq::~AlarmSystemMonitorBooleanSeq()
{
	ACS_TRACE("AlarmSystemMonitorBooleanSeq::~AlarmSystemMonitorBooleanSeq");
}

void AlarmSystemMonitorBooleanSeq::updateAlarm(int32_t pos,bool enable)
{
	ACS_TRACE("AlarmSystemMonitorBooleanSeq::updateAlarm");
	if(enable == ENABLE)
	{
		sendAlarm(1, true);
		alarmsRaised_m[pos] = ALARM_RAISED;
	} else if(enable == DISABLE) {
		sendAlarm(1, false);
		alarmsRaised_m[pos] = ALARM_NOT_RAISED;
	}
}

void AlarmSystemMonitorBooleanSeq::check(BACIValue &val,
								const ACSErr::Completion & c,
								const ACS::CBDescOut & desc)
{
	ACS_TRACE("AlarmSystemMonitorBooleanSeq::check");

	ACE_UNUSED_ARG(c);
	ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(this->faultStructMutex_m);

	ACS::booleanSeq valueSeq = val.getValue(static_cast<ACS::booleanSeq*>(0));

	if (alarmsRaised_m.size() != valueSeq.length())
	{
		alarmsRaised_m.clear();
		alarmsRaised_m.resize(valueSeq.length(), ALARM_NOT_RAISED); // initialize to no alarm
	}

	AlarmsRaisedVec::const_iterator it = alarmsRaised_m.begin();
	for (CORBA::ULong n = 0UL; n < valueSeq.length(); ++it, ++n)
	{
		if(*it != ALARM_NOT_RAISED && valueSeq[n] != property_mp->alarm_on())
		{
			std::ostringstream oss;
			std::string ts;
			oss << (valueSeq[n] ? "true" : "false") << std::ends;
			ts =  oss.str();

			setProperty("BACI_Value", ts.c_str());
			setProperty("BACI_Position", n);

			updateAlarm(n, DISABLE);
		} else if(*it == ALARM_NOT_RAISED && valueSeq[n] == property_mp->alarm_on()) {
			std::ostringstream oss;
			std::string ts;
			oss << (valueSeq[n] ? "true" : "false") << std::ends;
			ts =  oss.str();

			setProperty("BACI_Value", ts.c_str());
			setProperty("BACI_Position", n);

			updateAlarm(n, ENABLE);
		}
	}// for loop
}

}
