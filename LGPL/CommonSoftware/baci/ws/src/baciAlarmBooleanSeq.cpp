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
 * "@(#) $Id: baciAlarmBooleanSeq.cpp"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * pcolomer  2014/10/13  First version
 */
#include "baciAlarmBooleanSeq.h"
#include "baciAlarm_T.i"

namespace baci {

AlarmEventStrategyBooleanSeq::AlarmEventStrategyBooleanSeq(RObooleanSeq * property,
		EventDispatcher * eventDispatcher)
 : AlarmEventStrategy<ACS::booleanSeq, RObooleanSeq, ACS::Alarmboolean>(property, eventDispatcher)
{
	ACS_TRACE("AlarmEventStrategyBooleanSeq::AlarmEventStrategyBooleanSeq");
}

AlarmEventStrategyBooleanSeq::AlarmEventStrategyBooleanSeq(Callback_ptr callback_p,
		   const CBDescIn& descIn, const ACS::TimeInterval& interval,
		   RObooleanSeq * property, EventDispatcher * eventDispatcher)
 : AlarmEventStrategy<ACS::booleanSeq, RObooleanSeq, ACS::Alarmboolean>(callback_p, descIn, interval,
		 property, eventDispatcher)
{
	ACS_TRACE("AlarmEventStrategyBooleanSeq::~AlarmEventStrategyBooleanSeq");
}


void AlarmEventStrategyBooleanSeq::check(BACIValue &val,
		   const ACSErr::Completion & c, const ACS::CBDescOut & desc)
{
	ACS_TRACE("AlarmEventStrategyBooleanSeq::check");

	ACE_UNUSED_ARG(c);
	ACS::booleanSeq valueSeq = val.getValue(static_cast<ACS::booleanSeq*>(0));

	if(alarmsRaised_m.size() != static_cast<size_t>(valueSeq.length()))
	{
		alarmsRaised_m.clear();
		alarmsRaised_m.resize(valueSeq.length(), ALARM_NOT_RAISED);
	}

	for (CORBA::ULong i = 0UL; i < valueSeq.length(); ++i)
	{
		checkItem(alarmsRaised_m[i], valueSeq[i], c, desc);
	}
}

void AlarmEventStrategyBooleanSeq::checkItem(int32_t &alarmRaised, CORBA::Boolean value,
		const ACSErr::Completion & c, const ACS::CBDescOut & desc)
{
	ACS_TRACE("AlarmEventStrategyBooleanSeq::checkItem");
	if ((alarmRaised != ALARM_NOT_RAISED) && (value != property_mp->alarm_on())) { // we have an alarm (0 indicates no alarm) & "Off"
		try {
			Completion c = ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
			callback_mp->alarm_cleared(value, c, desc);

			succeeded();
			alarmRaised = ALARM_NOT_RAISED;
		} catch(...) {
			if (failed() == true)
			{
				destroy();
			}
		}

	} else if ((alarmRaised == ALARM_NOT_RAISED) && (value == property_mp->alarm_on())) { // no alarm for now & alarm state
		try {
			Completion c;
			c.timeStamp = getTimeStamp();
			c.type = ACSErr::ACSErrTypeAlarm;     // alarm
			c.code = ACSErrTypeAlarm::ACSErrAlarmHigh;     // high

			callback_mp->alarm_raised(value, c, desc);

			succeeded();
			alarmRaised = ALARM_RAISED;
		} catch(...) {
			if (failed() == true)
			{
				destroy();
			}
		}
	}
}

}
