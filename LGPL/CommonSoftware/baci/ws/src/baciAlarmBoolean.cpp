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
#include "baciAlarmBoolean.h"
#include "baciAlarm_T.i"

namespace baci {

AlarmEventStrategyBoolean::AlarmEventStrategyBoolean(ROboolean * property,
		EventDispatcher * eventDispatcher)
 : AlarmEventStrategy<CORBA::Boolean, ROboolean, ACS::Alarmboolean>(property, eventDispatcher)
{}

AlarmEventStrategyBoolean::AlarmEventStrategyBoolean(Callback_ptr callback_p,
		   const CBDescIn& descIn, const ACS::TimeInterval& interval,
		   ROboolean * property, EventDispatcher * eventDispatcher)
 : AlarmEventStrategy<CORBA::Boolean, ROboolean, ACS::Alarmboolean>(callback_p, descIn, interval,
		 property, eventDispatcher)
{}

void AlarmEventStrategyBoolean::check(BACIValue &val,
		   const ACSErr::Completion & c, const ACS::CBDescOut & desc)
{
	ACE_UNUSED_ARG(c);
	CORBA::Boolean value = val.getValue(static_cast<CORBA::Boolean*>(0));

	if ((this->alarmRaised_m != 0) && (value != this->property_mp->alarm_on())) { // we have an alarm (0 indicates no alarm) & "Off"
		try {
			Completion c = ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
			this->callback_mp->alarm_cleared(value, c, desc);

			this->succeeded();
			this->alarmRaised_m = 0;
		} catch(...) {
			if (this->failed() == true)
			{
				this->destroy();
			}
		}
	} else if ((this->alarmRaised_m == 0) && (value == this->property_mp->alarm_on())) { // no alarm for now & alarm state
		try {
			Completion c;
			c.timeStamp = getTimeStamp();
			c.type = ACSErr::ACSErrTypeAlarm;     // alarm
			c.code = ACSErrTypeAlarm::ACSErrAlarmHigh;     // high

			this->callback_mp->alarm_raised(value, c, desc);

			this->succeeded();
			this->alarmRaised_m = 1;
		} catch(...) {
			if (this->failed() == true)
			{
				this->destroy();
			}
		}
	}
}

}
