#ifndef _baci_alarm_pattern_H_
#define _baci_alarm_pattern_H_
/*******************************************************************************
 * ALMA - Atacama Large Millimiter Array
 * (c) European Southern Observatory, 2003 
 *
 *This library is free software; you can redistribute it and/or
 *modify it under the terms of the GNU Lesser General Public
 *License as published by the Free Software Foundation; either
 *version 2.1 of the License, or (at your option) any later version.
 *
 *This library is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *Lesser General Public License for more details.
 *
 *You should have received a copy of the GNU Lesser General Public
 *License along with this library; if not, write to the Free Software
 *Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 * "@(#) $Id: baciAlarmPattern.h,v 1.2 2008/02/26 15:06:15 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       2008-02-25  created
 */

/** 
 * @file 
 * Header file BACI Pattern Alarms.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciEvent.h>
#include <ACSErrTypeAlarm.h>

#include "baciAlarm_T.h"
#include "baciROpattern.h"

namespace baci {

/* AlarmEventStrategy implementation for pattern type */

    class baci_EXPORT AlarmEventStrategyPattern : public AlarmEventStrategyDisc<ACS::pattern, ROpatternImpl, ACS::Alarmpattern>
    {
      public:

	AlarmEventStrategyPattern(ROpatternImpl * property, EventDispatcher * eventDispatcher);
    
	AlarmEventStrategyPattern(Callback_ptr callback_p,
				  const CBDescIn& descIn,
				  const ACS::TimeInterval& interval,
				  ROpatternImpl * property,
				  EventDispatcher * eventDispatcher);
 
	virtual void check(BACIValue &value,
			   const ACSErr::Completion & c,
			   const ACS::CBDescOut & desc);
    
      private:
	  // lastValue that	  
	  ACS::pattern lastValue_m;

	  //size of pattern type in bits
	  const unsigned short patternSize_m;

	  // postion of bit 
	  unsigned short bitPos_m;


	  ACS::pattern alarmTrigger_m,
	  alarmMask_m;
	/**
	 * ALMA C++ coding standards state assignment operators should be disabled.
	 */
	void operator=(const AlarmEventStrategyPattern&);

	/**
	 * ALMA C++ coding standards state copy constructors should be disabled.
	 */
	AlarmEventStrategyPattern(const AlarmEventStrategyPattern&);
    };

}; //namespace baci

#endif /*!_H*/

