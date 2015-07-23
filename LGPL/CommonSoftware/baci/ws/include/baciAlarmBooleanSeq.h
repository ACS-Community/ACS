#ifndef BACIALARMBOOLEANSEQ_H_
#define BACIALARMBOOLEANSEQ_H_
/*******************************************************************
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
* "@(#) $Id: baciAlarmBooleanSeq"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* pcolomer  13/10/2014  created
*/


#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include <baciEvent.h>
#include <ACSErrTypeAlarm.h>

#include "baciAlarm_T.h"
#include "baciRObooleanSeq.h"

#include <vector>
#include <stdint.h>

namespace baci {

/***********************************************************************************************/
/* AlarmEventStrategy implementation for boolean type */

class baci_EXPORT AlarmEventStrategyBooleanSeq
				: public AlarmEventStrategy<ACS::booleanSeq, RObooleanSeq, ACS::Alarmboolean>
{
  public:
    AlarmEventStrategyBooleanSeq(RObooleanSeq * property, EventDispatcher * eventDispatcher);

    AlarmEventStrategyBooleanSeq(Callback_ptr callback_p,
			   const CBDescIn& descIn,
			   const ACS::TimeInterval& interval,
			   RObooleanSeq * property,
			   EventDispatcher * eventDispatcher);

    virtual void check(BACIValue &val,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);

  private:

    static const int32_t ALARM_NOT_RAISED = 0;
    static const int32_t ALARM_RAISED = 1;

    std::vector<int32_t> alarmsRaised_m;

    void checkItem(int32_t &alarmRaised, CORBA::Boolean value,
    		const ACSErr::Completion & c, const ACS::CBDescOut & desc);

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategyBooleanSeq&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategyBooleanSeq(const AlarmEventStrategyBooleanSeq&);
};

}

#endif /* BACIALARMBOOLEANSEQ_H_ */
