/*
 * baciAlarmBoolean.h
 *
 *  Created on: Oct 13, 2014
 *      Author: almamgr
 */

#ifndef BACIALARMBOOLEAN_H_
#define BACIALARMBOOLEAN_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif


#include <baciEvent.h>
#include <ACSErrTypeAlarm.h>

#include "baciAlarm_T.h"
#include "baciROboolean.h"



namespace baci {

/***********************************************************************************************/
/* AlarmEventStrategy implementation for boolean type */

class baci_EXPORT AlarmEventStrategyBoolean : public AlarmEventStrategy<CORBA::Boolean, ROboolean, ACS::Alarmboolean>
{
  public:
    AlarmEventStrategyBoolean(ROboolean * property, EventDispatcher * eventDispatcher);

    AlarmEventStrategyBoolean(Callback_ptr callback_p,
			   const CBDescIn& descIn,
			   const ACS::TimeInterval& interval,
			   ROboolean * property,
			   EventDispatcher * eventDispatcher);

    virtual void check(BACIValue &val,
		       const ACSErr::Completion & c,
		       const ACS::CBDescOut & desc);

  private:
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmEventStrategyBoolean&);

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmEventStrategyBoolean(const AlarmEventStrategyBoolean&);
};

}

#endif /* BACIALARMBOOLEAN_H_ */
