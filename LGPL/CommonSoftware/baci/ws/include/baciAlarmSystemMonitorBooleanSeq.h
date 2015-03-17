#ifndef BACIALARMSYSTEMMONITORBOOLEANSEQ_H_
#define BACIALARMSYSTEMMONITORBOOLEANSEQ_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciAlarmSystemMonitor_T.h"
#include "baciRObooleanSeq.h"
#include <vector>

namespace baci
{

typedef std::vector<int32_t> AlarmsRaisedVec;

/**
 * Implementation of the AlarmSystemMonitorBooleanSeq for booleanSeq
 */
class baci_EXPORT AlarmSystemMonitorBooleanSeq : public AlarmSystemMonitor<RObooleanSeq>
{
  public:
    
    AlarmSystemMonitorBooleanSeq(RObooleanSeq* property, EventDispatcher * eventDispatcher);

    virtual ~AlarmSystemMonitorBooleanSeq();
    
    virtual void check(BACIValue &val,
	       const ACSErr::Completion & c,
	       const ACS::CBDescOut & desc );
 
  private:

    static const bool ENABLE = true;
    static const bool DISABLE = false;
    static const int32_t ALARM_NOT_RAISED = 0;
    static const int32_t ALARM_RAISED = 1;

    AlarmsRaisedVec alarmsRaised_m;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmSystemMonitorBooleanSeq&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmSystemMonitorBooleanSeq(const AlarmSystemMonitorBooleanSeq&);
    
    /**
     * Update the alarm
     */
    void updateAlarm(int32_t pos,bool enable);


};//class AlarmSystemMonitorBooleanSeq

}//namespace baci

#endif /*BACIALARMSYSTEMBOOLEANSEQ_H_*/
