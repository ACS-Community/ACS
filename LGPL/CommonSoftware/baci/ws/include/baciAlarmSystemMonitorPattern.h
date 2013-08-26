#ifndef BACIALARMSYSTEMMONITORPATTERN_H_
#define BACIALARMSYSTEMMONITORPATTERN_H_

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciAlarmSystemMonitorDisc_T.h"
#include "baciROpattern.h"

namespace baci
{

    class ROpatternImpl;
/**
 * implementation of the AlarmSystemMonitorPattern for pattern 
 */
class baci_EXPORT AlarmSystemMonitorPattern : public AlarmSystemMonitorDisc<ACS::pattern, ROpatternImpl>
{
  public:
    
    AlarmSystemMonitorPattern(ROpatternImpl* property, EventDispatcher * eventDispatcher);

    virtual ~AlarmSystemMonitorPattern();
    
    virtual void check(BACIValue &val,
	       const ACSErr::Completion & c,
	       const ACS::CBDescOut & desc );
 
  protected:
    //we need specia version for pattern
    virtual void clearAlarm();

  private:
    // lastValue that	  
	ACS::pattern lastValue_m;
	
	//size of pattern type in bits
	const unsigned short patternSize_m;
	
	// postion of bit 
	unsigned short bitPos_m;
	
	
	ACS::pattern alarmTrigger_m,
				alarmMask_m;
	
	// descriptions for bits
	ACS::stringSeq *bitDescriptions_mp;
	
	// length of bitDescriptions (=number of bit descriptions)
	unsigned long bitDescLength_m;
	
    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const AlarmSystemMonitorPattern&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    AlarmSystemMonitorPattern(const AlarmSystemMonitorPattern&);
    
};//class AlarmSystemMonitorPattern

}//namespace baci
#endif /*BACIALARMSYSTEMPATTERN_H_*/
