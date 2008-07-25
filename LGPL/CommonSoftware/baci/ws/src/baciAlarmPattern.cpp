#include "baciAlarmPattern.h"
#include "baciAlarm_T.h"
#include "baciAlarm_T.i"
#include <PatternAlarmTriggered.h>
#include <PatternAlarmCleared.h>

using namespace baci;

AlarmEventStrategyPattern::AlarmEventStrategyPattern(ROpatternImpl * property, EventDispatcher * eventDispatcher):
	AlarmEventStrategyDisc<ACS::pattern, ROpatternImpl, ACS::Alarmpattern>(property, eventDispatcher),
	patternSize_m(sizeof(ACS::pattern)*8)
{
	// setting lastValue to NOT of alarm_trigger will trigger alarms at the first check
	lastValue_m = ~(property_mp->alarm_trigger());
};

AlarmEventStrategyPattern::AlarmEventStrategyPattern(Callback_ptr callback_p,
		const CBDescIn& descIn,
		const ACS::TimeInterval& interval,
		ROpatternImpl * property,
		EventDispatcher * eventDispatcher) :
			AlarmEventStrategyDisc<ACS::pattern, ROpatternImpl, ACS::Alarmpattern>(callback_p, descIn, interval, property, eventDispatcher), 
			patternSize_m(sizeof(ACS::pattern)*8)
			{
	// setting lastValue to NOT of alarm_trigger will trigger alarms at the first check
	lastValue_m = ~(property_mp->alarm_trigger());
			};

void AlarmEventStrategyPattern::check(BACIValue &val,
				      const ACSErr::Completion & c,
				      const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    ACS::pattern value = val.getValue(static_cast<ACS::pattern*>(0)); 
    
    // first we just check if the value has been changed since last time
    if (value!=lastValue_m) 
    {
    	// here we read the values every time! This can be done just once in ctor, but what if values in CDB change
    	alarmMask_m = property_mp->alarm_mask();
    	alarmTrigger_m = property_mp->alarm_trigger();
    	ACS::pattern tmpVal = value; // temporary storage of the value
    	for(bitPos_m=0; bitPos_m<patternSize_m; bitPos_m++)
    	{
    		//we check if the bit at position bitPos has been changed
    		if ((alarmMask_m & (value ^ lastValue_m ) & 1) )
    		{
    			
    			// how the bit has changed (we could merge this with if above but would	 not be cleare then)	
    			if ( ~(value ^ alarmTrigger_m) & 1 )
    			{
    				//here should be PatternAlarmTriggered::TriggeredBit[bitPos_m]Completion
    				// ... but would be too complicated to implement
    				Completion c = PatternAlarmTriggered::TriggeredBit0Completion(); 
    				c.code = bitPos_m; // code contains bit position
    				callback_mp->alarm_raised(value, c, desc);
    				succeeded();
    			}
    			else
    			{
    				//here should be PatternAlarmCleared::ClearedBit[bitPos_m]Completion
    				// ... but would be too complicated to implement
    				Completion c = PatternAlarmCleared::ClearedBit0Completion(); 
    				c.code = bitPos_m; // code contains bit position
    				callback_mp->alarm_cleared(value, c, desc);
    				succeeded();
    			}//if-else
    		}//if
    		// shift to the next bit 
    		value >>= 1;
    		lastValue_m >>= 1;
    		alarmMask_m >>= 1;
    		alarmTrigger_m >>= 1;
    	}//for
    	lastValue_m = tmpVal;
    }//if
        
}//check
