#include <Basic_Types.h> // for ACE_UINT64_FORMAT_SPECIFIER_ASCII

#include "baciROpattern.h"
#include "baciAlarmSystemMonitorPattern.h"
#include "baciAlarmSystemMonitor_T.i"
#include "baciAlarmSystemMonitorDisc_T.i"

namespace baci
{

AlarmSystemMonitorPattern::AlarmSystemMonitorPattern(ROpatternImpl* property,EventDispatcher * eventDispatcher)
	: AlarmSystemMonitorDisc<ACS::pattern, ROpatternImpl>(property, eventDispatcher),
	patternSize_m(sizeof(ACS::pattern)*8)
{
	// setting lastValue to NOT of alarm_trigger will trigger alarms at the first check
	lastValue_m = ~(property_mp->alarm_trigger());
}//AlarmSystemMonitorPattern


AlarmSystemMonitorPattern::~AlarmSystemMonitorPattern()
{
}//~AlarmSystemMonitorPattern

void AlarmSystemMonitorPattern::clearAlarm()
{
	ACE_TRACE("baci::AlarmSystemMonitorPattern::clearAlarm");
	ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(faultStructMutex_m);

	alarmMask_m = property_mp->alarm_mask();
	alarmTrigger_m = property_mp->alarm_trigger();
	bitDescriptions_mp = property_mp->bitDescription();

	ACS::pattern tmpVal = lastValue_m; //tmp storage of the value
	// here we artifficaly set the last value, so that next check can trigger alarm(s)
	lastValue_m = (~alarmMask_m & lastValue_m) | (alarmMask_m & ~alarmTrigger_m);

	for(bitPos_m=0; bitPos_m<patternSize_m; bitPos_m++)
	{
		// we check if bit by bit
		if ((alarmMask_m & ~(alarmTrigger_m ^ tmpVal ) & 1) )
		{
			setProperty("BACI_BitPosition", bitPos_m);
			const char *bitDesc;
			if (bitPos_m<bitDescriptions_mp->length())
			{
			   bitDesc = (*bitDescriptions_mp)[bitPos_m];
			}
			else
			{
				bitDesc = "w/o description";
			}
			setProperty("BACI_BitDescription", bitDesc);
			setProperty("BACI_Value", tmpVal);
			ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s, bit %d (%s) cleared. Value change to: "ACE_UINT64_FORMAT_SPECIFIER_ASCII, property_mp->name(), bitPos_m, bitDesc, tmpVal));
			this->sendAlarm(1, false);
			this->alarmRaised_m --;
		}//if
		// shift to the next bit
		tmpVal >>= 1;
		alarmMask_m >>= 1;
		alarmTrigger_m >>= 1;
	}//for
}//clearAlarm

void AlarmSystemMonitorPattern::check(BACIValue &val,
								const ACSErr::Completion & c,
								const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(faultStructMutex_m);

    ACS::pattern value = val.getValue(static_cast<ACS::pattern*>(0)); //val.patternValue();
    
    // first we just check if the value has been changed since last time
    if (value!=lastValue_m) 
    {
    	// here we read the values every time! This can be done just once in ctor, but what if values in CDB change
    	alarmMask_m = property_mp->alarm_mask();
    	alarmTrigger_m = property_mp->alarm_trigger();
    	bitDescriptions_mp = property_mp->bitDescription();

    	ACS::pattern tmpVal = value; //tmp storage of the value

    	for(bitPos_m=0; bitPos_m<patternSize_m; bitPos_m++)
    	{
    		//we check if the bit at position bitPos has been changed
    		if ((alarmMask_m & (value ^ lastValue_m ) & 1) )
    		{
    			setProperty("BACI_BitPosition", bitPos_m);
    			const char *bitDesc;
    			if (bitPos_m<bitDescriptions_mp->length())
    			{
    			   bitDesc = (*bitDescriptions_mp)[bitPos_m];
    			}
    			else
    			{
    				bitDesc = "w/o description";
    			}
    			setProperty("BACI_BitDescription", bitDesc);
    			setProperty("BACI_Value", value);

    			// how the bit has changed (we could merge this with if above but would	 not be cleared then)
    			if ( ~(value ^ alarmTrigger_m) & 1 )
    			{
    				ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s, bit %d (%s) raised. Value change to: "ACE_UINT64_FORMAT_SPECIFIER_ASCII, property_mp->name(), bitPos_m, bitDesc, value));
    				this->sendAlarm(1, true);
    				this->lastAlarmValue_m = val;
    				this->alarmRaised_m ++;
      			}
    			else
    			{
    				ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s, bit %d (%s) cleared. Value change to: "ACE_UINT64_FORMAT_SPECIFIER_ASCII, property_mp->name(), bitPos_m, bitDesc, value));
    				this->sendAlarm(1, false);
    				this->lastAlarmValue_m = val;
    				this->alarmRaised_m --;
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

}//namespace baci
