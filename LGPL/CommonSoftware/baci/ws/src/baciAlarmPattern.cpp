#include "baciAlarmPattern.h"

void AlarmEventStrategyPattern::check(BACIValue &val,
				      const ACSErr::Completion & c,
				      const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    ACS::pattern value = val.getValue(static_cast<ACS::pattern*>(0)); //val.patternValue();

    if ((this->alarmRaised_m!=0) &&				// we have an alarm (0 indicates no alarm)
	(value>=this->property_mp->alarm_low_on()) && 
	(value<=this->property_mp->alarm_high_on()))
	{
      
	try
	    { 
	    Completion c=ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
	  
	    this->callback_mp->alarm_cleared(value, c, desc);
	  
	    this->succeeded();
	    this->alarmRaised_m = 0;
	    }
	catch(...)
	    {
	    if (this->failed()==true) 
		this->destroy();
	    }
	}
    else if ((this->alarmRaised_m!=-1) &&            // if not alarm low
	     (value<=this->property_mp->alarm_low_on()))
	{
      
	try
	    {
	  
	    Completion c=ACSErrTypeAlarm::ACSErrAlarmLowCompletion();
	  
	    this->callback_mp->alarm_raised(value, c, desc);
	  
	    this->succeeded();
	    this->alarmRaised_m = -1;
	    }
	catch(...)
	    {
	    if (this->failed()==true)
		this->destroy();
	    }
	}
    else if ((this->alarmRaised_m!=1) &&            // if not alarm hi 
	     (value>=this->property_mp->alarm_high_on()))
	{
      
	try
	    {
	  
	    Completion c= ACSErrTypeAlarm::ACSErrAlarmHighCompletion();
		  
	    this->callback_mp->alarm_raised(value, c, desc);
	  
	    this->succeeded();
	    this->alarmRaised_m = 1;
	    }
	catch(...)
	    {
	    if (this->failed()==true)
		this->destroy();
	    }
	}
  
}
