/*********************************** IMPLEMENTATION of AlarmSystemMonitorDisc for pattern type */

#ifndef ALARM_SYSTEM_MONITOR_PATTERN_T_I
#define ALARM_SYSTEM_MONITOR_PATTERN_T_I

#include "baciAlarmSystemMonitorDisc_T.i"
#include "baciROpattern.h"

template<>
void AlarmSystemMonitorDisc<ACS::pattern, ROpatternImpl>::check(BACIValue &val,
								const ACSErr::Completion & c,
								const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    std::ostringstream ostr;
    std::string ts;

    ACS::pattern value = val.getValue(static_cast<ACS::pattern*>(0)); //val.patternValue();

    if ((this->alarmRaised_m!=0) &&		// we have an alarm (0 indicates no alarm)
	(value>=this->property_mp->alarm_low_on()) && 
	(value<=this->property_mp->alarm_high_on()))
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s cleared. Value change to: %s", this->property_mp->name(), ts.c_str()));
	this->sendAlarm("BACIProperty",this->property_mp->name(),1,false);
	this->alarmRaised_m = 0;
	}
    else if ((this->alarmRaised_m!=-1) &&            // if not alarm low
	     (value<=this->property_mp->alarm_low_on()))
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s raised. Value change to: %s", this->property_mp->name(), ts.c_str()));
	this->sendAlarm("BACIProperty",this->property_mp->name(),1,true);
	this->alarmRaised_m = -1;
	}
    else if ((this->alarmRaised_m!=1) &&            // if not alarm hi 
	     (value>=this->property_mp->alarm_high_on()))
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s raised. Value change to: %s", this->property_mp->name(), ts.c_str()));
	this->sendAlarm("BACIProperty",this->property_mp->name(),1,true);
	this->alarmRaised_m = 1;
	}

}//check

#endif /* ALARM_SYSTEM_MONITOR_PATTERN_T_I */
