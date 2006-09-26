
/*********************************** IMPLEMENTATION of AlarmSystemMonitorDisc */

template<class T, class TPROP>
AlarmSystemMonitorDisc<T, TPROP>::AlarmSystemMonitorDisc(TPROP * property, EventDispatcher * eventDispatcher) :
    AlarmSystemMonitor<TPROP>(property, eventDispatcher)
{
    ACS_TRACE("baci::AlarmSystemMonitorDisc&lt;&gt;::AlarmSystemMonitorDisc");
}//AlarmSystemMonitorDisc

template<class T, class TPROP>
AlarmSystemMonitorDisc<T, TPROP>::~AlarmSystemMonitorDisc()
{
    ACS_TRACE("baci::AlarmSystemMonitorDisc&lt;&gt;::~AlarmSystemMonitorDisc");
}//~AlarmSystemMonitorDisc

template<class T, class TPROP>
void AlarmSystemMonitorDisc<T, TPROP>::check(BACIValue &val,
					  const ACSErr::Completion & c,
					  const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    std::ostringstream ostr;
    std::string ts;

    T value = val.getValue(static_cast<T*>(0));

/// @todo check if strategy for sending alarms for the discret types (patter) is OK
// copied from Alarmpattern.cpp
    if ((this->alarmRaised_m!=0) &&    // we have an alarm (0 indicates no alarm)
	(value<=1))              // "On" or "Off"
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s cleared. Value change to: %s", this->property_mp->name(), ts.c_str()));
	this->sendAlarm("BACIProperty",this->property_mp->name(),1,false);

	  this->alarmRaised_m = 0;
	}
    else if ((this->alarmRaised_m==0) &&                   // no alarm for now
	     (value>1))                              // alarm state
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s raised. Value change to: %s", this->property_mp->name(), ts.c_str()));
	this->sendAlarm("BACIProperty",this->property_mp->name(),1,true);
	this->alarmRaised_m = 1;
	} //if
}//check
