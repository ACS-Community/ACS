
/*********************************** IMPLEMENTATION of AlarmSystemMonitorDisc */

template<class T, class TPROP>
baci::AlarmSystemMonitorDisc<T, TPROP>::AlarmSystemMonitorDisc(TPROP * property, EventDispatcher * eventDispatcher) :
    baci::AlarmSystemMonitor<TPROP>(property, eventDispatcher)
{
    ACS_TRACE("baci::AlarmSystemMonitorDisc&lt;&gt;::AlarmSystemMonitorDisc");
}//AlarmSystemMonitorDisc

template<class T, class TPROP>
baci::AlarmSystemMonitorDisc<T, TPROP>::~AlarmSystemMonitorDisc()
{
    ACS_TRACE("baci::AlarmSystemMonitorDisc&lt;&gt;::~AlarmSystemMonitorDisc");
}//~AlarmSystemMonitorDisc

template<class T, class TPROP>
void baci::AlarmSystemMonitorDisc<T, TPROP>::check(BACIValue &val,
					  const ACSErr::Completion & c,
					  const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    std::ostringstream ostr;
    std::string ts;
    ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(this->faultStructMutex_m);

    T value = val.getValue(static_cast<T*>(0));

/// @todo check if strategy for sending alarms for the discret types (patter) is OK
// copied from Alarmpattern.cpp
    if ((this->alarmRaised_m!=0) &&    // we have an alarm (0 indicates no alarm)
	(value<=1))              // "On" or "Off"
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s cleared. Value change to: %s", this->property_mp->name(), ts.c_str()));

	this->setProperty("BACI_Value", ts.c_str());
	this->clearAlarm();//=this->sendAlarm(1, false);
    this->alarmRaised_m = 0;
    this->lastAlarmValue_m = val;
	}
    else if ((this->alarmRaised_m==0) &&                   // no alarm for now
	     (value>1))                              // alarm state
	{
	ostr << value << std::ends;
	ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
	ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s raised. Value change to: %s", this->property_mp->name(), ts.c_str()));

	this->setProperty("BACI_Value", ts.c_str());
	this->sendAlarm(1, true);
	this->alarmRaised_m = 1;
	this->lastAlarmValue_m = val;
	} //if
}//check
