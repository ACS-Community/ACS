
/*********************************** IMPLEMENTATION of AlarmSystemMonitorCont */
template<class T, class TPROP>
baci::AlarmSystemMonitorCont<T, TPROP>::AlarmSystemMonitorCont(TPROP * property, EventDispatcher * eventDispatcher) :
    baci::AlarmSystemMonitor<TPROP>(property, eventDispatcher)
{
    ACS_TRACE("baci::AlarmSystemMonitorCont&lt;&gt;::AlarmSystemMonitorCont");
}//AlarmSystemMonitorCont

template<class T, class TPROP>
baci::AlarmSystemMonitorCont<T, TPROP>::~AlarmSystemMonitorCont()
{
    ACS_TRACE("baci::AlarmSystemMonitorCont&lt;&gt;::~AlarmSystemMonitorCont");
}//~AlarmSystemMonitorCont


template<class T, class TPROP>
void baci::AlarmSystemMonitorCont<T, TPROP>::check(BACIValue &val,
		const ACSErr::Completion & c,
		const ACS::CBDescOut & desc
)
{

	ACE_UNUSED_ARG(c);
	std::ostringstream ostr;
	std::string ts;
	ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(this->faultStructMutex_m);

	T value = val.getValue(static_cast<T*>(0)); //val.patternValue();

	if ((this->alarmRaised_m!=0) &&		// we have an alarm (0 indicates no alarm)
			(value>=this->property_mp->alarm_low_off()) &&
			(value<=this->property_mp->alarm_high_off()))
	{
		ostr << value << std::ends;
		ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
		ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s cleared. Value changed to: %s", this->property_mp->name(), ts.c_str()));

		this->setProperty("BACI_Value", ts.c_str());
		this->clearAlarm(); //=	this->sendAlarm(this->lastAlarmFaultCode_m, false);
		this->alarmRaised_m = 0;
		this->lastAlarmValue_m = val;
	}
	else if ((this->alarmRaised_m!=-1) &&            // if not alarm low
			(value<=this->property_mp->alarm_low_on()))
	{
		ostr << value << std::ends;
		ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work

		ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s raised - value %s too low!.", this->property_mp->name(), ts.c_str()));

		this->setProperty("BACI_Value", ts.c_str());
		this->sendAlarm(2, true);
		this->alarmRaised_m = -1;
		this->lastAlarmValue_m = val;
	}
	else if ((this->alarmRaised_m!=1) &&            // if not alarm high
			(value>=this->property_mp->alarm_high_on()))
	{
		ostr << value << std::ends;
		ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work

		ACS_SHORT_LOG((LM_DEBUG, "Alarm for property: %s raised - value %s too high!", this->property_mp->name(), ts.c_str()));

		this->setProperty("BACI_Value", ts.c_str());
		this->sendAlarm(3, true);
		this->alarmRaised_m = 1;
		this->lastAlarmValue_m = val;
	}
}//check


