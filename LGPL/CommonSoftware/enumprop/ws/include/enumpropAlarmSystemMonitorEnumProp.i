
#include <baciAlarmSystemMonitor_T.i>

/*********************************** IMPLEMENTATION of AlarmSystemMonitorEnumProp */
template<class T, class TPROP>
AlarmSystemMonitorEnumProp<T, TPROP>::AlarmSystemMonitorEnumProp(TPROP * property, EventDispatcher * eventDispatcher) :
    AlarmSystemMonitor<TPROP>(property, eventDispatcher)
{
    ACS_TRACE("baci::AlarmSystemMonitorEnumProp&lt;&gt;::AlarmSystemMonitorEnumProp");
}//AlarmSystemMonitorEnumProp

template<class T, class TPROP>
AlarmSystemMonitorEnumProp<T, TPROP>::~AlarmSystemMonitorEnumProp()
{
    ACS_TRACE("baci::AlarmSystemMonitorEnumProp&lt;&gt;::~AlarmSystemMonitorEnumProp");
}//~AlarmSystemMonitorEnumProp


template<class T, class TPROP>
void AlarmSystemMonitorEnumProp<T, TPROP>::check(BACIValue &val,
						 const ACSErr::Completion & c,
						 const ACS::CBDescOut & desc )
{
  ACE_UNUSED_ARG(c);
  std::ostringstream ostr;
  std::string ts;
  ACS::pattern value = val.patternValue();
 
  if ( this->property_mp->checkAlarm(T(value)) && !this->alarmRaised_m )
      {	
      ostr << value << std::ends;
      ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
      ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s raised. Value change to: %s", this->property_mp->name(), ts.c_str()));
      this->sendAlarm("enumprop",this->property_mp->name(),1,true);
      this->alarmRaised_m = 1;
      this->alarmRaised_m = true;
      }
  if (!this->property_mp->checkAlarm(T(value)) && this->alarmRaised_m )
      {
      ostr << value << std::ends;
      ts =  ostr.str(); // we have to make a temporary string otherwise there is problem with memory:  s = ostr.str().c_str(); does not work
      ACS_SHORT_LOG((LM_ALERT, "Alarm for property: %s cleared. Value change to: %s", this->property_mp->name(), ts.c_str()));
      this->sendAlarm("enumprop",this->property_mp->name(),1,false);
      this->alarmRaised_m = false;
      }
}//check

