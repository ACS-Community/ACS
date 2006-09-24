
/*********************************** IMPLEMENTATION of AlarmSystemMonitorSeqCont */
template<class T, class TPROP>
AlarmSystemMonitorSeqCont<T, TPROP>::AlarmSystemMonitorSeqCont(TPROP * property, EventDispatcher * eventDispatcher) :
    AlarmSystemMonitor<TPROP>(property, eventDispatcher)
{
    ACS_TRACE("baci::AlarmSystemMonitorSeqCont&lt;&gt;::AlarmSystemMonitorSeqCont");
}//AlarmSystemMonitorSeqCont

template<class T, class TPROP>
AlarmSystemMonitorSeqCont<T, TPROP>::~AlarmSystemMonitorSeqCont()
{
    ACS_TRACE("baci::AlarmSystemMonitorSeqCont&lt;&gt;::~AlarmSystemMonitorSeqCont");
}//~AlarmSystemMonitorSeqCont


template<class T, class TPROP>
void AlarmSystemMonitorSeqCont<T, TPROP>::check(BACIValue &val,
					  const ACSErr::Completion & c,
					  const ACS::CBDescOut & desc
    )
{
    ACE_UNUSED_ARG(c);
    T valueSeq = val.getValue(static_cast<T*>(0));
    
    if (alarmsRaisedLength_m!=static_cast<int>(valueSeq.length()))
	{
	if (alarmsRaised_mp !=0)
	    { 
	    delete alarmsRaised_mp; 
	    alarmsRaised_mp = 0; 
	    alarmsRaisedLength_m = 0;
	}

      alarmsRaisedLength_m = valueSeq.length();
      alarmsRaised_mp = new int[alarmsRaisedLength_m];
      
      // initialize to no alarm
      for (int i = 0; i < alarmsRaisedLength_m; i++)
	  alarmsRaised_mp[i] = 0;
	}

  for (CORBA::ULong n = 0UL; n < valueSeq.length(); n++)
    {
      if ((alarmsRaised_mp[n]!=0) &&							// we have an alarm (0 indicates no alarm)
	  (valueSeq[n]>=this->property_mp->alarm_low_off()) && 
	  (valueSeq[n]<=this->property_mp->alarm_high_off()))
	{
      
//	    Completion c = ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
//	      this->callback_mp->alarm_cleared(valueSeq[n], c, desc);
//	      this->succeeded();
	      alarmsRaised_mp[n] = 0;
	}
      else if ((alarmsRaised_mp[n]!=-1) &&            // if not alarm low
	       (valueSeq[n]<=this->property_mp->alarm_low_on()))
	  {
//	  Completion c = ACSErrTypeAlarm::ACSErrAlarmLowCompletion();
//	  this->callback_mp->alarm_raised(valueSeq[n], c, desc);
//	  this->succeeded();
	  alarmsRaised_mp[n] = -1;
	  }
      else if ((alarmsRaised_mp[n]!=1) &&            // if not alarm hi 
	       (valueSeq[n]>=this->property_mp->alarm_high_on()))
	  {
//	  Completion c=ACSErrTypeAlarm::ACSErrAlarmHighCompletion();
//	    this->callback_mp->alarm_raised(valueSeq[n], c, desc);
//	    this->succeeded();
	    alarmsRaised_mp[n] = 1;
	  }
    }  // for loop
}//check
