
/*********************************** IMPLEMENTATION of AlarmSystemMonitorSeqDisc */
template<class TPROP>
AlarmSystemMonitorSeqDisc<T, TPROP>::AlarmSystemMonitorSeqDisc(TPROP * property, EventDispatcher * eventDispatcher) :
    AlarmSystemMonitor<TPROP>(property, eventDispatcher)
{
    ACS_TRACE("baci::AlarmSystemMonitorSeqDisc&lt;&gt;::AlarmSystemMonitorSeqDisc");
}//AlarmSystemMonitorSeqDisc

template<class TPROP>
AlarmSystemMonitorSeqDisc<T, TPROP>::~AlarmSystemMonitorSeqDisc()
{
    ACS_TRACE("baci::AlarmSystemMonitorSeqDisc&lt;&gt;::~AlarmSystemMonitorSeqDisc");
}//~AlarmSystemMonitorSeqDisc

template<class TPROP>
void AlarmSystemMonitorSeqDisc<T, TPROP>::check(BACIValue &val,
						const ACSErr::Completion & c,
						const ACS::CBDescOut & desc )
{
    ACE_UNUSED_ARG(c);
    ACE_Guard<ACE_Recursive_Thread_Mutex>  protSect(this->faultStructMutex_m);

    T valueSeq = val.getValue(static_cast<T*>(0));
    
    if (alarmsRaisedLength_m!=static_cast<int>(valueSeq.length()))
	{
	if (alarmsRaised_mp != 0)
	    { 
	    delete alarmsRaised_mp; 
	    alarmsRaised_mp = 0; 
	    alarmsRaisedLength_m = 0;
	    }

	alarmsRaisedLength_m = valueSeq.length();
	alarmsRaised_mp = new int[alarmsRaisedLength_m];
      
	// initialize to no alarm
	for (int i = 0; i < alarmsRaisedLength_m; i++)  alarmsRaised_mp[i] = 0;
	}

    for (CORBA::ULong n = 0UL; n < valueSeq.length(); n++)
	{
	if ((alarmsRaised_mp[n]!=0))
	    {
/// %todo implementation missing      
	    alarmsRaised_mp[n] = 0;
	    }
      else if ((alarmsRaised_mp[n]!=-1))
	  {
/// %todo implementation missing   	  
	  alarmsRaised_mp[n] = -1;
	  }
	else if ((alarmsRaised_mp[n]!=1))
	    {
/// %todo implementation missing
	    alarmsRaised_mp[n] = 1;
	    }
	}  // for loop
}//check
