/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

#include <Basic_Types.h> // for ACE_UINT64_FORMAT_SPECIFIER_ASCII
#include "baciAlarmSystemMonitor_T.i"

/*********************************** IMPLEMENTATION MonitorEventDispatcher *************************************/


template<class T, class TCB, class POA_CB>
baci::MonitorEventDispatcher<T, TCB, POA_CB>::MonitorEventDispatcher(const CBDescIn& descIn,
							       const ACS::TimeInterval& interval,
							       BACIProperty * property) : 
    EventDispatcher(), monitorCallback_mp(0), callbackServant_mp(0)
{
    ACS_TRACE("baci::MonitorEventDispatcher&lt;&gt;::MonitorEventDispatcher&lt;&gt;");
    
    // create monitor callback
    callbackServant_mp = new EventCB<T, TCB, POA_CB>(this);        // !!!! destruction of the servant
    ACE_CString cbName = ACE_CString(property->getName())+"_callbackEventDispatcher";
    monitorCallback_mp = TCB::_narrow(BACI_CORBA::ActivateCORBAObject(callbackServant_mp, cbName.c_str()));
    if (CORBA::is_nil(monitorCallback_mp)==true)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "MonitorEventDispatcher&lt;&gt;::MonitorEventDispathcer&lt;&gt;",
		(LM_ERROR, "Failed to activate CORBA object '%s'", cbName.c_str()));
	}
    else /* do not have this line if _var is used */
	{
	callbackServant_mp->_remove_ref();
	}
                                                                                                                       
    BACIValue bv(static_cast<T>(0)); // in this way we get type of BACIValue	
    callbackID_m = property->getComponent()->registerCallback(bv.getType(),
							      	monitorCallback_mp, 
							      	descIn);
    
    // generate appropriate name
    ACS_NEW_RETURN(monitor_mp, 
		   BACIMonitor(ACE_CString(property->getName())+"_monitorEventDispatcher", 
			       callbackID_m, this, 
			       interval, BACIValue::NullValue, 
			       0, BACIValue::NullValue, 
			       property, 0),
		   );

}

template<class T, class TCB, class POA_CB>
baci::MonitorEventDispatcher<T, TCB, POA_CB>::~MonitorEventDispatcher()
{
    ACS_TRACE("baci::MonitorEventDispatcher&lt;&gt;::~MonitorEventDispatcher&lt;&gt;");
    if (monitorCallback_mp!=0) 
	{
	callbackServant_mp->disposeDispatcher(); 
	}
    if (monitor_mp!=0) 
	{ 
	monitor_mp->destroy(); 
	monitor_mp = 0; 
	}
    if (monitorCallback_mp!=0) 
	{ 
	BACI_CORBA::DestroyCORBAObject(monitorCallback_mp/*.in()*/); 
	}
}

template<class T, class TCB, class POA_CB>
int baci::MonitorEventDispatcher<T, TCB, POA_CB>::subscribe(EventStrategy * event)
{
    int res = EventDispatcher::subscribe(event);
    if (event->isSuspended()==false) 
	{
	resume();
	}
    return res;
}

template<class T, class TCB, class POA_CB>
int baci::MonitorEventDispatcher<T, TCB, POA_CB>::unsubscribe(EventStrategy * event)
{
    int res = EventDispatcher::unsubscribe(event);
    if (event->isSuspended()==false) 
	{
	suspend();
	}
    return res;
}

template<class T, class TCB, class POA_CB>
void baci::MonitorEventDispatcher<T, TCB, POA_CB>::dispatch(T value,
						      const ACSErr::Completion & c,
						      const ACS::CBDescOut & desc)
{
    getMutex().acquire();
    EventStrategyVector subscribers = EventStrategyVector(getSubscribers());    // copy
    
    for (EventStrategyVector::iterator iter = subscribers.begin();
	 iter != subscribers.end(); 
	 iter++)
	{
	BACIValue val(value);
	(*iter)->check(val, c, desc);
	}
    getMutex().release();
}

template<class T, class TCB, class POA_CB>
void baci::MonitorEventDispatcher<T, TCB, POA_CB>::suspend()
{
    if (active_m==1 && (monitor_mp!=0))
	{
	monitor_mp->suspend();
	}
    active_m--;
}

template<class T, class TCB, class POA_CB>
void baci::MonitorEventDispatcher<T, TCB, POA_CB>::resume()
{
    if (active_m==0 && (monitor_mp!=0))
	{
	monitor_mp->resume();
	}
    active_m++;
}

template<class T, class TCB, class POA_CB>
void baci::MonitorEventDispatcher<T, TCB, POA_CB>::monitorDestroyed()
{
    if (monitor_mp != 0) 
	{ 
	monitor_mp->destroy(); 
	monitor_mp = 0; 
	}
}

template<class T, class TCB, class POA_CB>
void baci::MonitorEventDispatcher<T, TCB, POA_CB>::monitorStateChanged()
{
}

/*********************************** IMPLEMENTATION EventCB *************************************/

template<class T, class TCB, class POA_CB>
baci::EventCB<T, TCB, POA_CB>::EventCB(baci::MonitorEventDispatcher<T, TCB, POA_CB>* dispatcher)
{
    dispatcher_mp = dispatcher;
}

template<class T, class TCB, class POA_CB>
baci::EventCB<T, TCB, POA_CB>::~EventCB()
{
    ACS_TRACE("baci::EventCB&lt;&gt;::~EventCB&lt;&gt;");
}

template<class T, class TCB, class POA_CB>
void baci::EventCB<T, TCB, POA_CB>::disposeDispatcher()
{
    dispatcher_mp = 0;
}

template<class T, class TCB, class POA_CB>
void baci::EventCB<T, TCB, POA_CB>::working (T value,
				       const ACSErr::Completion & c,
				       const ACS::CBDescOut & desc)
{
     if (dispatcher_mp != 0)
	{
	dispatcher_mp->dispatch(value, c, desc);
	}
}

template<class T, class TCB, class POA_CB>
void baci::EventCB<T, TCB, POA_CB>::done (T value,
				    const ACSErr::Completion & c,
				    const ACS::CBDescOut & desc)
{
    if (dispatcher_mp != 0)
	{
	dispatcher_mp->dispatch(value, c, desc);
	}
}

template<class T, class TCB, class POA_CB>
CORBA::Boolean baci::EventCB<T, TCB, POA_CB>:: negotiate (ACS::TimeInterval time_to_transmit,
						    const ACS::CBDescOut & desc)
{
    ACE_UNUSED_ARG(time_to_transmit);
    ACE_UNUSED_ARG(desc);
    return false;
}

/*********************************** IMPLEMENTATION AlarmEventStrategy **********************/

template<class T, class TPROP, class TALARM>
const int baci::AlarmEventStrategy<T, TPROP, TALARM>::maxFailureCount = 3;

template<class T, class TPROP, class TALARM>
baci::AlarmEventStrategy<T, TPROP, TALARM>::AlarmEventStrategy(TPROP * property,
						     EventDispatcher * eventDispatcher) :
  failureCount_m(0),eventDispatcher_mp(eventDispatcher),  property_mp(property), alarmRaised_m(0)
     
{
  // call recovery
}

template<class T, class TPROP, class TALARM>
baci::AlarmEventStrategy<T, TPROP, TALARM>::AlarmEventStrategy(Callback_ptr callback_p,
						     const CBDescIn& descIn,
						     const ACS::TimeInterval& interval,
						     TPROP * property,
						     EventDispatcher * eventDispatcher) :
    suspended_m(false), failureCount_m(0), desc_mIn(descIn), interval_m(interval),
    eventDispatcher_mp(eventDispatcher), property_mp(property), alarmRaised_m(0)
{
  
  ACS_TRACE("baci::AlarmEventStrategy&lt;&gt;::AlarmEventStrategy&lt;&gt;");
  
  name_m = ACE_CString(property->getProperty()->getName())+"_event";
  char* cname_p = BACIRecoveryManager::getInstance()->generateObjectName(name_m.c_str());
  name_m = cname_p;
  delete[] cname_p;
  
  ACS_DEBUG_PARAM("AlarmEventStrategy&lt;&gt;::AlarmEventStrategy&lt;&gt;", "Name is: '%s'", name_m.c_str());
  
  // !!!!
  callback_mp = TALARM::_narrow(callback_p);
  if (callback_mp/*.ptr()*/ == TALARM::_nil())
    {
      // report error
      return;
    }

  reference_mp = BACI_CORBA::ActivateCORBAObject(this, name_m.c_str());
  if (CORBA::is_nil(reference_mp)==true)
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmEventStrategy&lt;&gt;::AlarmEventStrategy&lt;&gt;",
	      (LM_ERROR, "Failed to activate CORBA object '%s'", name_m.c_str()));
    }
  else
    this->_remove_ref();
  
  // subscribe to event dispatcher
  eventDispatcher_mp->subscribe(this);
  
  BACIRecoveryManager::getInstance()->addRecoverableObject(this);
  
}

template<class T, class TPROP, class TALARM>
baci::AlarmEventStrategy<T, TPROP, TALARM>::~AlarmEventStrategy()
{
  ACS_TRACE("baci::AlarmEventStrategy&lt;&gt;::~AlarmEventStrategy&lt;&gt;");
  // unsubscribe to event dispatcher
  eventDispatcher_mp->unsubscribe(this);
}

template<class T, class TPROP, class TALARM>
bool baci::AlarmEventStrategy<T, TPROP, TALARM>::failed()
{
  return (++failureCount_m>=maxFailureCount);
}

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategy<T, TPROP, TALARM>::succeeded()
{
  failureCount_m = 0;
}

template<class T, class TPROP, class TALARM>
int baci::AlarmEventStrategy<T, TPROP, TALARM>::getId(void)
{
  return -1;
}

template<class T, class TPROP, class TALARM>
const char* baci::AlarmEventStrategy<T, TPROP, TALARM>::getName(void)
{
  return name_m.c_str();
}

template<class T, class TPROP, class TALARM>
const char* baci::AlarmEventStrategy<T, TPROP, TALARM>::getObjectState(void)
{
  ACE_TCHAR *buffer_p = 0;
  ACE_NEW_RETURN (buffer_p, ACE_TCHAR[MAX_RECORD_SIZE], 0);
  
  
  try 
    {
      
    CORBA::String_var ior = BACI_CORBA::getORB()->object_to_string(callback_mp/*.in()*/ 
								     );
      
      
      unsigned long tag = desc_mIn.id_tag;
      
      // !!! VxWorks support !!! Lx is not ANSI C and since VxWorks does not support
      // (U)LongLong, flag L will not work
      
      // name, ior, tag, time trigger, isSuspended
/*#ifdef MAKE_VXWOKRS
      ACE_OS::sprintf(buffer_p, "%s %s %lu %s %u",
		      getName(), ior.in(), tag, 
		      printLLUasString(interval_m),
		      suspended_m);
#else
*/      ACE_OS::sprintf(buffer_p, "%s %s %lu %llu %u",
		      getName(), ior.in(), tag, interval_m,
		      suspended_m);
//#endif
      
      return buffer_p;
      
    }
  catch(...) 
    { 
      // log debug
    }
    
  return 0;
}

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategy<T, TPROP, TALARM>::setObjectState(const char * state)
{
  ACE_TCHAR cname[MAX_NAME_LENGTH];
  ACE_TCHAR ior[MAX_IOR_LENGTH];
  
  unsigned long tag;
  unsigned int isSuspended;
  
  // name, ior, tag, time trigger, isSuspended
/*#ifdef MAKE_VXWORKS
  char *tmpPtr;
  sscanf(state, "%s %s %lu %s %u", 
	 cname, ior, &tag, tmpPtr, &isSuspended);
  interval_m = convString2LLU(tmpPtr);
#else
*/
  sscanf(state, "%s %s %lu "ACE_UINT64_FORMAT_SPECIFIER_ASCII" %u", 
	 cname, ior, &tag, &interval_m, &isSuspended);
//#endif
  /*
  ACE_OS::printf("R) Name: %s\n", cname);
  ACE_OS::printf("R) IOR: %s\n", ior);
  ACE_OS::printf("R) id_tag: %lu\n", tag);
  ACE_OS::printf("R) interval: %llu\n", interval_m);
  ACE_OS::printf("R) isSuspended: %u\n", isSuspended);
  */

  desc_mIn.id_tag = tag;
  suspended_m = isSuspended;
  
  name_m = cname;
  
  
  try 
    {      
      CORBA::Object_var obj = BACI_CORBA::getORB()->string_to_object(ior);
      
      
      if (CORBA::is_nil(obj.in())==true) 
	{
	  // log debug.
	  return;
	}
      
      callback_mp = TALARM::_narrow(obj.in());
      
      
      if (callback_mp/*.ptr()*/ == TALARM::_nil())
	{
	  // report error
	  return;
	}
      
      // generate appropriate name
      ACS_DEBUG_PARAM("AlarmEventStrategy&lt;&gt;::setObjectState", "Recovering: '%s'", name_m.c_str());
      
      reference_mp = BACI_CORBA::ActivateCORBAObject(this, name_m.c_str());
      if (CORBA::is_nil(reference_mp)==true)
	{
	  ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmEventStrategy&lt;&gt;::setObjectState",
		  (LM_ERROR, "Failed to activate CORBA object '%s'", name_m.c_str()));
	}
      else
	this->_remove_ref();
      
      BACIRecoveryManager::getInstance()->addRecoverableObject(this);
    }
  catch(...) 
    {
      // error.
    }
  }

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategy<T, TPROP, TALARM>::suspend ()
{
  if (suspended_m==false)
    {
      suspended_m=true;
      eventDispatcher_mp->suspend();
    }
}

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategy<T, TPROP, TALARM>::resume ()
{
  if (suspended_m==true)
    {
      suspended_m=false;
      eventDispatcher_mp->resume();
    }
}

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategy<T, TPROP, TALARM>::destroy ()
{
  ACS_TRACE("baci::AlarmEventStrategy&lt;&gt;::destroy");
  
  BACIRecoveryManager::getInstance()->removeRecoverableObject(this);
  
  if (CORBA::is_nil(reference_mp)==false) 
    {
      if (BACI_CORBA::DestroyCORBAObject(reference_mp)==false)
	{
	  ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmEventStrategy&lt;&gt;::destroy",
		  (LM_ERROR, "Failed to destroy CORBA object"));
	}
    }
}

/*********************************** IMPLEMENTATION AlarmEventStrategyDisc **********************/

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategyDisc<T, TPROP, TALARM>::check(BACIValue &val,
					     const ACSErr::Completion & c,
					     const ACS::CBDescOut & desc
					     )
{
  
  ACE_UNUSED_ARG(c);
  T value = val.getValue(static_cast<T*>(0));

// copied from Alarmpattern.cpp
 if ((this->alarmRaised_m!=0) &&    // we have an alarm (0 indicates no alarm)
      (value<=1))              // "On" or "Off"
    {
      
      try
	{

	  Completion c = ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
	  
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
  else if ((this->alarmRaised_m==0) &&                   // no alarm for now
	   (value>1))                              // alarm state
    {

      try
        {

	  Completion c;
	  c.timeStamp = getTimeStamp();
	  c.type = ACSErr::ACSErrTypeAlarm;     // alarm
	  c.code = ACSErrTypeAlarm::ACSErrAlarmHigh;     // high

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

/*********************************** IMPLEMENTATION AlarmEventStrategyCont **********************/

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategyCont<T, TPROP, TALARM>::check(BACIValue &val,
					     const ACSErr::Completion & c,
					     const ACS::CBDescOut & desc
					     )
{
  
  ACE_UNUSED_ARG(c);
  T value = val.getValue(static_cast<T*>(0)); //val.patternValue();
/*
  ACS_DEBUG_PARAM("AlarmEventStrategy&lt;&gt;::check", "Checking for alarms, value: %f.", value);
  ACS_DEBUG_PARAM("AlarmEventStrategy&lt;&gt;::check", "Low Off: %f.", property_mp->alarm_low_off());
  ACS_DEBUG_PARAM("AlarmEventStrategy&lt;&gt;::check", "High Off: %f.", property_mp->alarm_high_off());
*/
  if ((this->alarmRaised_m!=0) &&				// we have an alarm (0 indicates no alarm)
      (value>=this->property_mp->alarm_low_off()) && 
      (value<=this->property_mp->alarm_high_off()))
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
/*********************************** IMPLEMENTATION AlarmEventStrategyContSeq **********************/

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategyContSeq<T, TPROP, TALARM>::check(BACIValue &val,
					     const ACSErr::Completion & c,
					     const ACS::CBDescOut & desc)
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


// ACS_DEBUG_PARAM("AlarmdoubleSeqEventStrategy::check", "Low Off: %f.", this->property_mp->alarm_low_off());
//  ACS_DEBUG_PARAM("AlarmdoubleSeqEventStrategy::check", "High Off: %f.", this->property_mp->alarm_high_off());


  for (CORBA::ULong n = 0UL; n < valueSeq.length(); n++)
    {
//	  ACS_DEBUG_PARAM("AlarmdoubleSeqEventStrategy::check", "Checking for alarms, value: %f.", valueSeq[n]);

      if ((alarmsRaised_mp[n]!=0) &&							// we have an alarm (0 indicates no alarm)
	  (valueSeq[n]>=this->property_mp->alarm_low_off()) && 
	  (valueSeq[n]<=this->property_mp->alarm_high_off()))
	{
      
	  try
	    {
	      
	    Completion c = ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
	      
	      this->callback_mp->alarm_cleared(valueSeq[n], c, desc);
	      
	      this->succeeded();
	      alarmsRaised_mp[n] = 0;
	    }
	  catch(...)
	    {
	      if (this->failed()==true) 
		  this->destroy();
	    }
	  	}
      else if ((alarmsRaised_mp[n]!=-1) &&            // if not alarm low
	       (valueSeq[n]<=this->property_mp->alarm_low_on()))
	{
	  
	  try
	    {
	      
	    Completion c = ACSErrTypeAlarm::ACSErrAlarmLowCompletion();
	      
	      this->callback_mp->alarm_raised(valueSeq[n], c, desc);
	      
	      this->succeeded();
	      alarmsRaised_mp[n] = -1;
	    }
	  catch(...)
	    {
	      if (this->failed()==true)
		  this->destroy();
	    }
	  	}
      else if ((alarmsRaised_mp[n]!=1) &&            // if not alarm hi 
	   (valueSeq[n]>=this->property_mp->alarm_high_on()))
	{
	  
	  try
	    {
	      
	    Completion c=ACSErrTypeAlarm::ACSErrAlarmHighCompletion();
	      
	    this->callback_mp->alarm_raised(valueSeq[n], c, desc);
	      
	    this->succeeded();
	    alarmsRaised_mp[n] = 1;
	    }
	  catch(...)
	    {
	      if (this->failed())
		  this->destroy();
	    }
	  	  }
    }  // for loop
}

/*********************************** IMPLEMENTATION AlarmEventStrategyDiscSeq **********************/
// I do not know if this code works or not. It is here that DiscreteSeq can build. Anywat alarm system will be replaced soon.

template<class T, class TPROP, class TALARM>
void baci::AlarmEventStrategyDiscSeq<T, TPROP, TALARM>::check(BACIValue &val,
					     const ACSErr::Completion & c,
					     const ACS::CBDescOut & desc)
{
    ACE_UNUSED_ARG(c);
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
      for (int i = 0; i < alarmsRaisedLength_m; i++)
	  alarmsRaised_mp[i] = 0;
    }

  for (CORBA::ULong n = 0UL; n < valueSeq.length(); n++)
    {
      if ((alarmsRaised_mp[n]!=0) 
//&&							// we have an alarm (0 indicates no alarm)
//	  (valueSeq[n]>=property_mp->alarm_low_off()) && 
//	  (valueSeq[n]<=property_mp->alarm_high_off())
)
	{
      
	  try
	    {
	      
	    Completion c=ACSErrTypeAlarm::ACSErrAlarmClearedCompletion();
	      
	      this->callback_mp->alarm_cleared(valueSeq[n], c, desc);
	      
	      this->succeeded();
	      alarmsRaised_mp[n] = 0;
	    }
	  catch(...)
	    {
	      if (this->failed()==true) 
		  this->destroy();
	    }
	  	}
      else if ((alarmsRaised_mp[n]!=-1) 
//&&            // if not alarm low
//	       (valueSeq[n]<=property_mp->alarm_low_on())
)
	{
	  
	  try
	    {
	      
	    Completion c = ACSErrTypeAlarm::ACSErrAlarmLowCompletion();
	      
	      this->callback_mp->alarm_raised(valueSeq[n], c, desc);
	      
	      this->succeeded();
	      alarmsRaised_mp[n] = -1;
	    }
	  catch(...)
	    {
	      if (this->failed()==true)
		  this->destroy();
	    }
	  	}
      else if ((alarmsRaised_mp[n]!=1) 
//&&            // if not alarm hi 
//	   (valueSeq[n]>=property_mp->alarm_high_on())
)
	{
	  
	  try
	    {
	    Completion c=ACSErrTypeAlarm::ACSErrAlarmHighCompletion();
	    
	    this->callback_mp->alarm_raised(valueSeq[n], c, desc);
	      
	    this->succeeded();
	    alarmsRaised_mp[n] = 1;
	    }
	  catch(...)
	    {
	      if (this->failed()==true)
		  this->destroy();
	    }
	  	  }
    }  // for loop
}



















