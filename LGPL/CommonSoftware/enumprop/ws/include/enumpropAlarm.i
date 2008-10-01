
template <class T, class ROT, class AlarmT>
const int AlarmenumpropEventStrategy<T, ROT, AlarmT>::maxFailureCount = 3;

#ifdef MAKE_VXWORKS 
template <class T, class ROT, class AlarmT>
AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy() :
    callback_mp(0), eventDispatcher_mp(0)
{}
#endif

template <class T, class ROT, class AlarmT>
AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy(ROT * property,
				       EventDispatcher * eventDispatcher) :
  failureCount_m(0), property_mp(property), eventDispatcher_mp(eventDispatcher), alarmRaised_m(false)
  
{
  // call recovery
}

template <class T, class ROT, class AlarmT> 
AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy(Callback_ptr callback,
				       const CBDescIn& descIn,
				       const ACS::TimeInterval& interval,
				       ROT * property,
				       EventDispatcher * eventDispatcher) :
  suspended_m(false), failureCount_m(0), m_descIn(descIn), interval_m(interval),
  property_mp(property), eventDispatcher_mp(eventDispatcher), alarmRaised_m(false)
{

    //ACS_TRACE("baci::AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy");

  name_m = ACE_CString(property->getProperty()->getName())+"_event";
  char* cname = BACIRecoveryManager::getInstance()->generateObjectName(name_m.c_str());
  name_m = cname;
  delete[] cname;
  
//  ACS_DEBUG_PARAM("AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy", "Name is: '%s'",  name_m.c_str());
  
  callback_mp = ACS::Alarmpattern::_narrow(callback);
  if (callback_mp  == ACS::Alarmpattern::_nil())
      {
      ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy", (LM_ERROR, "Failed to narrow callback"));
	// report error
	return;
    }

  reference_mp = BACI_CORBA::ActivateCORBAObject((PortableServer::Servant)this, name_m.c_str());
  if (CORBA::is_nil(reference_mp))
  {
    ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmenumpropEventStrategy<T, ROT, AlarmT>::AlarmenumpropEventStrategy",
								(LM_ERROR, "Failed to activate CORBA object '%s'", name_m.c_str()));
  }
  else
      this->_remove_ref();
      

  // subscribe to event dispatcher
  eventDispatcher_mp->subscribe(this);

  BACIRecoveryManager::getInstance()->addRecoverableObject(this);
}

/// monitor have to be destroyed using destroy() method
/// POA will call desctructor
template <class T, class ROT, class AlarmT>
AlarmenumpropEventStrategy<T, ROT, AlarmT>::~AlarmenumpropEventStrategy()
{
  ACS_TRACE("baci::AlarmenumpropEventStrategy<T, ROT, AlarmT>::~AlarmenumpropEventStrategy");
  // unsubscribe to event dispatcher
  if (callback_mp)
      CORBA::release(callback_mp);
  if (eventDispatcher_mp)
      eventDispatcher_mp->unsubscribe(this);
}

template <class T, class ROT, class AlarmT>
bool AlarmenumpropEventStrategy<T, ROT, AlarmT>::failed()
{
  return (++failureCount_m>=maxFailureCount);
}

template <class T, class ROT, class AlarmT>
void AlarmenumpropEventStrategy<T, ROT, AlarmT>::succeeded()
{
  failureCount_m = 0;
}

template <class T, class ROT, class AlarmT>
void AlarmenumpropEventStrategy<T, ROT, AlarmT>::check(BACIValue &val,
			  const ACSErr::Completion & c,
			  const ACS::CBDescOut & desc
			  )
{
  ACE_UNUSED_ARG(c);

  ACS::pattern value = val.patternValue();
 
  try
      {
      if ( property_mp->checkAlarm(T(value)) && !alarmRaised_m )
	  {	  
//TBD:: here should be alarm raised, but it has to be added to codes
	  ACSErrTypeAlarm::ACSErrAlarmHighCompletion comp;
	  callback_mp->alarm_raised(T(value), comp, desc);
	  alarmRaised_m = true;
	  }
      if (!property_mp->checkAlarm(T(value)) && alarmRaised_m )
	  {
	  ACSErrTypeAlarm::ACSErrAlarmClearedCompletion comp;
	  callback_mp->alarm_cleared(T(value), comp, desc);
	  alarmRaised_m = false;
	  }
      succeeded();
      }
  catch(...)
      {
      if (failed()) 
	  destroy();
	}
}//check
  
/* ------------------- [ Recoverable interface ] --------------------*/

template <class T, class ROT, class AlarmT>
int AlarmenumpropEventStrategy<T, ROT, AlarmT>::getId(void)
{
  return -1;
}

template <class T, class ROT, class AlarmT>
const char* AlarmenumpropEventStrategy<T, ROT, AlarmT>::getName(void)
{
  return name_m.c_str();
}
 
template <class T, class ROT, class AlarmT>
char* AlarmenumpropEventStrategy<T, ROT, AlarmT>::getObjectState(void)
{
  ACE_TCHAR *buffer;
  ACE_NEW_RETURN (buffer, ACE_TCHAR[MAX_RECORD_SIZE], 0);
  
  
  ACE_TRY 
    {

      CORBA::String_var ior = BACI_CORBA::getORB()->object_to_string(callback_mp
								     );
      
      
      unsigned long tag = m_descIn.id_tag;
      
      // !!! VxWorks support !!! Lx is not ANSI C and since VxWorks does not support
      // (U)LongLong, flag L will not work
      
      // name, ior, tag, time trigger, isSuspended
      ACE_OS::sprintf(buffer, "%s %s %lu %llu %u",
		      getName(), ior.in(), tag, interval_m,
		      suspended_m);

      return buffer;
      
    }
  ACE_CATCHANY 
    { 
      // log debug
    }
  ACE_ENDTRY;
  
  return 0;
}
 
template <class T, class ROT, class AlarmT>
void AlarmenumpropEventStrategy<T, ROT, AlarmT>::setObjectState(const char * state)
{
  ACE_TCHAR cname[MAX_NAME_LENGTH];
  ACE_TCHAR ior[MAX_IOR_LENGTH];
  
  unsigned long tag;
  unsigned int isSuspended;

  // name, ior, tag, time trigger, isSuspended
  sscanf(state, "%s %s %lu %llu %u", 
	 cname, ior, &tag, &interval_m, &isSuspended);
  /*
  ACE_OS::printf("R) Name: %s\n", cname);
  ACE_OS::printf("R) IOR: %s\n", ior);
  ACE_OS::printf("R) id_tag: %lu\n", tag);
  ACE_OS::printf("R) interval: %llu\n", interval_m);
  ACE_OS::printf("R) isSuspended: %u\n", isSuspended);
  */

  m_descIn.id_tag = tag;
  suspended_m = isSuspended;

  name_m = cname;

  
  ACE_TRY 
    {
       
      CORBA::Object_var obj = BACI_CORBA::getORB()->string_to_object(ior);
      
      
      if (CORBA::is_nil(obj.in())) 
	{
	  // log debug.
	  return;
	}

      callback_mp = ACS::Alarmpattern::_narrow(obj.in());
      

      if (callback_mp == ACS::Alarmpattern::_nil())
	{
	  // report error
	  return;
	}
      
      // generate appropriate name
      ACS_DEBUG_PARAM("AlarmenumpropEventStrategy<T, ROT, AlarmT>::setObjectState", "Recovering: '%s'", name_m.c_str());

      reference_mp = BACI_CORBA::ActivateCORBAObject(this, name_m.c_str());
      if (CORBA::is_nil(reference_mp))
	  {
		ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmenumpropEventStrategy<T, ROT, AlarmT>::setObjectState",
				(LM_ERROR, "Failed to activate CORBA object '%s'", name_m.c_str()));
	  }
      else
	this->_remove_ref();
      
      BACIRecoveryManager::getInstance()->addRecoverableObject(this);
    }
  ACE_CATCHANY 
    {
      // error.
    }
  ACE_ENDTRY;
}

/* --------------- [ Subscription interface ] --------------- */ 

template <class T, class ROT, class AlarmT>
void AlarmenumpropEventStrategy<T, ROT, AlarmT>::suspend ()
{
  if (!suspended_m)
  {
    suspended_m=true;
    eventDispatcher_mp->suspend();
  }
}
 
template <class T, class ROT, class AlarmT>
void AlarmenumpropEventStrategy<T, ROT, AlarmT>::resume ()
{
  if (suspended_m)
  {
    suspended_m=false;
    eventDispatcher_mp->resume();
  }
}

template <class T, class ROT, class AlarmT> 
void AlarmenumpropEventStrategy<T, ROT, AlarmT>::destroy ()
{
  ACS_TRACE("baci::AlarmenumpropEventStrategy<T, ROT, AlarmT>::destroy");

  BACIRecoveryManager::getInstance()->removeRecoverableObject(this);
  
  if (!CORBA::is_nil(reference_mp)) 
    {
      if (!BACI_CORBA::DestroyCORBAObject(reference_mp))
		ACS_LOG(LM_RUNTIME_CONTEXT, "AlarmenumpropEventStrategy::destroy",
			(LM_ERROR, "Failed to destroy CORBA object"));
    }
}
















