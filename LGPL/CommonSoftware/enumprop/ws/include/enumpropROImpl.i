/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: enumpropROImpl.i,v 1.59 2011/03/25 10:33:40 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-05-30 removed setting intial value fro DevIO
* bjeram 2003-03-14 changed defaultValue_m from ACS::pattern type to type T
* bjeram 2003-03-14 added BACIvalue(0.0) as a parameter to constructor of baci::Monitorpattern (recovery)
* bjeram 2003-03-14 changed DevIO to its template versio DevIOT
* bjeram 2002-11-18 changed to onchange monitor
* bjeram 2002-07-08 added implementation of allSTates
* bjeram 2001-10-25 created 
*/


/*
TODO
CBpattern -> CBT
baci::Monitorpattern -> Monitor(T)
Alarmpattern
*/

template <ACS_ENUM_C>
ROEnumImpl<ACS_ENUM_T(T), SK>::ROEnumImpl(const ACE_CString& name, baci::BACIComponent* cob, DevIO<T> *devIO, bool flagdeldevIO) : 
    CharacteristicModelImpl(name, cob->getCharacteristicModel()), 
    initialization_m(1), destroyed_m(false), reference_mp(CORBA::Object::_nil()), property_mp(0),
    monitorEventDispatcher_mp(0), alarmSystemMonitorEnumProp_mp(0), historyStart_m(-1), historyTurnaround_m(false), m_enumLength(0)
    {

	ACS_TRACE("ROEnumImpl::ROEnumImpl");

	// initialize
	m_condition.length(0);
	m_statesDescription.length(0);

	// create BACI property instance
	CORBA::Any tAny;
	tAny <<= default_value();
	property_mp = new baci::BACIProperty(name.c_str(), this, this,
			baci::BACIValue(default_value(), tAny), cob);
	if (!property_mp) {
		std::string procName="ROEnumImpl::ROEnumImpl(";
		procName+=name.c_str();
		procName+=",...)";
		baciErrTypeProperty::PropertyCreationExImpl ex(__FILE__,__LINE__,procName.c_str());
		ex.addData("Property",name.c_str());
		throw ex;
	}

	// read static data
	if (!readCharacteristics())
	{
		std::string procName="ROEnumImpl::ROEnumImpl(";
		procName+=name.c_str();
		procName+=",...)";

		ACS_LOG(LM_RUNTIME_CONTEXT, "ROEnumImpl::ROEnumImpl",
				(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));

		baciErrTypeProperty::PropertyStaticDataExImpl ex(__FILE__,__LINE__,procName.c_str());
		ex.addData("Property",name.c_str());
		throw ex;
	}

	reference_mp = BACI_CORBA::ActivateCORBAObject(this, name.c_str());
	if (CORBA::is_nil(reference_mp))
	{
		std::string procName="ROEnumImpl::ROEnumImpl(";
		procName+=name.c_str();
		procName+=",...)";
		ACS_LOG(LM_RUNTIME_CONTEXT, "ROEnumImpl::ROEnumImpl",
				(LM_ERROR, "Failed to activate CORBA object '%s'", name.c_str()));

		delete property_mp;
		baciErrTypeProperty::PropertyActivationExImpl ex(__FILE__,__LINE__,procName.c_str());
		ex.addData("Property",name.c_str());
		throw ex;
	}

	ACE_CString_Vector recoveryMonitorsNames =
			baci::BACIRecoveryManager::getInstance()->getObjectsStartingWith(name.c_str());
	if (recoveryMonitorsNames.size()>0)
	{
		for (ACE_CString_Vector::iterator i = recoveryMonitorsNames.begin();
				i != recoveryMonitorsNames.end(); i++) {
			new baci::Monitorpattern(i->c_str(), min_timer_trigger(), baci::BACIValue(0.0), property_mp);
		}
	}
	state = (T)defaultValue_m;
	alarmRaised_m = checkAlarm(state);

	if (devIO!=0)
	{
		devIO_mp = devIO;
		deldevIO_m = flagdeldevIO;
		devIO_mp->m_initialize = initializeDevIO_m;
	}
	else
	{
		deldevIO_m = true;
		m_value = (T)defaultValue_m;
		devIO_mp = new DevIOMem<T> (m_value);
		ACS_DEBUG("ROEnumImpl::ROEnumImpl", "No DevIO provided - created DevIOMem.");
	}//if-else

	if (monitorEventDispatcher_mp==0 && this-> m_alarm_timer_trig!=0)
	{
		CBDescIn descIn;
		descIn.id_tag = 0;
		monitorEventDispatcher_mp = new baci::MonitorenumpropEventDispatcher(descIn, m_alarm_timer_trig, property_mp);

		if (this->monitorEventDispatcher_mp!=0 && this->m_alarm_timer_trig!=0)
		{
			alarmSystemMonitorEnumProp_mp = new baci::AlarmSystemMonitorEnumProp<T, ROEnumImpl<ACS_ENUM_T(T), SK> >(this, this->monitorEventDispatcher_mp);
		}//if
	}  


	ACS_DEBUG_PARAM("ROEnumImpl::ROEnumImpl", "Successfully created %s.", name.c_str() );

	// property successfuly initialized
	initialization_m = 0;
    }

template <ACS_ENUM_C>
ROEnumImpl<ACS_ENUM_T(T), SK>::~ROEnumImpl()
{
  ACS_TRACE("ROEnumImpl::~ROEnumImpl");

  if (deldevIO_m)
      delete devIO_mp;
  if (alarmSystemMonitorEnumProp_mp)
      {
      delete alarmSystemMonitorEnumProp_mp;
      alarmSystemMonitorEnumProp_mp = 0;
      }//if

  // destroy event dispatcher (including event subscribers)
  if (monitorEventDispatcher_mp) 
  {
    delete monitorEventDispatcher_mp;
    monitorEventDispatcher_mp = 0;
  }

  // destroy BACI property
  if (property_mp) {
    delete property_mp;
    property_mp = 0;
  }
}

template <ACS_ENUM_C>
void ROEnumImpl<ACS_ENUM_T(T), SK>::destroy()
{
  ACS_TRACE("ROEnumImpl::destroy");
  if (destroyed_m)
    return;
  destroyed_m = true;

  if (!CORBA::is_nil(reference_mp))
    {
	  // this calls delete on this object, so DO NOT use any of its variables anymore
	  if (!BACI_CORBA::DestroyCORBAObject(reference_mp))
		{
		ACS_LOG(LM_RUNTIME_CONTEXT, "ROEnumImpl::~ROEnumImpl",
			(LM_ERROR, "Failed to destroy CORBA object '%s'", property_mp->getName()));
		}
	    else
		this->_remove_ref();

    }
  
}

/* --------------- [ Action implementator interface ] -------------- */

template <ACS_ENUM_C>
baci::ActionRequest ROEnumImpl<ACS_ENUM_T(T), SK>::invokeAction(int function,
		       baci::BACIComponent* cob, const int& callbackID, 
		       const CBDescIn& descIn, baci::BACIValue* value, 
		       Completion& completion, CBDescOut& descOut)
{
  ACE_UNUSED_ARG(function);
  CompletionImpl c;
  // only one action
  baci::ActionRequest req = getValueAction(cob, callbackID, descIn, value, c, descOut);

  if (c.isErrorFree())
      {
      completion = c;
      }
  else
      {
      completion = baciErrTypeProperty::InvokeActionErrorCompletion(c,
					       __FILE__,
					       __LINE__,
					       "ROEnumImpl&lt;&gt;::getValue");
      }//if-else

  return req;
}

/* -------------- [ Property implementator interface ] -------------- */

template <ACS_ENUM_C>
void ROEnumImpl<ACS_ENUM_T(T), SK>::getValue(baci::BACIProperty* property,
		   baci::BACIValue* value, 
		   Completion &completion,
		   CBDescOut& descOut)
{
  ACE_UNUSED_ARG(property);
  ACE_UNUSED_ARG(descOut);

  ACS::pattern nval;
  T realVal;
  
  ACS::Time ts;
  try 
      {
      realVal = devIO_mp->read(ts); 
      nval = (ACS::pattern)realVal;
      } 
  catch (ACSErr::ACSbaseExImpl& ex) 
      {
	completion = baciErrTypeDevIO::ReadErrorCompletion (ex, __FILE__, __LINE__,"ROEnumImpl<>::getValue");
	return;
      }
  
  CORBA::Any tAny;
  tAny <<= realVal;

  value->enumValue(nval, tAny);

  // if there is no error add value to history
  // !!! to be done in a loop
  addValueToHistory(completion.timeStamp, nval);

  completion = ACSErrTypeOK::ACSErrOKCompletion();
  completion.timeStamp = ts;
}//getValue
 
/* --------------- [ History support ] --------------- */

template <ACS_ENUM_C>
void ROEnumImpl<ACS_ENUM_T(T), SK>::addValueToHistory(ACS::Time time, ACS::pattern value)
{
  if (!historyTurnaround_m && (historyStart_m==(HISTORY_SIZE-1))) 
	  historyTurnaround_m = true;
  historyStart_m = ++historyStart_m % HISTORY_SIZE;
  historyTime_m[historyStart_m] = time;
  historyValue_m[historyStart_m] = (T)value;
}

/* -------------- [ Other interfaces ] -------------- */

/// async. get value action implementation
template <ACS_ENUM_C>
baci::ActionRequest ROEnumImpl<ACS_ENUM_T(T), SK>::getValueAction(baci::BACIComponent* cob, const int& callbackID,
				       const CBDescIn& descIn, baci::BACIValue* value,
				       Completion& completion, CBDescOut& descOut)
{
  ACE_UNUSED_ARG(cob);
  ACE_UNUSED_ARG(callbackID);
  ACE_UNUSED_ARG(descIn);
  CompletionImpl co;

  getValue(property_mp, value, co, descOut);
  
  if (co.isErrorFree())
      {
      completion = co;
      }
  else
      {
      completion = baciErrTypeProperty::CanNotGetValueCompletion(co, 
					    __FILE__, 
					    __LINE__, 
					    "ROEnumImpl<>::getValue");
      }//if-else

  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return baci::reqInvokeDone;
}


/* ---------------------- [ CORBA interface ] ---------------------- */

template <ACS_ENUM_C>
bool ROEnumImpl<ACS_ENUM_T(T), SK>::readCharacteristics()
{

  cdb::DAONode* dao = this->getDAONode();
  if (!dao)
      return false;
  
  try
      {
      CORBA::String_var str;
      
      str = dao->get_string("description");
      m_description = str.in();
      
      str = dao->get_string("format");
      format_m = str.in();
      
      str = dao->get_string("units");
      units_m = str.in();

      m_resolution = dao->get_long("resolution");

      CORBA::Double dbl;

      dbl = dao->get_double("default_timer_trig");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      defaultTimerTrig_m = static_cast<CORBA::ULong>(dbl);

      dbl = dao->get_double("min_timer_trig");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      minTimerTrig_m = static_cast<CORBA::ULong>(dbl);

      defaultValue_m = static_cast<T>(dao->get_long("default_value"));


      CDB::stringSeq_var descs = dao->get_string_seq("statesDescription");
      CORBA::ULong len = descs->length();
      m_statesDescription.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          m_statesDescription[i] = CORBA::string_dup(descs[i].in());

      m_enumLength = len;   //! entry in the CDB should be the right length otherwise ....

      CDB::longSeq_var lc = dao->get_long_seq("condition");
      len = lc->length();
      m_condition.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          m_condition[i] = static_cast<ACS::Condition>(lc[i]);

      CDB::longSeq_var aon = dao->get_long_seq("alarm_on");
      len = aon->length();
      m_alarm_on.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          m_alarm_on[i] = static_cast<T>(aon[i]);

      CDB::longSeq_var aoff = dao->get_long_seq("alarm_off");
      len = aoff->length();
      m_alarm_off.length(len);
      for (CORBA::ULong i = 0; i < len; i++)
          m_alarm_off[i] = static_cast<T>(aoff[i]);

      dbl = dao->get_double("alarm_timer_trig");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      m_alarm_timer_trig = static_cast<CORBA::ULong>(dbl);

#if 0
      dbl = dao->get_double("archive_min_int");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      m_archive_min_int = static_cast<CORBA::ULong>(dbl);

      dbl = dao->get_double("archive_max_int");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      m_archive_max_int = static_cast<CORBA::ULong>(dbl);

      m_archive_delta = dao->get_long("archive_delta");
#endif

      // alarm_fault_{family,member} are optional, so don't fail if they're not present
      try {
         str = dao->get_string("alarm_fault_family");
      } catch(ACSErr::ACSbaseExImpl& ex) {}

      if (strlen(str)!=0) //if FF is not specified in the CDB
    	  alarmFaultFamily_m = str.in();
      else {
        ACE_CString compType = this->property_mp->getComponent()->getType();
        if( compType.length() == 0 ) {
    	    alarmFaultFamily_m = "BACIProperty"; //default
        }
        else {

          size_t pos;

          // strip out "IDL:" and ":1.0" from the type string, if necessary
          if( compType.find("IDL:") == 0 )
            alarmFaultFamily_m = compType.substr(4, -1);
          if( (pos = alarmFaultFamily_m.find(":1.0")) == (alarmFaultFamily_m.length() - 4) )
            alarmFaultFamily_m = alarmFaultFamily_m.substr(0, pos);

          // keep just the interface name, throw away the pragma and module names
          while( (pos = alarmFaultFamily_m.find('/')) != ACE_CString::npos )
            alarmFaultFamily_m = alarmFaultFamily_m.substr(pos + 1);

          ACE_CString fm(this->property_mp->getName());     // property name is "CompName:PropName
          alarmFaultFamily_m += '#';
          alarmFaultFamily_m += fm.substr(fm.find(':') + 1);
        }
      }

      try {
         str = dao->get_string("alarm_fault_member");
      } catch(ACSErr::ACSbaseExImpl& ex) {}

      if (strlen(str)!=0) //if FF is not specified in the CDB
    	  alarmFaultMember_m = str.in();
      else {
        ACE_CString fm(this->property_mp->getName());     // property name is "CompName:PropName"
    	  alarmFaultMember_m = fm.substr(0, fm.find(':')); //
      }

      alarmLevel_m = 0;
      try {
         alarmLevel_m = dao->get_long("alarm_level");
      } catch(ACSErr::ACSbaseExImpl& ex) {}

      return true;
      }
  catch (ACSErr::ACSbaseExImpl& ex)
      {
      ex.log();
      return false;
      }
  catch (...)
      {
      return false;
      }

}

/* ---------------------- [ CORBA interface ] ---------------------- */
template <ACS_ENUM_C>
char *ROEnumImpl<ACS_ENUM_T(T), SK>::name ()
{
  return CORBA::string_dup(property_mp->getName());           
}

template <ACS_ENUM_C>
char *ROEnumImpl<ACS_ENUM_T(T), SK>::characteristic_component_name ()
{
  return CORBA::string_dup (property_mp->getComponent()->getName());
}

template <ACS_ENUM_C>
char *ROEnumImpl<ACS_ENUM_T(T), SK>::description ()
{
  return CORBA::string_dup (m_description.c_str());
}

template <ACS_ENUM_C>
char *ROEnumImpl<ACS_ENUM_T(T), SK>::format ()
{
  return CORBA::string_dup (format_m.c_str());
}

template <ACS_ENUM_C>
char *ROEnumImpl<ACS_ENUM_T(T), SK>::units ()
{
  return CORBA::string_dup (units_m.c_str());
}

template <ACS_ENUM_C>
CORBA::Boolean ROEnumImpl<ACS_ENUM_T(T), SK>::initialize_devio ()
{
  return CORBA::Boolean(initializeDevIO_m);
}

template <ACS_ENUM_C>
ACS::pattern ROEnumImpl<ACS_ENUM_T(T), SK>::resolution ()
{
  return m_resolution;
}

template <ACS_ENUM_C>
T ROEnumImpl<ACS_ENUM_T(T), SK>::get_sync (ACSErr::Completion_out c
		    )
{
    CompletionImpl co;

    ACS::CBDescOut descOut;
    baci::BACIValue value(0.0);

    this->getValue(property_mp, &value, co, descOut);

    if (co.isErrorFree())
	{
	c = co.returnCompletion(false);
	}
    else
	{
	c = baciErrTypeProperty::CanNotGetValueCompletion(co, 
				     __FILE__, 
				     __LINE__, 
				     "ROEnumImpl&lt;&gt;::get_sync").returnCompletion(false);
     }//if-else

    return (T)(value.patternValue());
}

template <ACS_ENUM_C>
void ROEnumImpl<ACS_ENUM_T(T), SK>::get_async (CBpattern* cb,
		     const ACS::CBDescIn & desc
		     )
{
  property_mp->getComponent()->registerAction(baci::BACIValue::type_pattern, cb, 
				       desc, this, 0);
}

template <ACS_ENUM_C>
CORBA::Long ROEnumImpl<ACS_ENUM_T(T), SK>::get_history (CORBA::Long n_last_values,
		       TSeq_out vs,
		       ACS::TimeSeq_out ts
		       )
{

  // thread lock needed
    
  if (n_last_values < 0)
	  ACE_THROW_RETURN(CORBA::BAD_PARAM(), 0);
 
  CORBA::Long length, first;
  if (historyTurnaround_m)
    {
      length = HISTORY_SIZE;
      first = historyStart_m+1;
    }
  else
    {
      length = historyStart_m+1;
      first = 0;
    }

  if (n_last_values < length)
	  length = n_last_values;

  vs = new TSeq(length); vs->length(length);
  ts = new ACS::TimeSeq(length); ts->length(length);
  for (int i = 0; i < length; i++)
    {
      (*vs)[i] = historyValue_m[(first+i)%HISTORY_SIZE];
      (*ts)[i] = historyTime_m[(first+i)%HISTORY_SIZE];
    } 
  return length;
}

template <ACS_ENUM_C>
ACS::Monitorpattern_ptr ROEnumImpl<ACS_ENUM_T(T), SK>::create_monitor (CBpattern* cb,
			  const ACS::CBDescIn & desc
			  )
{

    baci::Monitorpattern* monitor = new baci::Monitorpattern(ACE_CString(property_mp->getName())+"_monitor",
					     cb, desc, 
					     default_timer_trigger(),
					      baci::BACIValue::NullValue,  
					     min_timer_trigger(),
					   baci::BACIValue::NullValue,  
					     property_mp);

  if (!monitor)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Monitorpattern::_nil());
  else if (monitor->initialization())
  {
	  monitor->destroy();
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Monitorpattern::_nil());
  }
  
  ACS::Monitorpattern_var mon = 
	  ACS::Monitorpattern::_narrow(monitor->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Monitorpattern::_nil());

  return mon._retn();
}

template <ACS_ENUM_C>
ACS::Monitor_ptr ROEnumImpl<ACS_ENUM_T(T), SK>::create_postponed_monitor (ACS::Time start_time,
			  CBpattern* cb,
			  const ACS::CBDescIn & desc
			  )
{

  baci::Monitorpattern* monitor = new baci::Monitorpattern(ACE_CString(property_mp->getName())+"_monitor",
					     cb, desc, 
					     default_timer_trigger(),
					       baci::BACIValue::NullValue, 
					     min_timer_trigger(), 
					       baci::BACIValue::NullValue, 
					     property_mp,
					     start_time);
  
  if (!monitor)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Monitor::_nil());
  else if (monitor->initialization())
  {
	  monitor->destroy();
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Monitor::_nil());
  }
  
  ACS::Monitor_var mon = 
	  ACS::Monitor::_narrow(monitor->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Monitor::_nil());

  return mon._retn();
}

template <ACS_ENUM_C>
ACS::TimeInterval ROEnumImpl<ACS_ENUM_T(T), SK>::default_timer_trigger ()
{
  return defaultTimerTrig_m;
}

template <ACS_ENUM_C>
ACS::TimeInterval ROEnumImpl<ACS_ENUM_T(T), SK>::min_timer_trigger ()
{
  return minTimerTrig_m;
}

template <ACS_ENUM_C>
T ROEnumImpl<ACS_ENUM_T(T), SK>::default_value ()
{
  return (T)defaultValue_m;
}

/*
ACS::Subscription_ptr ROEnumImpl<ACS_ENUM_T(T), SK>::new_subscription_Alarmpattern (ACS::Alarmpattern_ptr cb,
					const ACS::CBDescIn & desc,
					CORBA::Environment &)
{

  if (monitorEventDispatcher_mp==0)
  {
    CBDescIn descIn;
    descIn.id_tag = 0;
    monitorEventDispatcher_mp = new baci::MonitorpatternEventDispatcher(descIn, m_alarm_timer_trig, property_mp);
	if (!monitorEventDispatcher_mp)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
  }  

  AlarmpatternEventStrategy * eventStrategy = 
    new AlarmpatternEventStrategy(cb, desc, m_alarm_timer_trig, 
				 this, monitorEventDispatcher_mp);
  if (!eventStrategy)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());

  ACS::Subscription_var subscription = 
    ACS::Subscription::_narrow(eventStrategy->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Subscription::_nil());

  return subscription._retn();
}
*/

template <ACS_ENUM_C>
ACS::stringSeq *ROEnumImpl<ACS_ENUM_T(T), SK>::statesDescription ()
{

  // create and initialize the return parameter
  ACS::stringSeq_var result = new ACS::stringSeq(m_statesDescription);
  // to return, take the sequence away from the _var
  return result._retn();
}

template <ACS_ENUM_C>
ACS::ConditionSeq *ROEnumImpl<ACS_ENUM_T(T), SK>::condition ()
{

  // create and initialize the return parameter
  ACS::ConditionSeq_var result = new ACS::ConditionSeq(m_condition);
  // to return, take the sequence away from the _var
  return result._retn();
}

template <ACS_ENUM_C>
TSeq * ROEnumImpl<ACS_ENUM_T(T), SK>::allStates ( )
{
    TSeq* result = new TSeq();
    result->length(m_enumLength);
 // m_enumLength is read out of the length of the seq of states deescription which should be OK otherwise this method will not work correctly.

    for (int i=0; i<m_enumLength; i++)
	(*result)[i] = (T)i;


    return result;   
}
 /* -------------------- [ RO interface ] -------------------- */
template <ACS_ENUM_C>  
TSeq* ROEnumImpl<ACS_ENUM_T(T), SK>::alarm_on ( )
{

  // create and initialize the return parameter
  TSeq *result = new TSeq(m_alarm_on);
  // to return, take the sequence away from the _var
  return result;
}

template <ACS_ENUM_C>
TSeq* ROEnumImpl<ACS_ENUM_T(T), SK>::alarm_off ( )
{

  // create and initialize the return parameter
  TSeq *result = new TSeq(m_alarm_off);
  // to return, take the sequence away from the _var
  return result;
}

template <ACS_ENUM_C>
ACS::Subscription_ptr ROEnumImpl<ACS_ENUM_T(T), SK>::new_subscription_AlarmEnum (ACS::Alarmpattern_ptr cb,
					const ACS::CBDescIn & desc
					)
{
   if (this->m_alarm_timer_trig==0)
       {
       
       ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROEnumImpl&lt;&gt;::new_subscription_Alarm",
	       (LM_ERROR, "Can not create alarm dispatcher for %s because alarm_timer_trig=0", 
		this->getProperty()->getName()));
       ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
       }//if

  if (monitorEventDispatcher_mp==0)
  {
    CBDescIn descIn;
    descIn.id_tag = 0;
    monitorEventDispatcher_mp = new baci::MonitorenumpropEventDispatcher(descIn, m_alarm_timer_trig, property_mp);
	if (!monitorEventDispatcher_mp)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
  }  

  baci::AlarmenumpropEventStrategy<T, ROEnumImpl<ACS_ENUM_T(T), SK>, ACS::Alarmpattern> * eventStrategy = 
      new baci::AlarmenumpropEventStrategy<T, ROEnumImpl<ACS_ENUM_T(T), SK>, ACS::Alarmpattern>
      (cb,
       desc,
       m_alarm_timer_trig,
       this,
       monitorEventDispatcher_mp);

if (!eventStrategy)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());

  ACS::Subscription_var subscription = 
    ACS::Subscription::_narrow(eventStrategy->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Subscription::_nil());

  return subscription._retn();
}


template <ACS_ENUM_C>
bool ROEnumImpl<ACS_ENUM_T(T), SK>::checkAlarm(T state){
   for(unsigned int i=0; i< m_alarm_on.length(); i++)
	if( m_alarm_on[i]==state) return true;
   return false;
}


template <ACS_ENUM_C>
void ROEnumImpl<ACS_ENUM_T(T), SK>::setAlarmFaultFamily(const char* ff)
{
	ACS_TRACE("baci::ROcommonImpl&lt;&gt;::setAlarmFaultFamily");
	if (this->alarmSystemMonitorEnumProp_mp!=0)
	{
		this->alarmSystemMonitorEnumProp_mp->setFaultFamily(ff);
	}
	else
	{
		//@TBD error handling
	}//if-else
}//setAlarmFaultFamily

template <ACS_ENUM_C>
void ROEnumImpl<ACS_ENUM_T(T), SK>::setAlarmFaultMember(const char* fm)
{
	if (this->alarmSystemMonitorEnumProp_mp!=0)
	{
		this->alarmSystemMonitorEnumProp_mp->setFaultMember(fm);
	}
	else
	{
//@TBD error handling
	}//if-else

}//setAlarmFaultMember






















