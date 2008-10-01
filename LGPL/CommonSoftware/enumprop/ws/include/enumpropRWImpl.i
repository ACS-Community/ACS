/*******************************************************************************
* E.S.O. - VLT project
*
* "@(#) $Id: enumpropRWImpl.i,v 1.57 2008/10/01 02:33:31 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram 2003-05-30 added intalization to default value for external DevIO
* bjeram 2003-03-14 added baci::BACIValue::NullValue as a parameter to constructor of baci::Monitorpattern
* bjeram 2002-11-18 chnged to onchange monitor
* bjeram 2002-07-08 fixed set_async and set_nonblocking
* bjeram 2001-11-05 created 
*/

#define GET_ACTION 0
#define SET_ACTION 1

/*
TODO
CBpattern -> CBT
baci::Monitorpattern -> Monitor(T)
*/

template <ACS_ENUM_C>
RWEnumImpl<ACS_ENUM_T(T), SK>::RWEnumImpl(const ACE_CString& name, baci::BACIComponent* cob, DevIO<T> *devIO, bool flagdeldevIO) : 
    CharacteristicModelImpl(name, cob->getCharacteristicModel()), 
    initialization_m(1), destroyed_m(false), reference_mp(CORBA::Object::_nil()), property_mp(0),
    historyStart_m(-1), historyTurnaround_m(false), m_enumLength(0)
{

    ACS_TRACE("RWEnumImpl::RWEnumImpl");
  
    // initialize
    m_condition.length(0);
    m_statesDescription.length(0);
    // read static data
    if (!readCharacteristics())
	{
	std::string procName="RWEnumImpl::RWEnumImpl(";
        procName+=name.c_str();
        procName+=",...)";
	ACS_LOG(LM_RUNTIME_CONTEXT, "RWEnumImpl::RWEnumImpl",
		(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
	baciErrTypeProperty::PropertyStaticDataExImpl ex(__FILE__,__LINE__,procName.c_str());
	ex.addData("Property",name.c_str());
	throw ex;
	}
  
    // create BACI property instance
    CORBA::Any tAny;
    tAny <<= default_value();
    property_mp = new baci::BACIProperty(name.c_str(), this, this,
				  baci::BACIValue(default_value(), tAny), cob);
    if (!property_mp){
	std::string procName="RWEnumImpl::RWEnumImpl(";
        procName+=name.c_str();
        procName+=",...)";
	baciErrTypeProperty::PropertyCreationExImpl ex(__FILE__,__LINE__,procName.c_str());
	ex.addData("Property",name.c_str());
	throw ex;
    }

    reference_mp = BACI_CORBA::ActivateCORBAObject(this, name.c_str());
    if (CORBA::is_nil(reference_mp))
	{
	std::string procName="RWEnumImpl::RWEnumImpl(";
        procName+=name.c_str();
        procName+=",...)";
	ACS_LOG(LM_RUNTIME_CONTEXT, "RWEnumImpl::RWEnumImpl",
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
	new baci::Monitorpattern(i->c_str(), min_timer_trigger(), baci::BACIValue::NullValue, property_mp);
	}
	}
    state = (T)defaultValue_m;

    if (devIO!=0)
	{
	devIO_mp = devIO;
	deldevIO_m = flagdeldevIO;
	devIO_mp->m_initialize = initializeDevIO_m;
	if (devIO_mp->initializeValue()) 
	    {
	    ACS::Time timeStamp = getTimeStamp();

	    try
		{
		devIO_mp->write((T)defaultValue_m, timeStamp);
		}
	    catch (ACSErr::ACSbaseExImpl& ex) 
		{
		std::string procName="RWEnumImpl::RWEnumImpl(";
		procName+=name.c_str();
		procName+=",...)";
		baciErrTypeProperty::PropertySetInitValueExImpl newEx(ex.getErrorTrace(),__FILE__,__LINE__,procName.c_str());
		newEx.addData("Property",name.c_str());
		throw newEx;
		} 
	    catch (...) 
		{
		std::string procName="RWcommonImpl::RWcommonImpl(";
		procName+=name.c_str();
		procName+=",...)";
		baciErrTypeProperty::PropertySetInitValueExImpl newEx(__FILE__,__LINE__,procName.c_str());
		newEx.addData("Property",name.c_str());
		throw newEx;
		}

	    ACS_DEBUG("baci::RWEnumImpl<>::RWEnumImpl", "DevIO initial value set to the default value.");
	    }
	}
    else
	{
	deldevIO_m = true;
	m_value = (T)defaultValue_m; 
	devIO_mp = new DevIOMem<T> (m_value);
	ACS_DEBUG("ROEnumImpl::ROEnumImpl", "No DevIO provided - created DevIOMem.");
	}
 
    ACS_DEBUG_PARAM("RWEnumImpl::RWEnumImpl", "Successfully created %s.", name.c_str() );
	
    // property successfuly initialized
    initialization_m = 0;
}

template <ACS_ENUM_C>
RWEnumImpl<ACS_ENUM_T(T), SK>::~RWEnumImpl()
{
  ACS_TRACE("RWEnumImpl::~RWEnumImpl");

   if (deldevIO_m)
      delete devIO_mp;

  // destroy BACI property
  if (property_mp) {
    delete property_mp;
    property_mp = 0;
  }
}

template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::destroy()
{
  ACS_TRACE("RWEnumImpl::destroy");
  if (destroyed_m)
    return;
  destroyed_m = true;

  if (!CORBA::is_nil(reference_mp))
    {
	  // this calls delete on this object, so DO NOT use any of its variables anymore
	  if (!BACI_CORBA::DestroyCORBAObject(reference_mp))
	  {
		ACS_LOG(LM_RUNTIME_CONTEXT, "RWEnumImpl::~RWEnumImpl",
			(LM_ERROR, "Failed to destroy CORBA object '%s'", property_mp->getName()));
	  }
	 else
		this->_remove_ref();

    }
  
}

/* --------------- [ Action implementator interface ] -------------- */

template <ACS_ENUM_C>
baci::ActionRequest  RWEnumImpl<ACS_ENUM_T(T), SK>::invokeAction(int function,
		       baci::BACIComponent* cob, const int& callbackID, 
		       const CBDescIn& descIn, baci::BACIValue* value, 
		       Completion& completion, CBDescOut& descOut)
{
    CompletionImpl c;
    baci::ActionRequest req;
  // only one action
  // better implementation with array is possible

  switch (function) 
    {
    case GET_ACTION:
	req = this->getValueAction(cob, callbackID, descIn, value, c, descOut);
	break;
    case SET_ACTION:
	req = this->setValueAction(cob, callbackID, descIn, value, c, descOut);
	break;
/*   
 case INC_ACTION:
      req = incrementAction(cob, callbackID, descIn, value, c, descOut);
    case DEC_ACTION:
      req = decrementAction(cob, callbackID, descIn, value, c, descOut);
*/
      default:
      return baci::reqDestroy;
    }

  if (c.isErrorFree())
      {
      completion = c;
      }
  else
      {
      completion = baciErrTypeProperty::InvokeActionErrorCompletion(c,
					       __FILE__,
					       __LINE__,
					       "RWEnumImpl<>::invokeAction");
	}

  return req;
}

/* -------------- [ Property implementator interface ] -------------- */

template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::getValue(baci::BACIProperty* property,
		   baci::BACIValue* value, 
		   Completion &completion,
		   CBDescOut& descOut)
{
  ACE_UNUSED_ARG(property);
  ACE_UNUSED_ARG(descOut);
  ACS::pattern nval;
  T realVal;

  ACS::Time ts = getTimeStamp();
  try 
      {
      realVal = devIO_mp->read(ts); 
      nval = (ACS::pattern)realVal;
      }
  catch (ACSErr::ACSbaseExImpl& ex) 
      {
      completion = baciErrTypeDevIO::ReadErrorCompletion (ex, __FILE__, __LINE__,"RWEnumImpl<>::getValue");
      return;
      } 

  CORBA::Any tAny;
  tAny <<= realVal;
  value->enumValue( nval, tAny );


  // if there is no error add value to history
  // !!! to be done in a loop
  addValueToHistory(completion.timeStamp, nval);
	
  completion =  ACSErrTypeOK::ACSErrOKCompletion();
  completion.timeStamp = ts;
}
 
/* --------------- [ History support ] --------------- */

template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::addValueToHistory(ACS::Time time, ACS::pattern value)
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
baci::ActionRequest RWEnumImpl<ACS_ENUM_T(T), SK>::getValueAction(baci::BACIComponent* cob, const int& callbackID,
				       const CBDescIn& descIn, baci::BACIValue* value,
				       Completion& completion, CBDescOut& descOut)
{
  ACE_UNUSED_ARG(cob);
  ACE_UNUSED_ARG(callbackID);
  ACE_UNUSED_ARG(descIn);
  CompletionImpl co;

  getValue(property_mp, value, co/*mpletion*/, descOut);
  
  if (co.isErrorFree())
      {
      completion = co;
      }
  else
      {
      completion = baciErrTypeProperty::CanNotGetValueCompletion(co, 
					    __FILE__, 
					    __LINE__, 
					    "RWEnumImpl<>::getValue");
      }//if-else
  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return baci::reqInvokeDone;
}

/// async. set value action implementation
template <ACS_ENUM_C>
baci::ActionRequest RWEnumImpl<ACS_ENUM_T(T), SK>::setValueAction(baci::BACIComponent* cob, const int& callbackID,
			 const CBDescIn& descIn, baci::BACIValue* value,
			 Completion& completion, CBDescOut& descOut)
{
  ACE_UNUSED_ARG(cob);
  ACE_UNUSED_ARG(callbackID);
  ACE_UNUSED_ARG(descIn);
  CompletionImpl co;

  setValue(property_mp, value, co, descOut);
  
  if (co.isErrorFree())
      {
      completion = co;
      }
  else
      {
      completion = baciErrTypeProperty::CanNotSetValueCompletion(co, 
					    __FILE__, 
					    __LINE__, 
					    "RWEnumImpl<>::setValueAction");
      }
  // complete action requesting done invokation, 
  // otherwise return reqInvokeWorking and set descOut.estimated_timeout
  return baci::reqInvokeDone;
}

/// async. set value action implementation
template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::setValue(baci::BACIProperty* property,
		   baci::BACIValue* value, 
		   Completion &completion,
		   CBDescOut& descOut)
{
  ACE_UNUSED_ARG(property);
  ACE_UNUSED_ARG(descOut);

  ACS::Time ts = getTimeStamp();
  state = (T)(value->patternValue());
  try
      {
      devIO_mp->write( state, ts );
      }
  catch (ACSErr::ACSbaseExImpl& ex) 
      {
      completion = baciErrTypeDevIO::WriteErrorCompletion (ex, __FILE__, __LINE__,"RWcommonImpl<>::setValue");
      return;
      } 

  ACS_DEBUG_PARAM("RWEnumImpl::setValue", "Set state to: %u\n", state);
  completion=  ACSErrTypeOK::ACSErrOKCompletion();
  completion.timeStamp = ts;
}

/* ---------------------- [ CORBA interface ] ---------------------- */

template <ACS_ENUM_C>
bool RWEnumImpl<ACS_ENUM_T(T), SK>::readCharacteristics()
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

      str = dao->get_string("initialize_devio");
      if(strcmp(str.in(),"false") == 0 || strcmp(str.in(),"0")==0) initializeDevIO_m = false;
	else initializeDevIO_m = true;

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

#if 0
      dbl = dao->get_double("archive_min_int");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      m_archive_min_int = static_cast<CORBA::ULong>(dbl);

      dbl = dao->get_double("archive_max_int");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      m_archive_max_int = static_cast<CORBA::ULong>(dbl);

      m_archive_delta = dao->get_long("archive_delta");
#endif

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
char *RWEnumImpl<ACS_ENUM_T(T), SK>::name ()
{
  return CORBA::string_dup(property_mp->getName());           
}

template <ACS_ENUM_C>
char *RWEnumImpl<ACS_ENUM_T(T), SK>::characteristic_component_name ()
{
  return CORBA::string_dup (property_mp->getComponent()->getName());
}

template <ACS_ENUM_C>
char *RWEnumImpl<ACS_ENUM_T(T), SK>::description ()
{
  return CORBA::string_dup (m_description.c_str());
}

template <ACS_ENUM_C>
char *RWEnumImpl<ACS_ENUM_T(T), SK>::format ()
{
  return CORBA::string_dup (format_m.c_str());
}

template <ACS_ENUM_C>
char *RWEnumImpl<ACS_ENUM_T(T), SK>::units ()
{
  return CORBA::string_dup (units_m.c_str());
}

template <ACS_ENUM_C>
CORBA::Boolean RWEnumImpl<ACS_ENUM_T(T), SK>::initialize_devio ()
{
  return CORBA::Boolean(initializeDevIO_m);
}

template <ACS_ENUM_C>
ACS::pattern RWEnumImpl<ACS_ENUM_T(T), SK>::resolution ()
{
  return m_resolution;
}

template <ACS_ENUM_C>
T RWEnumImpl<ACS_ENUM_T(T), SK>::get_sync (ACSErr::Completion_out c
		    )
{
    CompletionImpl co;

    ACS::CBDescOut descOut;
    baci::BACIValue value(0.0);

    getValue(property_mp, &value, co, descOut);

    if (co.isErrorFree())
	{
	c = co.returnCompletion(false);
	}
    else
	{
	c = baciErrTypeProperty::CanNotGetValueCompletion(co, 
				     __FILE__, 
				     __LINE__, 
				     "RWEnumImpl&lt;&gt;::get_sync").returnCompletion(false);
     }//if-else

    return (T)(value.patternValue());
}

template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::get_async (CBpattern* cb,
		     const ACS::CBDescIn & desc
		     )
{
  property_mp->getComponent()->registerAction(baci::BACIValue::type_pattern, cb, 
				       desc, this, 0);
}

template <ACS_ENUM_C>
CORBA::Long RWEnumImpl<ACS_ENUM_T(T), SK>::get_history (CORBA::Long n_last_values,
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
ACS::Monitorpattern_ptr RWEnumImpl<ACS_ENUM_T(T), SK>::create_monitor (CBpattern* cb,
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
ACS::Monitor_ptr RWEnumImpl<ACS_ENUM_T(T), SK>::create_postponed_monitor (ACS::Time start_time,
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
ACS::TimeInterval RWEnumImpl<ACS_ENUM_T(T), SK>::default_timer_trigger ()
{
  return defaultTimerTrig_m;
}

template <ACS_ENUM_C>
ACS::TimeInterval RWEnumImpl<ACS_ENUM_T(T), SK>::min_timer_trigger ()
{
  return minTimerTrig_m;
}

template <ACS_ENUM_C>
T RWEnumImpl<ACS_ENUM_T(T), SK>::default_value ()
{
  return (T)defaultValue_m;
}

template <ACS_ENUM_C>
ACS::stringSeq *RWEnumImpl<ACS_ENUM_T(T), SK>::statesDescription ()
{

  // create and initialize the return parameter
  ACS::stringSeq_var result = new ACS::stringSeq(m_statesDescription);
  // to return, take the sequence away from the _var
  return result._retn();
}

template <ACS_ENUM_C>
ACS::ConditionSeq *RWEnumImpl<ACS_ENUM_T(T), SK>::condition ()
{

  // create and initialize the return parameter
  ACS::ConditionSeq_var result = new ACS::ConditionSeq(m_condition);
  // to return, take the sequence away from the _var
  return result._retn();
}

template <ACS_ENUM_C>
TSeq * RWEnumImpl<ACS_ENUM_T(T), SK>::allStates ( )
{
    TSeq *result = new TSeq();
    result->length(m_enumLength);
    // m_enumLength is read out of the length of the seq of states deescription which should be OK otherwise this method will not work correctly.

    for (int i=0; i<m_enumLength; i++)
	(*result)[i] = T(i);

    return result;  
}

 /* -------------------- [ RW interface ] -------------------- */
template <ACS_ENUM_C>
ACSErr::Completion *  RWEnumImpl<ACS_ENUM_T(T), SK>::set_sync ( T val ) 
{
    CompletionImpl co;
    CORBA::Any tAny;
    tAny <<= val;

  baci::BACIValue value((ACS::pattern)val, tAny);
  ACS::CBDescOut descOut;

  setValue(property_mp, &value, co, descOut);

  if (co.isErrorFree())
      {
      return co.returnCompletion(false);
      }
  else
      {
      baciErrTypeProperty::CanNotSetValueCompletion completion(co, 
					  __FILE__, 
					  __LINE__, 
					  "RWEnumImpl&lt;&gt;::set_sync");
      return completion.returnCompletion(false);
      }
}
	
template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::set_async ( T val, ACS::CBvoid_ptr cb, const ACS::CBDescIn & desc
		  ) 
{
    CORBA::Any tAny;
    tAny <<= val;
  baci::BACIValue value((ACS::pattern)val, tAny);
  property_mp->getComponent()->registerAction(baci::BACIValue::type_null, cb, 
				       desc, this, SET_ACTION, value);
}
	
template <ACS_ENUM_C>
void RWEnumImpl<ACS_ENUM_T(T), SK>::set_nonblocking ( T val)  
{
    ACSErr::CompletionImpl c;
    CORBA::Any tAny;
    tAny <<= val;
    baci::BACIValue value((ACS::pattern)val, tAny);
    ACS::CBDescOut descOut;

    this->setValue(property_mp, &value, c, descOut);
    if (!c.isErrorFree())
	{
	baciErrTypeProperty::CanNotSetValueCompletion completion(c, 
					    __FILE__, 
					    __LINE__, 
					    "RWEnumImpl&lt;&gt;::set_nonblocking");
	completion.log();
	}
}
