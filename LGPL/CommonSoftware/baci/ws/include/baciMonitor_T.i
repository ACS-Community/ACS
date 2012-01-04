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

template<ACS_MONITOR_C>
baci::Monitor<ACS_MONITOR_T>::Monitor(ACE_CString name,
					    const ACS::TimeInterval& minTriggerTime,
					    const baci::BACIValue& minTriggerValue,
					    BACIProperty* property) :
  initialization_m(1), monitor_mp(0), reference_mp(CORBA::Object::_nil())
{
  ACS_TRACE("baci::Monitor&lt;&gt;::Monitor");
  const char* state = BACIRecoveryManager::getInstance()->getObjectState(name.c_str());
  setObjectState(state, minTriggerTime, minTriggerValue, property);
  delete[] state;
  initialization_m = CORBA::is_nil(reference_mp);
}

template<ACS_MONITOR_C>
baci::Monitor<ACS_MONITOR_T>::Monitor(ACE_CString name_,
					    Callback_ptr callback_p,
					    const CBDescIn& inDesc,
					    const ACS::TimeInterval& triggerTime,
					    const baci::BACIValue& triggerValue,
					    const ACS::TimeInterval& minTriggerTime,
					    const baci::BACIValue& minTriggerValue,
					    BACIProperty* property,
					    const ACS::TimeInterval& transmitTime,
					    const BACIMonitor::UpdateMode& updateMode) :
  initialization_m(1), monitor_mp(0), reference_mp(CORBA::Object::_nil())
{
  ACS_TRACE("baci::Monitor&lt;&gt;::Monitor");

  char* cname_p = BACIRecoveryManager::getInstance()->generateObjectName(name_.c_str());
  ACE_CString name(cname_p);
  delete[] cname_p;

  ACS_DEBUG_PARAM("baci::Monitor&lt;&gt;::Monitor", "Name is: '%s'", name.c_str());

  int callbackID = property->getComponent()->registerCallback(TBACIValuetype,
							callback_p,
                                                        inDesc);
  if (!callbackID)
	return;

  monitor_mp = new BACIMonitor(name, callbackID, this,
			     triggerTime, triggerValue,
			     minTriggerTime, minTriggerValue,
			     property, transmitTime, updateMode);

  if (monitor_mp==0) {
	property->getComponent()->removeCallback(callbackID);
	return;
  }

  reference_mp = BACI_CORBA::ActivateCORBAObject(this, name.c_str());
  if (CORBA::is_nil(reference_mp)==true) {
    ACS_LOG(LM_RUNTIME_CONTEXT, "baci::Monitor&lt;&gt;::Monitor", (LM_ERROR, "Failed to activate CORBA object '%s'", name.c_str()));
    monitor_mp->destroy(); monitor_mp = 0;
    return;
  }
  else
    this->_remove_ref();

  BACIRecoveryManager::getInstance()->addRecoverableObject(this);

  initialization_m = 0;
}

template<ACS_MONITOR_C>
baci::Monitor<ACS_MONITOR_T>::~Monitor()
{
  ACS_TRACE("BACI::Monitor&lt;&gt;::~Monitor");
}

template<ACS_MONITOR_C>
int baci::Monitor<ACS_MONITOR_T>::getId(void)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::getId");
    return -1;
}

template<ACS_MONITOR_C> const char*
baci::Monitor<ACS_MONITOR_T>::getName(void)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::getName");
    return monitor_mp->getName();
}

template<ACS_MONITOR_C>
const char* baci::Monitor<ACS_MONITOR_T>::getObjectState(void) {
    ACS_TRACE("baci::Monitor&lt;&gt;::getObjectState");
  ACE_TCHAR *buffer_p;
  ACE_NEW_RETURN (buffer_p, ACE_TCHAR[MAX_RECORD_SIZE], 0);
  
  
  try
    {      
      BACICallback* bcb_p = monitor_mp->getProperty()->getComponent()->getCallback(monitor_mp->getCallbackID());
      if (bcb_p==0) return 0;

      CORBA::String_var ior = BACI_CORBA::getORB()->object_to_string(bcb_p->getCallback()
								     );
      

      unsigned long tag = bcb_p->getDescIn().id_tag;

      ACE_CString valueTrigger;
      monitor_mp->getTriggerValue().toString(valueTrigger);
      if (ACE_OS::strlen(valueTrigger.c_str())==0)
        valueTrigger = "0";
      
      HEADER_PRINT_GET_OBJECT_STATE

      return buffer_p;

    }
  catch(...)
    {
      ACS_SHORT_LOG((LM_ERROR, "baci::Monitor&lt;&gt;::getObjectState"));
    }
  

  return 0;

}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::setObjectState(const char* state)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::setObjectState");
  ACE_UNUSED_ARG(state);
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::setObjectState(const char* state,
							const ACS::TimeInterval& minTriggerTime,
							const baci::BACIValue& minTriggerValue,
							BACIProperty *property)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::setObjectState");
  ACE_TCHAR cname[MAX_NAME_LENGTH];
  ACE_TCHAR ior[MAX_IOR_LENGTH];

  CBDescIn descIn;
  unsigned long tag;
  ACS::TimeInterval timeTrigger;
  ACS::TimeInterval transmitTime;
  ACE_TCHAR valueTrigger[MAX_VALUE_LENGTH];
  int mode;
  unsigned int triggerOnValue, isSuspended;

  HEADER_SCAN_SET_OBJECT_STATE

  descIn.id_tag = tag;

  ACE_CString name(cname);

  
  try
    {

      CORBA::Object_var obj = BACI_CORBA::getORB()->string_to_object(ior);
      

      if (CORBA::is_nil(obj.in())==true)
	{
	  return;
	}
      ACS_DEBUG_PARAM("baci::Monitor&lt;&gt;::setObjectState", "Got obj for: '%s'", name.c_str());

      TCB * cb_p = TCB::_narrow(obj.in());
      
      ACS_DEBUG_PARAM("baci::Monitor&lt;&gt;::setObjectState", "Narrow OK: '%p'", (void *)cb_p);

      if (cb_p ==TCB::_nil())
	{
	  CORBA::release(cb_p);
	  return;
	}	
      ACS_DEBUG_PARAM("baci::Monitor&lt;&gt;::setObjectState", "Narrow OK and not nil for: '%s'", name.c_str());
      
      int callbackID = property->getComponent()->registerCallback(TBACIValuetype,
							    cb_p,
							    descIn);
      
      ACS_DEBUG_PARAM("baci::Monitor&lt;&gt;::setObjectState", "Recovering: '%s'", name.c_str());
      
      baci::BACIValue value(minTriggerValue);
      if (value.fromString(valueTrigger)==false)
	{
	  triggerOnValue = false;
	}
      
      ACS_NEW_RETURN(monitor_mp,
		     BACIMonitor(name.c_str(), callbackID, this,
				 timeTrigger, value,
				 minTriggerTime, minTriggerValue,
				 property, transmitTime,
				 BACIMonitor::UpdateMode(mode), isSuspended),
		     );
      monitor_mp->setTriggerOnValue(triggerOnValue);
      
      reference_mp = BACI_CORBA::ActivateCORBAObject(this, name.c_str());
      if (CORBA::is_nil(reference_mp)==true) {
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::Monitor&lt;&gt;::setObjectState", (LM_ERROR, "Failed to activate CORBA object '%s'", name.c_str()));
	monitor_mp->destroy(); monitor_mp = 0;
	CORBA::release(cb_p);
	return;
      }
      else
	this->_remove_ref();
      
      CORBA::release(cb_p);    
    }
  catch(...)
    {
      ACS_SHORT_LOG((LM_ERROR, "baci::Monitor&lt;&gt;::setObjectState"));
    }
  

}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::monitorDestroyed()
{
    ACS_TRACE("baci::Monitor&lt;&gt;::monitorDestroyed");
  destroy();
}

template<ACS_MONITOR_C> 
void baci::Monitor<ACS_MONITOR_T>::monitorStateChanged()
{
    ACS_TRACE("baci::Monitor&lt;&gt;::monitorStateChanged");
  BACIRecoveryManager::getInstance()->updateRecoverableObject(this);
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::suspend ()
{
    ACS_TRACE("baci::Monitor&lt;&gt;::suspend");
  monitor_mp->suspend();
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::resume ()
{
    ACS_TRACE("baci::Monitor&lt;&gt;::resume");
  monitor_mp->resume();
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::destroy ()
{
  ACS_TRACE("BACI::Monitor&lt;&gt;::destroy");

  BACIRecoveryManager::getInstance()->removeRecoverableObject(this);

  if (monitor_mp!=0) 
      { 
      monitor_mp->destroy(); 
      monitor_mp = 0; 
      }

  if (CORBA::is_nil(reference_mp)==false)
    {
	if (BACI_CORBA::DestroyCORBAObject(reference_mp)==false) {
	  ACS_LOG(LM_RUNTIME_CONTEXT, "baci::Monitor&lt;&gt;::destroy", (LM_ERROR, "Failed to destroy CORBA object"));	
	}
    }

}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::set_timer_trigger (ACS::TimeInterval timer)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::set_timer_trigger");
  monitor_mp->setTriggerTime(timer);
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::get_timer_trigger (ACS::TimeInterval_out timer)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::get_timer_trigger");
  timer = monitor_mp->getTriggerTime();
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::set_value_trigger (TCORBA delta, CORBA::Boolean enable
							    )
{
    ACS_TRACE("baci::Monitor&lt;&gt;::set_value_trigger");
  if (enable==false) monitor_mp->setTriggerOnValue(false);
  else {
    monitor_mp->setTriggerValue(baci::BACIValue(delta));
    monitor_mp->setTriggerOnValue(true);
  }
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::get_value_trigger (TCORBA_out delta, CORBA::Boolean_out enable
							    )
{
    ACS_TRACE("baci::Monitor&lt;&gt;::get_value_trigger");
  delta = monitor_mp->getTriggerValue().getValue(static_cast<TCORBA*>(0));
  enable = monitor_mp->getTriggerOnValue();
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::set_value_percent_trigger (CORBA::Double delta, CORBA::Boolean enable)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::set_value_percent_trigger");
    if(enable == false) monitor_mp->setTriggerOnValuePercent(false);
    else {
        monitor_mp->setTriggerValuePercent(baci::BACIValue(delta));
        monitor_mp->setTriggerOnValuePercent(true);
    }
}

template<ACS_MONITOR_C>
void baci::Monitor<ACS_MONITOR_T>::get_value_percent_trigger (CORBA::Double_out delta, CORBA::Boolean_out enable)
{
    ACS_TRACE("baci::Monitor&lt;&gt;::get_value_percent_trigger");
    delta = monitor_mp->getTriggerValuePercent().getValue(static_cast<CORBA::Double*>(0));
    enable = monitor_mp->getTriggerOnValuePercent();
}

template<ACS_MONITOR_C>
ACS::Time baci::Monitor<ACS_MONITOR_T>::start_time ()
{
ACS_TRACE("baci::Monitor&lt;&gt;::start_time");
  return monitor_mp->getTransmitTime();
}


template<ACS_MONITOR_BASIC_C> 
baci::MonitorBasic<ACS_MONITOR_BASIC_T>::MonitorBasic(ACE_CString name,
						     const ACS::TimeInterval& minTriggerTime,
						     const baci::BACIValue& minTriggerValue,
						     BACIProperty* property) :
  initialization_m(1), monitor_mp(0), reference_mp(CORBA::Object::_nil())
{
    ACE_UNUSED_ARG(minTriggerValue);
    ACS_TRACE("baci::MonitorBasic&lt;&gt;::MonitorBasic");
  const char* state_p = BACIRecoveryManager::getInstance()->getObjectState(name.c_str());
  setObjectState(state_p, minTriggerTime, property);
  delete[] state_p;
  initialization_m = CORBA::is_nil(reference_mp);
}

template<ACS_MONITOR_BASIC_C> 
baci::MonitorBasic<ACS_MONITOR_BASIC_T>::MonitorBasic(ACE_CString name_,
						Callback_ptr callback_p,
						const CBDescIn& inDesc,
						const ACS::TimeInterval& triggerTime,
						const baci::BACIValue& triggerValue,
						const ACS::TimeInterval& minTriggerTime,
						const baci::BACIValue& minTriggerValue,
						BACIProperty* property,
						const ACS::TimeInterval& transmitTime,
						const BACIMonitor::UpdateMode& updateMode) :
    initialization_m(1), monitor_mp(0), reference_mp(CORBA::Object::_nil())
{
    ACE_UNUSED_ARG(triggerValue);
    ACE_UNUSED_ARG(minTriggerValue);

  ACS_TRACE("baci::MonitorBasic&lt;&gt;::MonitorBasic");
  char* cname_p = BACIRecoveryManager::getInstance()->generateObjectName(name_.c_str());
  ACE_CString name(cname_p);
  delete[] cname_p;

  ACS_DEBUG_PARAM("baci::MonitorBasic&lt;&gt;::MonitorBasic", "Name is: '%s'", name.c_str());

  int callbackID = property->getComponent()->registerCallback(TBACIValuetype,
							callback_p,
							inDesc);
  if (callbackID==0)
      {
	return;
      }

  monitor_mp = new BACIMonitor(name, callbackID, this,
			     triggerTime, baci::BACIValue::NullValue,
			     minTriggerTime, baci::BACIValue::NullValue,
			     property, transmitTime, updateMode);

  if (monitor_mp==0) 
      {
      property->getComponent()->removeCallback(callbackID);
      return;
      }

  reference_mp = BACI_CORBA::ActivateCORBAObject(this, name.c_str());
  if (CORBA::is_nil(reference_mp)==true) {
    ACS_LOG(LM_RUNTIME_CONTEXT, "baci::MonitorBasic&lt;&gt;::MonitorBasic", (LM_ERROR, "Failed to activate CORBA object '%s'", name.c_str()));
    monitor_mp->destroy(); monitor_mp = 0;
    return;
  }
  else
    this->_remove_ref();

  BACIRecoveryManager::getInstance()->addRecoverableObject(this);

  initialization_m = 0;

}


template<ACS_MONITOR_BASIC_C>
baci::MonitorBasic<ACS_MONITOR_BASIC_T>::~MonitorBasic()
{
  ACS_TRACE("BACI::MonitorBasic&lt;&gt;::~MonitorBasic&lt;&gt;");
}

template<ACS_MONITOR_BASIC_C>
int baci::MonitorBasic<ACS_MONITOR_BASIC_T>::getId(void)
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::getId");
  return -1;
}

template<ACS_MONITOR_BASIC_C>
const char* baci::MonitorBasic<ACS_MONITOR_BASIC_T>::getName(void)
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::getName");
  return monitor_mp->getName();
}

template<ACS_MONITOR_BASIC_C>
const char* baci::MonitorBasic<ACS_MONITOR_BASIC_T>::getObjectState(void) {
ACS_TRACE("baci::MonitorBasic&lt;&gt;::getObjectState");
  ACE_TCHAR *buffer_p = 0;
  ACE_NEW_RETURN (buffer_p, ACE_TCHAR[MAX_RECORD_SIZE], 0);

  
  try
    {
      BACICallback* bcb_p = monitor_mp->getProperty()->getComponent()->getCallback(monitor_mp->getCallbackID());
      if (bcb_p==0) return 0;

      CORBA::String_var ior = BACI_CORBA::getORB()->object_to_string(bcb_p->getCallback()
								     );
      

      unsigned long tag = bcb_p->getDescIn().id_tag;

      IMPL_PRINT_GET_OBJECT_STATE

      return buffer_p;

    }
  catch(...)
    {
      ACS_SHORT_LOG((LM_ERROR, "baci::MonitorBasic&lt;&gt;::getObjectState"));
    }
  

  return 0;

}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::setObjectState(const char* state)
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::setObjectState");
  ACE_UNUSED_ARG(state);
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::setObjectState(const char* state,
			      const ACS::TimeInterval& minTriggerTime,
			      BACIProperty *property)
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::setObjectState");
  ACE_TCHAR cname[MAX_NAME_LENGTH];
  ACE_TCHAR ior[MAX_IOR_LENGTH];

  CBDescIn descIn;
  unsigned long tag;
  ACS::TimeInterval timeTrigger;
  ACS::TimeInterval transmitTime;
  int mode;
  unsigned int isSuspended;

  IMPL_SCAN_SET_OBJECT_STATE

  descIn.id_tag = tag;

  ACE_CString name(cname);

  
  try
    {

      CORBA::Object_var obj = BACI_CORBA::getORB()->string_to_object(ior);
      

      if (CORBA::is_nil(obj.in())==true)
	{
	  return;
	}

      TCB * cb_p = TCB::_narrow(obj.in());
      

      if (cb_p == TCB::_nil())
	{
	  CORBA::release(cb_p);
	  return;
	}
      
      int callbackID = property->getComponent()->registerCallback(TBACIValuetype,
							    cb_p,
							    descIn);
      
      ACS_DEBUG_PARAM("baci::MonitorBasic&lt;&gt;::setObjectState", "Recovering: '%s'", name.c_str());
      
      ACS_NEW_RETURN(monitor_mp,
		     BACIMonitor(name.c_str(), callbackID, this,
				 timeTrigger, baci::BACIValue::NullValue,
				 minTriggerTime, baci::BACIValue::NullValue,
				 property, transmitTime, BACIMonitor::UpdateMode(mode),
				 isSuspended),
		     );
      
      reference_mp = BACI_CORBA::ActivateCORBAObject(this, name.c_str());
      if (CORBA::is_nil(reference_mp)==true) {
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::MonitorBasic&lt;&gt;::setObjectState",
		(LM_ERROR, "Failed to activate CORBA object '%s'", name.c_str()));
	monitor_mp->destroy(); monitor_mp = 0;
	CORBA::release(cb_p);
	return;
      }
      else
	this->_remove_ref();
      
      CORBA::release(cb_p);
    }
  catch(...)
    {
      ACS_SHORT_LOG((LM_ERROR, "baci::MonitorBasic&lt;&gt;::getObjectState"));
    }
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::monitorDestroyed()
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::monitorDestroyed");
  destroy();
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::monitorStateChanged()
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::monitorStateChanged");
  BACIRecoveryManager::getInstance()->updateRecoverableObject(this);
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::suspend ()
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::suspend");
  monitor_mp->suspend();
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::resume ()
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::resume");
  monitor_mp->resume();
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::destroy ()
{
  ACS_TRACE("BACI::MonitorBasic&lt;&gt;::destroy");

  BACIRecoveryManager::getInstance()->removeRecoverableObject(this);

  if (monitor_mp != 0) { monitor_mp->destroy(); monitor_mp = 0; }

  if (CORBA::is_nil(reference_mp)==false)
    {
      if (BACI_CORBA::DestroyCORBAObject(reference_mp)==false) {
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::MonitorBasic&lt;&gt;::destroy",
		(LM_ERROR, "Failed to destroy CORBA object"));
	  }
    }

}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::set_timer_trigger (ACS::TimeInterval timer)
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::set_timer_trigger");
  monitor_mp->setTriggerTime(timer);
}

template<ACS_MONITOR_BASIC_C>
void baci::MonitorBasic<ACS_MONITOR_BASIC_T>::get_timer_trigger (ACS::TimeInterval_out timer)
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::get_timer_trigger");
  timer = monitor_mp->getTriggerTime();
}

template<ACS_MONITOR_BASIC_C>
ACS::Time baci::MonitorBasic<ACS_MONITOR_BASIC_T>::start_time ()
{
ACS_TRACE("baci::MonitorBasic&lt;&gt;::start_time");
  return monitor_mp->getTransmitTime();
}










