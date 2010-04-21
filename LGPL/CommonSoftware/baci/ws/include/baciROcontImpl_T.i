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

#include "baciROcontImpl_T.h"
#include "baciPcontImpl_T.i"
#include "baciROcommonImpl_T.i"
#include <cdbErrType.h>

template<ACS_RO_C> 
baci::ROcontImpl<ACS_RO_TL>::ROcontImpl(const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    ROcommonImpl<ACS_RO_TL>(name, component_p, devIO, flagdeldevIO),
    PcontImpl<ACS_P_TL>(name, this->getProperty(), component_p, devIO, flagdeldevIO)
{
  ACS_TRACE("baci::ROcontImpl&lt;&gt;::ROcontImpl");
  
  // read static data

  if (readCharacteristics()==false) 
    {
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcontImpl&lt;&gt;::ROcontImpdmmakel",
			(LM_ERROR, "Failed to read static data for '%s'", this->getProperty()->getName()));
		return;
    }
  
  if (this->monitorEventDispatcher_mp!=0 && this->alarmTimerTrig_m!=0)
      {
      this->alarmSystemMonitor_mp = new AlarmSystemMonitorCont<TS, PropType>(this, this->monitorEventDispatcher_mp);
      }//if

  ACS_DEBUG("baci::ROcontImpl&lt;&gt;::ROcontImpl", "Successfully created.");

  // property successfuly initialized
  this->initialization_m = 0;
}


template<ACS_RO_C> 
baci::ROcontImpl<ACS_RO_TL>::ROcontImpl(bool init, const ACE_CString& name, BACIComponent* component_p, DevIO<TM>* devIO, bool flagdeldevIO) : 
    ROcommonImpl<ACS_RO_TL>(init, name, component_p, devIO, flagdeldevIO),
    PcontImpl<ACS_P_TL>(name, this->getProperty(), component_p, devIO, flagdeldevIO)
{
  ACS_TRACE("baci::ROcontImpl&lt;&gt;::ROcontImpl");
  
  // read static data
  if (readCharacteristics()==false) 
    {
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcontImpl&lt;&gt;::ROcontImpl",
			(LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
		return;
    }
/* will be done in subclass  
  if (this->monitorEventDispatcher_mp!=0 && this->alarmTimerTrig_m!=0)
      {
      alarmSystemMonitor_mp = new AlarmSystemMonitorCont<T, PropType>(this, this->monitorEventDispatcher_mp);
      }//if
*/
  ACS_DEBUG("baci::ROcontImpl&lt;&gt;::ROcontImpl", "Successfully created.");
  // property successfuly initialized
  this->initialization_m = 0;
}

template<ACS_RO_C> baci::ROcontImpl<ACS_RO_TL>::~ROcontImpl()
{
  ACS_TRACE("baci::ROcontImpl&lt;&gt;::~ROcontImpl");
  if (this->alarmSystemMonitor_mp)
      {
      delete this->alarmSystemMonitor_mp;
      this->alarmSystemMonitor_mp = 0;
      }
}

template<ACS_RO_C> 
bool baci::ROcontImpl<ACS_RO_TL>::readCharacteristics()
{

  cdb::DAONode* dao = this->getDAONode();
  if (!dao)
      return false;
  
  try
      {
	  alarmLowOn_m = dao->getValue<TSM>("alarm_low_on");
	  alarmLowOff_m = dao->getValue<TSM>("alarm_low_off");
	  alarmHighOn_m = dao->getValue<TSM>("alarm_high_on");
	  alarmHighOff_m = dao->getValue<TSM>("alarm_high_off");

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


template<ACS_RO_C> 
TS baci::ROcontImpl<ACS_RO_TL>::alarm_low_on ()
{

  return CORBAMem<TS, TSM>::retn(alarmLowOn_m);
}


template<ACS_RO_C> 
TS baci::ROcontImpl<ACS_RO_TL>::alarm_low_off ()
{

  return CORBAMem<TS, TSM>::retn(alarmLowOff_m);
}

template<ACS_RO_C> 
TS baci::ROcontImpl<ACS_RO_TL>::alarm_high_on ()
{

  return CORBAMem<TS, TSM>::retn(alarmHighOn_m);
}

template<ACS_RO_C> 
TS baci::ROcontImpl<ACS_RO_TL>::alarm_high_off ()
{

  return CORBAMem<TS, TSM>::retn(alarmHighOff_m);
}

/* ---------------------- [ CORBA interface ] ---------------------- */

template<ACS_RO_C>
ACS::Subscription_ptr baci::ROcontImpl<ACS_RO_TL>::new_subscription_Alarm (TAlarm *cb,
					const ACS::CBDescIn & desc
					)
{
//TBD: this could be done just in the constructor
    if (this->alarmTimerTrig_m==0)
	{
	
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROcontImpl&lt;&gt;::new_subscription_Alarm",
		(LM_ERROR, "Can not create alarm dispatcher for %s because alarm_timer_trig=0", 
		 this->getProperty()->getName()));
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	}//

    
    if (this->monitorEventDispatcher_mp==0)
	{
	CBDescIn descIn;
	descIn.id_tag = 0;
	this->monitorEventDispatcher_mp = new MonitorEventDispatcher<TIN, TCB, POA_CB>(descIn, this->alarmTimerTrig_m, this->property_mp);
	
	if (this->monitorEventDispatcher_mp==0)
	    {
	    ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	    }
	}  

  AlarmEventStrategyCont<TS, PropType, TAlarm> * eventStrategy_p = 
    new AlarmEventStrategyCont<TS, PropType, TAlarm>(cb, desc, this->alarmTimerTrig_m, 
				 this, this->monitorEventDispatcher_mp);
  if (eventStrategy_p==0)
      {
      ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
      }

  ACS::Subscription_var subscription = 
    ACS::Subscription::_narrow(eventStrategy_p->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Subscription::_nil());

  return subscription._retn();
}

/*___oOo___*/























