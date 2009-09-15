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

#include "baciROSeqContImpl_T.h"
#include "baciROcontImpl_T.i"

template <ACS_RO_C> 
baci::ROSeqContImpl<ACS_RO_TL>::ROSeqContImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO) :
    ROcontImpl<ACS_RO_TL>(false, name, component_p, devIO, flagdeldevIO)
{
    ACS_TRACE("baci::ROSeqContImpl&lt;&gt;::ROSeqContImpl"); 

    if ( this->devIO_mp->initializeValue()==true) 
	{
	//devIO_mp->write(defaultValue_m, timeStamp);
	ACS_DEBUG("baci::ROSeqContImpl&lt;&gt;::ROSeqContImpl", "DevIO initial value set not implemented yet.");
	}

    if (this->monitorEventDispatcher_mp!=0 && this->alarmTimerTrig_m!=0)
    {
    	this->alarmSystemMonitor_mp = new AlarmSystemMonitorSeqCont<TM, PropType>(this, this->monitorEventDispatcher_mp);
    }//if

  this->initialization_m = 0;   // property successfuly initialized
  ACS_DEBUG("baci::ROSeqContImpl&lt;&gt;::ROSeqContImpl", "Successfully created.");  
}

template <ACS_RO_C> 
baci::ROSeqContImpl<ACS_RO_TL>::~ROSeqContImpl()
{
    ACS_TRACE("baci::ROSeqContImpl&lt;&gt;::~ROSeqContImpl");
    if (this->alarmSystemMonitor_mp)
	{
	delete this->alarmSystemMonitor_mp;
	this->alarmSystemMonitor_mp = 0;
	}//if
}

template<ACS_RO_C>
ACS::Subscription_ptr baci::ROSeqContImpl<ACS_RO_TL>::new_subscription_Alarm (TAlarm *cb,
					const ACS::CBDescIn & desc
					)
{
//TBD: this could be done just in the constructor
    if (this->alarmTimerTrig_m==0)
	{
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROSeqContImpl&lt;&gt;::new_subscription_Alarm",
		(LM_ERROR, "Can not create alarm dispatcher for %s because alarm_timer_trig=0", 
		 this->getProperty()->getName()));
	ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	}//if

    if (this->monitorEventDispatcher_mp==0)
	{
	CBDescIn descIn;
	descIn.id_tag = 0;
	this->monitorEventDispatcher_mp = new MonitorEventDispatcher<TIN, TCB, POA_CB>(descIn, this->alarmTimerTrig_m, this->property_mp);
	
	if (this->monitorEventDispatcher_mp==0)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	}  

  AlarmEventStrategyContSeq<TM, PropType, TAlarm> * eventStrategy_p = 
    new AlarmEventStrategyContSeq<TM, PropType, TAlarm>(cb, desc, this->alarmTimerTrig_m, 
				 this, this->monitorEventDispatcher_mp);
  if (eventStrategy_p==0)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());

  ACS::Subscription_var subscription = 
    ACS::Subscription::_narrow(eventStrategy_p->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Subscription::_nil());

  return subscription._retn();
}// new_subscription_Alarm









