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

#include "baciROSeqDiscImpl_T.h"
#include "baciROdiscImpl_T.i"

template <ACS_RO_C> 
ROSeqDiscImpl<ACS_RO_TL>::ROSeqDiscImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO) :
    ROdiscImpl<ACS_RO_TL>(false, name, component_p, devIO, flagdeldevIO)
{
    ACS_TRACE("baci::ROSeqDiscImpl&lt;&gt;::ROSeqDiscImpl"); 

    if ( devIO_mp->initializeValue()) 
	{
	//ACS::Time timeStamp;
	//devIO_mp->write(defaultValue_m, timeStamp);
	ACS_DEBUG("baci::ROSeqDiscImpl&lt;&gt;::ROSeqDiscImpl", "DevIO initial value set not implemented yet.");
	}
#ifndef MAKE_VXWORKS
    if (this->monitorEventDispatcher_mp!=0 && this->alarmTimerTrig_m!=0)
	{
	alarmSystemMonitor_mp = new AlarmSystemMonitorSeqDisc<TM, PropType>(this, this->monitorEventDispatcher_mp);
	}//if
#endif
  initialization_m = 0;   // property successfuly initialized
  ACS_DEBUG("baci::ROSeqDiscImpl&lt;&gt;::ROSeqDiscImpl", "Successfully created.");  
}

template <ACS_RO_C> 
ROSeqDiscImpl<ACS_RO_TL>::~ROSeqDiscImpl()
{
    ACS_TRACE("baci::ROSeqDiscImpl&lt;&gt;::~ROSeqDiscImpl");
    if(alarmSystemMonitor_mp) 
	{
	delete alarmSystemMonitor_mp;
	alarmSystemMonitor_mp=0;
	}
}

template<ACS_RO_C>
ACS::Subscription_ptr ROSeqDiscImpl<ACS_RO_TL>::new_subscription_Alarm (TAlarm *cb,
					const ACS::CBDescIn & desc
					)
{
//TBD: this could be done just in the constructor
    if (this->alarmTimerTrig_m==0)
	{
	
	ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROSeqDiscImpl&lt;&gt;::new_subscription_Alarm",
		(LM_ERROR, "Can not create alarm dispatcher for %s because alarm_timer_trig=0", 
		 this->getProperty()->getName()));
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	}//

    if (monitorEventDispatcher_mp==0)
	{
	CBDescIn descIn;
	descIn.id_tag = 0;
	monitorEventDispatcher_mp = new MonitorEventDispatcher<TIN, TCB, POA_CB>(descIn, alarmTimerTrig_m, property_mp);

	if (!monitorEventDispatcher_mp)
	    {
	    ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	    }
	}//if 

  AlarmEventStrategyDiscSeq<TM, PropType, TAlarm> * eventStrategy = 
    new AlarmEventStrategyDiscSeq<TM, PropType, TAlarm>(cb, desc, alarmTimerTrig_m, 
				 this, monitorEventDispatcher_mp);
  if (!eventStrategy)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());

  ACS::Subscription_var subscription = 
    ACS::Subscription::_narrow(eventStrategy->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Subscription::_nil());

  return subscription._retn();
}// new_subscription_Alarm









