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

#include "baciROdiscImpl_T.h"
#include "baciPcommonImpl_T.i"
#include "baciROcommonImpl_T.i"

template <ACS_RO_C> 
ROdiscImpl<ACS_RO_TL>::ROdiscImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO, int initalize) :
	ROcommonImpl<ACS_RO_TL>(name, component_p, devIO, flagdeldevIO) 
{
    ACS_TRACE("baci::ROdiscImpl&lt;&gt;::ROdiscImpl"); 
    this->initialization_m = initalize;
}

template <ACS_RO_C> 
ROdiscImpl<ACS_RO_TL>::ROdiscImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO,  int initalize) :
	ROcommonImpl<ACS_RO_TL>(init, name, component_p, devIO, flagdeldevIO) 
{
    ACS_TRACE("baci::ROdiscImpl&lt;&gt;::ROdiscImpl"); 
    this->initialization_m = initalize;
}

/* ---------------------- [ CORBA interface ] ---------------------- */

template<ACS_RO_C>
ACS::Subscription_ptr ROdiscImpl<ACS_RO_TL>::new_subscription_Alarm (TAlarm *cb,
					const ACS::CBDescIn & desc
					)
  throw (CORBA::SystemException)
{

  if (this->monitorEventDispatcher_mp==0)
  {
    CBDescIn descIn;
    descIn.id_tag = 0;
    this->monitorEventDispatcher_mp = new MonitorEventDispatcher<TIN, TCB, POA_CB>(descIn, this->alarmTimerTrig_m, this->property_mp);
	if (this->monitorEventDispatcher_mp==0)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
  }  

  AlarmEventStrategyDisc<TS, PropType, TAlarm> * eventStrategy_p = 
    new AlarmEventStrategyDisc<TS, PropType, TAlarm>(cb, desc, this->alarmTimerTrig_m, 
				 this, this->monitorEventDispatcher_mp);
  if (eventStrategy_p==0)
	  ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());

  ACS::Subscription_var subscription = 
    ACS::Subscription::_narrow(eventStrategy_p->getCORBAReference());
  ACE_CHECK_RETURN(ACS::Subscription::_nil());

  return subscription._retn();
}











