/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration),
*    All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciRObooleanSeq.cpp,v 1.2 2012/10/09 14:22:54 bjeram Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
* msekoran  2002/02/10  created
*/

#include "baciRObooleanSeq.h"
#include "baciAlarmSystemMonitorBooleanSeq.h"
#include "baciAlarmBooleanSeq.h"
#include "baciAlarm_T.i"
#include "baciROcommonImpl_T.i"
//#include "baciPcommonImpl_T.i"
#include "baciPcontImpl_T.i"
#include "baciMonitor_T.i"

//template class baci::Monitor<ACS_MONITOR_SEQ(boolean, CORBA::Boolean)>;
//template class baci::ROcommonImpl<ACS_RO_BOOLSEQ_TL>;


namespace baci {

RObooleanSeq::RObooleanSeq(const ACE_CString& name, BACIComponent *component_p,
		DevIO<ACS::booleanSeq> *devIO, bool flagdeldevIO)
	: ROcommonImpl<ACS_RO_BOOLSEQ_TL>(name, component_p, devIO, flagdeldevIO),
	  PcontImpl<ACS_RO_BOOLSEQ_P>(name, this->getProperty(), component_p, devIO, flagdeldevIO),
	  alarmOn_m(true)
{
	ACS_TRACE("baci::RObooleanSeq::RObooleanSeq");

	if (readCharacteristics()==false)
	{
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::RObooleanSeq::RObooleanSeq",
			(LM_ERROR, "Failed to read static data for '%s'", this->getProperty()->getName()));
		return;
	}

	if ( this->devIO_mp->initializeValue() == true)
	{
		//devIO_mp->write(defaultValue_m, timeStamp);
		ACS_DEBUG("baci::RObooleanSeq::RObooleanSeq", "DevIO initial value set not implemented yet.");
	}

    if (this->monitorEventDispatcher_mp != 0 && this->alarmTimerTrig_m != 0)
    {
    	this->alarmSystemMonitor_mp = new AlarmSystemMonitorBooleanSeq(this, this->monitorEventDispatcher_mp);
    }

    this->initialization_m = 0;   // property successfuly initialized
}

RObooleanSeq::RObooleanSeq(bool init, const ACE_CString& name, BACIComponent *component_p,
		DevIO<ACS::booleanSeq> *devIO, bool flagdeldevIO)
     : ROcommonImpl<ACS_RO_BOOLSEQ_TL>(init, name, component_p, devIO, flagdeldevIO),
	   PcontImpl<ACS_RO_BOOLSEQ_P>(name, this->getProperty(), component_p, devIO, flagdeldevIO),
	   alarmOn_m(true)
{
	ACS_TRACE("baci::RObooleanSeq::RObooleanSeq");

	if (readCharacteristics()==false)
	{
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::RObooleanSeq::RObooleanSeq",
			(LM_ERROR, "Failed to read static data for '%s'", this->getProperty()->getName()));
		return;
	}

	if ( this->devIO_mp->initializeValue() == true)
	{
		//devIO_mp->write(defaultValue_m, timeStamp);
		ACS_DEBUG("baci::RObooleanSeq::RObooleanSeq", "DevIO initial value set not implemented yet.");
	}

    if (this->monitorEventDispatcher_mp != 0 && this->alarmTimerTrig_m != 0)
    {
    	this->alarmSystemMonitor_mp = new AlarmSystemMonitorBooleanSeq(this, this->monitorEventDispatcher_mp);
    }

    this->initialization_m = 0;   // property successfuly initialized
}

RObooleanSeq::~RObooleanSeq()
{
    ACS_TRACE("baci::RObooleanSeq::~RObooleanSeq");
    if (this->alarmSystemMonitor_mp)
	{
		delete this->alarmSystemMonitor_mp;
		this->alarmSystemMonitor_mp = 0;
	}//if
}

CORBA::Boolean RObooleanSeq::alarm_on ()
{
	return alarmOn_m;
}

ACS::Subscription_ptr RObooleanSeq::new_subscription_Alarm (
		ACS::Alarmboolean *cb, const ACS::CBDescIn & desc)
{
	//TBD: this could be done just in the constructor
	if (this->alarmTimerTrig_m == 0)
	{
		ACS_LOG(LM_RUNTIME_CONTEXT, "baci::RObooleanSeq::new_subscription_Alarm",
		(LM_ERROR, "Can not create alarm dispatcher for %s because alarm_timer_trig=0",
		 this->getProperty()->getName()));
		throw CORBA::NO_RESOURCES();
	}//if

	if (this->monitorEventDispatcher_mp == 0)
	{
		CBDescIn descIn;
		descIn.id_tag = 0;
		this->monitorEventDispatcher_mp = new MonitorEventDispatcher<const ACS::booleanSeq&/*TIN*/,
				ACS::CBbooleanSeq/*TCB*/, POA_ACS::CBbooleanSeq/*POA_CB*/>(descIn, this->alarmTimerTrig_m, this->property_mp);

		if (this->monitorEventDispatcher_mp == 0)
		{
			throw CORBA::NO_RESOURCES();
		}
	}

	AlarmEventStrategyBooleanSeq * eventStrategy_p = new AlarmEventStrategyBooleanSeq(cb, desc,
			this->alarmTimerTrig_m, this, this->monitorEventDispatcher_mp);
	if (eventStrategy_p == 0)
	{
		throw CORBA::NO_RESOURCES();
	}

	ACS::Subscription_var subscription =
			ACS::Subscription::_narrow(eventStrategy_p->getCORBAReference());

	return subscription._retn();
}

bool RObooleanSeq::readCharacteristics()
{
	ACS_TRACE("baci::RObooleanSeq::readCharacteristics");
	bool ret = false;

	cdb::DAONode* dao = this->getDAONode();

	if (dao != NULL)
	{
		try {
			alarmOn_m = dao->getValue<CORBA::Boolean>("alarm_on");
			ret = true;
		} catch(ACSErr::ACSbaseExImpl& ex) {
			ex.log();
		} catch(...) {
			ACS_LOG(LM_RUNTIME_CONTEXT, "baci::RObooleanSeq::readCharacteristics",
				(LM_ERROR, "Unknown exception"));
		}
	}

	return ret;
}

}


/*___oOo___*/
