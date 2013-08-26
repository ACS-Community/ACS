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
 * "@(#) $Id: baciROpattern.cpp,v 1.116 2010/04/20 15:29:12 bjeram Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * bjeram    2003/02/21  change to template impl
 * msekoran  2001/03/10  modified 
 */


#include "baciROpattern.h"
#include "baciAlarmSystemMonitorPattern.h"
#include "baciAlarm_T.i"
#include "baciAlarmPattern.h"
#include "baciROdiscImpl_T.i"
#include "baciMonitor_T.i"


namespace baci {

// we need it in enumprop
    template class MonitorEventDispatcher<ACS::pattern, ACS::CBpattern, POA_ACS::CBpattern>; 
    template class Monitor<ACS_MONITOR(pattern, ACS::pattern)>;

    template class ROcommonImpl<ACS_RO_T(pattern, ACS::pattern)>;
    template class ROdiscImpl<ACS_RO_T(pattern, ACS::pattern)>;

    ROpatternImpl::ROpatternImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<ACS::pattern> *devIO, bool flagdeldevIO) :
	ROdiscImpl<ACS_RO_T(pattern, ACS::pattern)>(name, component_p, devIO, flagdeldevIO, 1),
	PpatternImpl(name, this->getProperty())
    {
	// read static data
	if (readCharacteristics()==false) 
	    {
	    ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROpatternImpl&lt;&gt;::ROpatternImpl",
		    (LM_ERROR, "Failed to read static data for '%s'", name.c_str()));
	    return;
	    }

	if (this->monitorEventDispatcher_mp!=0 && this->alarmTimerTrig_m!=0)
	    {
	    alarmSystemMonitor_mp = new AlarmSystemMonitorPattern(this, this->monitorEventDispatcher_mp);
	    }//if
	initialization_m = 0;
	ACS_DEBUG("baci::ROpatternImpl::ROpatternImpl", "Successfully created.");
    }


    ROpatternImpl::~ROpatternImpl()
    {
	ACS_TRACE("baci::ROpatternImpl::~ROpatternImpl");
	if (alarmSystemMonitor_mp) 
	    {
	    delete alarmSystemMonitor_mp;
	    alarmSystemMonitor_mp = 0;
	    }
    }//~ROpatternImpl


    bool ROpatternImpl::readCharacteristics()
    {
	cdb::DAONode* dao = this->getDAONode();
	if (!dao)
	    return false;
  
	try
	    {
		alarmMask_m = dao->getValue<ACS::pattern>("alarm_mask");
	    alarmTrigger_m = dao->getValue<ACS::pattern>("alarm_trigger");

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

    ACS::pattern ROpatternImpl::alarm_mask ()
    {
	return alarmMask_m;
    }


    ACS::pattern ROpatternImpl::alarm_trigger ()
    {

	return alarmTrigger_m;
    }


    ACS::Subscription_ptr ROpatternImpl::new_subscription_Alarm (ACS::Alarmpattern *cb,
								 const ACS::CBDescIn & desc
	)
    {
//TBD: this could be done just in the constructor
	if (this->alarmTimerTrig_m==0)
	    {
	
	    ACS_LOG(LM_RUNTIME_CONTEXT, "baci::ROpatternImpl&lt;&gt;::new_subscription_Alarm",
		    (LM_ERROR, "Can not create alarm dispatcher for %s because alarm_timer_trig=0", 
		     this->getProperty()->getName()));

	    ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	    }//

	if (this->monitorEventDispatcher_mp==0)
	    {
	    CBDescIn descIn;
	    descIn.id_tag = 0;
	    this->monitorEventDispatcher_mp = new MonitorEventDispatcher<ACS::pattern, ACS::CBpattern, POA_ACS::CBpattern>(descIn, this->alarmTimerTrig_m, this->property_mp);
	
	    if (this->monitorEventDispatcher_mp==0)
		ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());
	    }  
	
	AlarmEventStrategyPattern * eventStrategy_p = 
	    new AlarmEventStrategyPattern(cb, desc, this->alarmTimerTrig_m, 
					  this, this->monitorEventDispatcher_mp);
	if (eventStrategy_p==0)
	    ACE_THROW_RETURN(CORBA::NO_RESOURCES(), ACS::Subscription::_nil());

	ACS::Subscription_var subscription = 
	    ACS::Subscription::_narrow(eventStrategy_p->getCORBAReference());
	ACE_CHECK_RETURN(ACS::Subscription::_nil());

	return subscription._retn();
    }


}; 


/*___oOo___*/


