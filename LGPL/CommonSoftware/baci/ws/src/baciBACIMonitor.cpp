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
* "@(#) $Id: baciBACIMonitor.cpp,v 1.9 2011/03/30 17:57:23 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

#include "baciBACIMonitor.h"
#include "baciBACIProperty.h"
#include "baci.h"
#include "baciUtil.h"

ACE_RCSID(baci, baci, "$Id: baciBACIMonitor.cpp,v 1.9 2011/03/30 17:57:23 tstaig Exp $");


namespace baci {
/////////////////////////////////////////////////
// BACIMonitor
/////////////////////////////////////////////////

BACIMonitor::BACIMonitor(const ACE_CString& _name, int _callbackID, 
			 MonitorImplementator* _monitorImplementator_p,
			 const ACS::TimeInterval& _triggerTime, const BACIValue& _triggerValue, 
			 const ACS::TimeInterval& _minTriggerTime, const BACIValue& _minTriggerValue, 
			 BACIProperty* _property, const ACS::TimeInterval& _transmitTime,
			 const UpdateMode& _updateMode, 
			 const bool _archivingMonitor,
			 const bool _suspended,
                         const bool _deltaValueAndTimerInteraction,
                         const unsigned int _priority) :

  name_m(_name), callbackID_m(_callbackID), 
  monitorImplementator_mp(_monitorImplementator_p), updateMode_m(mumLast),
  triggerTime_m(0), minTriggerTime_m(_minTriggerTime),
  transmitTime_m(0), userControlledTransmitTime_m(false), lastTime_m(0), 
  minTriggerValue_m(_minTriggerValue), triggerOnValue_m(false), triggerOnValuePercent_m(false),
  property_mp(_property), archivingMonitor_m(_archivingMonitor), 
  suspended_m(_suspended),
  deltaValueAndTimerInteraction_m(_deltaValueAndTimerInteraction),
  priority_m(_priority),
  inDestructionState_m(false),
  destroyed_m(false)
{
  ACS_TRACE("baci::BACIMonitor::BACIMonitor");

  ACS_LOG(0, "baci::BACIMonitor::BACIMonitor",
	  (LM_DEBUG, "Creating monitor '%s' (archiving: %d, triggerTime: %d)", 
	   name_m.c_str(), archivingMonitor_m, (unsigned)_triggerTime));
  if (archivingMonitor_m==true)
      {
      ACS_LOG(0, "baci::BACIMonitor::BACIMonitor",
	      (LM_DEBUG, "Archiver monitor with priority %d", priority_m));
      }

  // VxWorks (should not be as initializer)
  monitorImplementator_mp = 0;
  
  // set update mode
  setUpdateMode(_updateMode);

  // set monitors
  setTriggerTime(_triggerTime);
  setTriggerValue(_triggerValue);

  
  if (getTriggerValue().isNull()==0 && getTriggerValue().noDelta()==false)
      {
      setTriggerOnValue(true);
      }
  if (getTriggerValuePercent().isNull()==0 && getTriggerValuePercent().noDelta()==false)
      {
      setTriggerOnValuePercent(true);
      }
  
  // set transmit_time
  if (_transmitTime!=0)
    {
      userControlledTransmitTime_m = true;
      setTransmitTime(_transmitTime);    // user now handles monitor sync.
    }

  if (archivingMonitor_m==false) 
    {
      property_mp->getComponent()->getCallback(callbackID_m)->setRemoveOnFailure(false);
    }

  monitorImplementator_mp = _monitorImplementator_p;

  // add monitor
  property_mp->addMonitor(this);

}

BACIMonitor::~BACIMonitor() {
	
  destroyed_m = true;

  ACS_TRACE("baci::BACIMonitor::~BACIMonitor");
  ACS_DEBUG_PARAM("baci::BACIMonitor::~BACIMonitor", "Destroying monitor '%s'", name_m.c_str());

  // remove monitor (if not yet)
  property_mp->removeMonitor(this);

  // Call done() and remove CB
  if (archivingMonitor_m==false)
    {
      Completion completion = ACSErrTypeOK::ACSErrOKCompletion();

      CBDescOut descOut;
      if (property_mp->getComponent()->finishCallback(callbackID_m, 
					      property_mp->getLastValue(), 
					      descOut, completion)==false)
	  {
	  property_mp->getComponent()->removeCallback(callbackID_m);
	  }
    }

  ACS_DEBUG_PARAM("baci::BACIMonitor::~BACIMonitor", "Monitor '%s' destroyed.", name_m.c_str());

}

void BACIMonitor::destroy() 
{
  ACS_TRACE("baci::BACIMonitor::destroy");

  if (destroyed_m==true) 
      {
      return;
      }

  if (property_mp->isInDestructionState()==false)
      {
      delete this;
      }
  else if (inDestructionState_m==true)        // called via monitorImplementator_mp
      {
      delete this;
      }
  else
      {
      monitorImplementator_mp = 0;       // monitorImplementator_mp will destroy itself, while this instance later by ~BACIProperty
      }
}

void BACIMonitor::internalDestroy() 
{
  ACS_TRACE("baci::BACIMonitor::internalDestroy");

  if (inDestructionState_m==true) 
      {
      return;
      }

  inDestructionState_m = true;

  // remove monitor
  property_mp->removeMonitor(this);

  /// via monitorImplementator_mp
  if (monitorImplementator_mp!=0)
      {
      monitorImplementator_mp->monitorDestroyed();
      }
  else 
      {
      destroy();
      }
}


void BACIMonitor::setTriggerOnValue(bool enable)
{
  ACS_TRACE("baci::BACIMonitor::setTriggerOnValue");
  if (triggerOnValue_m!=enable) {
    triggerOnValue_m=enable;
    if((triggerOnValue_m == true || triggerOnValuePercent_m == true) && triggerTime_m > 0)
      deltaValueAndTimerInteraction_m = true;
    else
      deltaValueAndTimerInteraction_m = false;
    monitorStateChanged();
    property_mp->updateMonitorStates();
  }
}

void BACIMonitor::setTriggerOnValuePercent(bool enable)
{
  ACS_TRACE("baci::BACIMonitor::setTriggerOnValuePercent");
  if (triggerOnValuePercent_m!=enable) {
    triggerOnValuePercent_m=enable;
    if((triggerOnValue_m == true || triggerOnValuePercent_m == true) && triggerTime_m > 0)
      deltaValueAndTimerInteraction_m = true;
    else
      deltaValueAndTimerInteraction_m = false;
    monitorStateChanged();
    property_mp->updateMonitorStates();
  }
}

void BACIMonitor::suspend() 
{
  ACS_TRACE("baci::BACIMonitor::suspend");
  ACS_DEBUG_PARAM("baci::BACIMonitor::suspend", "'%s' suspended", name_m.c_str());
  if (suspended_m==false) {
    suspended_m=true;
    monitorStateChanged();
    property_mp->updateMonitorStates();
  }
}

void BACIMonitor::resume()
{
  ACS_TRACE("baci::BACIMonitor::resume");
  ACS_DEBUG_PARAM("baci::BACIMonitor::resume", "'%s' resumed", name_m.c_str());
  if (suspended_m==true) {
    suspended_m=false;
    monitorStateChanged();
    property_mp->updateMonitorStates();
  }
}

void BACIMonitor::setUpdateMode(const UpdateMode& _updateMode) 
{ 
    ACS_TRACE("baci::BACIMonitor::setUpdateMode");
  if (updateMode_m!=_updateMode) {
    updateMode_m=_updateMode;
    monitorStateChanged();
  }
}

void BACIMonitor::setTriggerTime(const ACS::TimeInterval& _triggerTime)
{
  ACS_TRACE("baci::BACIMonitor::setTriggerTime");
  if (_triggerTime != triggerTime_m)
    {
      if (_triggerTime<=0)
	  {
	  triggerTime_m=0;
	  }
      else if (_triggerTime < minTriggerTime_m)
	  {
	  triggerTime_m=minTriggerTime_m; 
	  }
      else 
	  {
	  triggerTime_m=_triggerTime;
	  }

      setLastTime(getTimeStamp()-getTriggerTime());

      if((triggerOnValue_m == true || triggerOnValuePercent_m == true) && triggerTime_m > 0)
        deltaValueAndTimerInteraction_m = true;
      else
        deltaValueAndTimerInteraction_m = false;
      monitorStateChanged();
      property_mp->updateMonitorStates();
    }
}

void BACIMonitor::setMinTriggerTime(const ACS::TimeInterval& _minTriggerTime) 
{
  ACS_TRACE("baci::BACIMonitor::setMinTriggerTime");
  minTriggerTime_m=_minTriggerTime; 
  if (triggerTime_m< minTriggerTime_m && triggerTime_m!=0) 
      {
      setTriggerTime(minTriggerTime_m);
      }
}

void BACIMonitor::setLastTime(const ACS::TimeInterval& _lastTime) 
{ 
  lastTime_m=_lastTime; 

  // set transmit time
  //transmitTime_m=lastTime_m+triggerTime_m;
}

void BACIMonitor::setTriggerValue(const BACIValue& _triggerValue) 
{
  ACS_TRACE("baci::BACIMonitor::setTriggerValue");
  if ((_triggerValue==triggerValue_m)==false) 
    {
      if (_triggerValue < minTriggerValue_m)
	  {
	  triggerValue_m = minTriggerValue_m; 
	  }
      else 
	  {
	  triggerValue_m=_triggerValue;
	  }
      monitorStateChanged();
    }
}

void BACIMonitor::setTriggerValuePercent(const BACIValue& _triggerValuePercent) 
{
	//TODO
	ACS_TRACE("baci::BACIMonitor::setTriggerValuePercent");
	if ((_triggerValuePercent==triggerValuePercent_m)==false) {
		if (_triggerValuePercent < minTriggerValue_m) {
			triggerValue_m = minTriggerValue_m; 
		} else {
			triggerValuePercent_m=_triggerValuePercent;
		}
		monitorStateChanged();
	}
}

void BACIMonitor::setMinTriggerValue(const BACIValue& _minTriggerValue) 
{
  ACS_TRACE("baci::BACIMonitor::setMinTriggerValue");
  minTriggerValue_m=_minTriggerValue;
  if (triggerValue_m<minTriggerValue_m && triggerValue_m.noDelta()==false) 
      {
      setTriggerValue(minTriggerValue_m);
      }
}

void BACIMonitor::setLastValue(const BACIValue& _lastValue) 
{ 
  ACS_TRACE("baci::BACIMonitor::setLastValue");
  lastValue_m=_lastValue;
}

 }; 

/*___oOo___*/
