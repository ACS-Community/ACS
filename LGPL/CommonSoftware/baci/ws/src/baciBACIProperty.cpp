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
* "@(#) $Id: baciBACIProperty.cpp,v 1.13 2011/03/30 17:57:23 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/


#include "baciBACIProperty.h"
#include "baci.h"

#include "baciUtil.h"
#include "baciDB.h"


ACE_RCSID(baci, baci, "$Id: baciBACIProperty.cpp,v 1.13 2011/03/30 17:57:23 tstaig Exp $");

namespace baci {

/////////////////////////////////////////////////
// BACIProperty
/////////////////////////////////////////////////

BACIProperty::BACIProperty(const ACE_CString& _name,
			   PropertyImplementator* _propertyImplementator,
			   CharacteristicModelImpl *characteristicModel,
			   const BACIValue& _defaultValue,
			   BACIComponent* component_p) :
  name_m(_name), 
  propertyImplementator_mp(_propertyImplementator),
  characteristicModel_mp(characteristicModel),
  lastValue_m(_defaultValue), component_mp(component_p), 
  monitorVector_m(), triggerOnValueMonitor_m(false), triggerOnValuePercentMonitor_m(false),
  pollInterval_m(0), lastPollTime_m(0), monMinTriggerTime_m(0LL),
  inDestructionState_m(false)
{
  ACS_TRACE("baci::BACIProperty::BACIProperty");
  ACS_DEBUG_PARAM("baci::BACIProperty::BACIProperty", "Creating property '%s'", name_m.c_str());

  // check
  if (component_mp==0) 
    {
      ACS_LOG(0, "baci::BACIProperty::BACIProperty", 
	      (LM_ERROR, "Null Component given! Constuction of property %s aborted.", name_m.c_str()));
      return;
    }

  // should not be null type
  if (_defaultValue.isNull()!=0) 
    {
      ACS_LOG(0, "baci::BACIProperty::BACIProperty", 
	      (LM_ERROR, "Null-type default value given! Constuction of property %s aborted.", name_m.c_str()));
      return;
    }

  /// !!!! for Sequences default value is not the same as type
  type_m = _defaultValue.getType();

  /* setup archiver - read from DB */
  CORBA::ULong archive_priority = 3UL, archiveMinInt=0UL, archiveMaxInt=0UL;
  BACIValue archiveDelta;
  ACE_CString archiveMechanism, archiveSuppress;


  cdb::DAONode* dao = characteristicModel_mp->getDAONode();
  if (!dao)
      return;
  

  try
     {
	  archiveMechanism = dao->get_string("archive_mechanism");
	  archiveSuppress = dao->get_string("archive_suppress");
     }
  catch (...)
     {
	  // noop
     }

   try
      {
      archive_priority = dao->get_long("archive_priority");
      }
  catch (...)
      {
      // noop
      }

  try
      {
      CORBA::Double dbl = dao->get_double("archive_min_int");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      archiveMinInt = static_cast<CORBA::ULong>(dbl);
      }
  catch(...)
      {
      // noop
      }


  try
      {
      CORBA::Double dbl = dao->get_double("archive_max_int");
      dbl = dbl * static_cast<CORBA::Double>(10000000.0);
      archiveMaxInt = static_cast<CORBA::ULong>(dbl);
      }
  catch(...)
      {
      // noop
      }

  try
      {
      CORBA::String_var str = dao->get_string("archive_delta");
      // use no type definitions in stringified values, obtain it from defaultValue
      archiveDelta.setType(type_m);
      archiveDelta.fromString(str.in(), false);
      }
  catch (...)
      {
      // noop
      }

    if (
      archiveSuppress == "false" &&
      archiveMechanism == "notification_channel" &&
      (archiveMinInt!=0 ||
      archiveMaxInt!=0 ||
      (archiveDelta.isNull()==false && archiveDelta.noDelta()==false)))
    { 
      CBDescIn inDesc; 
      inDesc.id_tag = 0;
      int callbackID = component_mp->registerCallback(BACIValue::type_null, 0, inDesc);
      if (callbackID!=0)
        {
          // name must be w/o device name
          const char * pos_p = ACE_OS::strchr(name_m.c_str(), ':');
          ACE_CString archiverName;
          if (pos_p!=0)
	      {
	      archiverName = pos_p+1;
	      }
          else
	      {
	      archiverName = name_m;
	      }
	  archiver_mp = new BACIMonitor(archiverName, callbackID, 0,
				     archiveMaxInt, archiveDelta, 
				     archiveMinInt, BACIValue(BACIValue::NullValue),
				     this, 0, BACIMonitor::mumLast, true, false, true, archive_priority);
        }
    }
  else
    {
      ACS_LOG(LM_RUNTIME_CONTEXT, "baci::BACIProperty::BACIProperty",
    		  (LM_DEBUG, "Archiver disabled! (archive_suppress=%s, archive_mechanism=%s, archive_min_int=%d, archive_max_int=%d)",
    		  archiveSuppress.c_str(), archiveMechanism.c_str(),
    		  archiveMinInt, archiveMaxInt));
      archiver_mp = 0;
    }
    
  // add to Component's property list
  component_mp->addProperty(this);
}

BACIProperty::~BACIProperty() {
  ACS_TRACE("baci::BACIProperty::~BACIProperty");
  ACS_DEBUG_PARAM("baci::BACIProperty::~BACIProperty", "Destroying: %s", getName());
    
  // set destruction flag
  inDestructionState_m = true;

  component_mp->removeProperty(this);

  ACS_DEBUG_PARAM("baci::BACIProperty::~BACIProperty", "Now I will go through monitors of '%s'.", getName());
  /*    
  if (archiver_mp!=0) 
    delete archiver_mp;
  */

  ThreadSyncGuard guard(&monitorVectorMutex_m);
  BACIMonitorVector vectorCopy(monitorVector_m);

  for (BACIMonitorVector::iterator iter = vectorCopy.begin();
       iter != vectorCopy.end(); 
       iter++)
      {
      (*iter)->internalDestroy();
      }

  ACS_DEBUG_PARAM("baci::BACIProperty::~BACIProperty", "Property '%s' destroyed.", getName());
}
	
void BACIProperty::addMonitor(BACIMonitor* monitor) 
{
  ACS_TRACE("baci::BACIProperty::addMonitor");

  if (inDestructionState_m==true) 
      {
      return;
      }

  ThreadSyncGuard guard(&monitorVectorMutex_m);
  monitorVector_m.push_back(monitor);
  guard.release();
  updateMonitorStates();
}

void BACIProperty::removeMonitor(BACIMonitor* monitor)
{
  ACS_TRACE("baci::BACIProperty::removeMonitor");

  if (inDestructionState_m==true) 
      {
      return;
      }

  ThreadSyncGuard guard(&monitorVectorMutex_m);
  BACIMonitorVector::iterator i = find(monitorVector_m.begin(), monitorVector_m.end(), monitor);
  if (i!=monitorVector_m.end())
      {
      monitorVector_m.erase(i);
      }
  guard.release();
  updateMonitorStates();
}

void BACIProperty::dispatchMonitors(Completion& completion, CBDescOut& descOut)
{
  if (component_mp->isInDestructionState()==true || inDestructionState_m==true) 
      {
      return;
      }
  
  BACIValue lastValue = getLastValue();
  ACS::TimeInterval lastPollTime = getLastPollTime();
  //Completion lastCompletion = getLastCompletion();
  
  bool ok;

  ACS::TimeInterval monitorTriggerTime, monitorLastTime;
  BACIValue monitorTriggerValue;
  BACIValue monitorTriggerValuePercent;
  
  BACIMonitor* mon_p=0;

  ThreadSyncGuard guard(&monitorVectorMutex_m);

  for (int n=0; n<getMonitorCount() && inDestructionState_m==false; n++) 
    {
      mon_p = getMonitorAt(n);
      if (mon_p==0 || mon_p->isInDestructionState()==true || mon_p->isSuspended()==true ||
	  (mon_p->getTransmitTime()>lastPollTime)) 
	  {
	  continue;
	  }
      
      // monitor name (used for archiving)
      BACIMonitor * archiver_p = 0;
      if (mon_p->isArchivingMonitor()==true) 
	  {
	  archiver_p = mon_p;
	  }

      monitorTriggerTime = mon_p->getTriggerTime();
      monitorLastTime = mon_p->getLastTime();
      
      // on-time check
      if (monitorTriggerTime > 0) 
	  {	  
	  if ((lastPollTime-monitorLastTime)>=monitorTriggerTime) 
	      {
/*
  completion.timeStamp = mon->getLastTime();
  completion.code = ACSErr::ACSErrMonitorOnTimer;		//  time triggered
  bool ok = component_mp->dispatchCallback(mon->getCallbackID(), lastValue, descOut, completion, archiver);
  if (!ok && !component_mp->getCallback(mon->getCallbackID())->isOK() && !mon->isInDestructionState()) 
  {	// removed
  //ACS_DEBUG("baci::BACIProperty::dispatchMonitors", "Before internalDestroy()");
  mon->internalDestroy(); n--;
  continue;
  }
*/	    
	      switch (mon_p->getUpdateMode()) 
		  {
		  case BACIMonitor::mumLast: 
		  {
		  ACS::TimeInterval delay=calculateModulus(lastPollTime-monitorLastTime, monitorTriggerTime);
		  mon_p->setLastTime(lastPollTime-delay); 
		  break; 
		  }
		  case BACIMonitor::mumTrunc: 
		  {
		  mon_p->setLastTime(lastPollTime); 
		  break;
		  }
		  default: {}
		  }

	      if ( completion.previousError.length() == 0 ) // does completion contain an error ?
		  {
		  completion.timeStamp = mon_p->getLastTime();
		  completion.type = ACSErr::ACSErrTypeMonitor;
		  completion.code = ACSErrTypeMonitor::ACSErrMonitorOnTimer;	       //  time triggered
		  ok = component_mp->dispatchCallback(mon_p->getCallbackID(), lastValue, descOut, completion, archiver_p);
		  }
	      else
		  {
		    ACSErrTypeMonitor::ACSErrMonitorErrorCompletion c(completion, __FILE__, __LINE__, "BACIProperty::dispatchMonitors");
		  ok = component_mp->dispatchCallback(mon_p->getCallbackID(), lastValue, descOut, c, archiver_p);	  
		  }//if-else
		  
	      if (ok==false && 
		  component_mp->getCallback(mon_p->getCallbackID())->isOK()==false && 
		  mon_p->isInDestructionState()==false) 
		  {	// removed
		  //ACS_DEBUG("baci::BACIProperty::dispatchMonitors", "Before internalDestroy()");
		  mon_p->internalDestroy(); 
		  n--;
		  continue;
		  }//if
	      }//if
	  }//if
      


      // if this monitor is setup to be triggered when the property's underlying value
      // reaches a specific delta value
      if (mon_p->getTriggerOnValue() == true)
	  {
	  // monitorTriggerValue is a local copy of the monitor's trigger value
	  monitorTriggerValue = mon_p->getTriggerValue(); 
	  // if the trigger value is not null and contains a delta value do something
	  if ((monitorTriggerValue.isNull()==0) && (monitorTriggerValue.noDelta()==false)) 
	    {
	    //create a copy of the last monitored value
	    BACIValue monitorLastValue=mon_p->getLastValue();
	    
	    // if the last monitored value is not null OR the "lastValue" (DWF-this looks like
	    // a bug to me...why not use the local copy just created!!!)
	    if (( (monitorLastValue.isNull()!=0 ) || (lastValue.lessThanDelta(monitorLastValue, monitorTriggerValue)==false))
		  && 
		( (mon_p->isDeltaValueAndTimerInteraction()==false) || (lastPollTime-monitorLastTime)>=mon_p->getMinTriggerTime())
		  ) 
		{
		
		if ( completion.previousError.length() == 0 ) // does completion contain an error ?
		    {
		    completion.type = ACSErr::ACSErrTypeMonitor;
		    completion.code = ACSErrTypeMonitor::ACSErrMonitorOnValue;	 //  value triggered
		    ok = component_mp->dispatchCallback(mon_p->getCallbackID(), lastValue, descOut, completion, archiver_p);
		    }
		else
		    {
		    ACSErrTypeMonitor::ACSErrMonitorErrorCompletion c(completion, __FILE__, __LINE__, "BACIProperty::dispatchMonitors");
		    ok  = component_mp->dispatchCallback(mon_p->getCallbackID(), lastValue, descOut, c, archiver_p);	  
		    }//if-else
		
		  if (ok==false && 
		      component_mp->getCallback(mon_p->getCallbackID())->isOK()==false && 
		      mon_p->isInDestructionState()==false) 
		    {
		      mon_p->internalDestroy(); 
		      n--;
		      continue;
		    }
	      
		  mon_p->setLastValue(lastValue);
		  if (mon_p->isDeltaValueAndTimerInteraction()==true)
		      {
		      mon_p->setLastTime(lastPollTime);
		      }
		  //If we already executed the callback due to delta_value, then we don't
		  //require to check for delta_percent_value.
		  continue;
		}
	    }
	}      

		// if this monitor is setup to be triggered when the property's underlying value
		// reaches a percentual delta value
		if (mon_p->getTriggerOnValuePercent() == true) {
			// monitorTriggerValuePercent is a local copy of the monitor's trigger value percent
			monitorTriggerValuePercent = mon_p->getTriggerValuePercent(); 
			// if the trigger value percent is not null and contains a delta value do something
			if ((monitorTriggerValuePercent.isNull()==0) && (monitorTriggerValuePercent.noDelta()==false)) {
				//create a copy of the last monitored value
				BACIValue monitorLastValue=mon_p->getLastValue();
				
				// if the last monitored value is not null OR the "lastValue" (DWF-this looks like
				// a bug to me...why not use the local copy just created!!!)
				if (((monitorLastValue.isNull()!=0 ) || (lastValue.lessThanPercentDelta(monitorLastValue, monitorTriggerValuePercent)==false)) && 
					((mon_p->isDeltaValueAndTimerInteraction()==false) || (lastPollTime-monitorLastTime)>=mon_p->getMinTriggerTime())) {
					if ( completion.previousError.length() == 0 ) { // does completion contain an error ?
						completion.type = ACSErr::ACSErrTypeMonitor;
						completion.code = ACSErrTypeMonitor::ACSErrMonitorOnValuePercent;	 //  value triggered
						ok = component_mp->dispatchCallback(mon_p->getCallbackID(), lastValue, descOut, completion, archiver_p);
					} else {
						ACSErrTypeMonitor::ACSErrMonitorErrorCompletion c(completion, __FILE__, __LINE__, "BACIProperty::dispatchMonitors");
						ok = component_mp->dispatchCallback(mon_p->getCallbackID(), lastValue, descOut, c, archiver_p);
					}//if-else
					
					if (ok==false && component_mp->getCallback(mon_p->getCallbackID())->isOK()==false && 
						mon_p->isInDestructionState()==false) 
					{
						mon_p->internalDestroy(); 
						n--;
						continue;
					}
					
					mon_p->setLastValue(lastValue);
					if (mon_p->isDeltaValueAndTimerInteraction()==true)
					{
						mon_p->setLastTime(lastPollTime);
					}
					//This is here to replicate the structure as for delta_value checks.
					continue;
				}
			}
		}
	}
}

ACS::TimeInterval BACIProperty::GCD(ACS::TimeInterval t1, ACS::TimeInterval t2) 
{
  ACS_TRACE("baci::BACIProperty::GCD");
  if (t1==0) 
      {
      return t2;
      }
  else if (t2==0) 
      {
      return t1;
      }
  else if (t2>t1)
    {
      ACS::TimeInterval t = t2;
      t2=t1; 
      t1=t;
    }
  
  while ((t2!=0) && (t1>component_mp->getMTSleepTime())) 
    {
    ACS::TimeInterval t=calculateModulus(t1,t2); 
    t1=t2; 
    t2=t;
    }
  
  if (t1<component_mp->getMTSleepTime()) 
      {
      t1=component_mp->getMTSleepTime();
      }

  return t1;
}

void BACIProperty::updateMonitorStates() 
{
  ACS_TRACE("baci::BACIProperty::updateMonitorStates");
  //ThreadSyncGuard guard(&monitorVectorMutex_m, !isInDestructionState());
  
  // update pollInterval
  triggerOnValueMonitor_m=false;
  triggerOnValuePercentMonitor_m=false;
  setMonMinTriggerTime(0LL);
  ACS::TimeInterval pi=0;
  ACS::TimeInterval minTriggerTime = 0LL;
 
  for (int n=0; n<getMonitorCount(); n++) 
    {
      BACIMonitor *mon_p = getMonitorAt(n);
      if (mon_p!=0 && 
	  mon_p->isSuspended()==false) 
	{
	  if (mon_p->getTriggerOnValue() == true || mon_p->getTriggerOnValuePercent() == true)
	      {
	      triggerOnValueMonitor_m=mon_p->getTriggerOnValue();
	      triggerOnValuePercentMonitor_m=mon_p->getTriggerOnValuePercent();
	      //first time being set...take the monitor's value!
	      if (minTriggerTime==0)
		  {
		  minTriggerTime = mon_p->getMinTriggerTime();
		  }
	      //new monitor value is smaller...take it!
	      else if (minTriggerTime < mon_p->getMinTriggerTime())
		  {
		  minTriggerTime = mon_p->getMinTriggerTime();
		  }
	      }

	  pi=GCD(pi, mon_p->getTriggerTime());
	}
    }
  setPollInterval(pi);
  setMonMinTriggerTime(minTriggerTime);
}



 }; 

/*___oOo___*/


