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
* "@(#) $Id: baciBACIComponent.cpp,v 1.24 2011/03/30 17:57:23 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

#include "baciBACIComponent.h"
#include "baciUtil.h"
#include "baciError.h"
#include "logging.h"

ACE_RCSID(baci, baci, "$Id: baciBACIComponent.cpp,v 1.24 2011/03/30 17:57:23 tstaig Exp $");

using namespace baciErrTypeProperty;
using namespace ACSErrTypeCommon;

namespace baci {


/////////////////////////////////////////////////
// Action Thread
/////////////////////////////////////////////////

void actionThreadWorker(void* param) {

  if (param==0) return;

  BACIThreadParameter* baciParameter_p = static_cast<BACIThreadParameter*>(param);
  BACIThread* myself_p = baciParameter_p->getBACIThread();
  BACIComponent* component_p = (BACIComponent*)baciParameter_p->getParameter();

  if (BACIThread::InitThread)
      {
      BACIThread::InitThread(myself_p->getName().c_str());
      }

  ACS_TRACE("baci::actionThreadWorker");

  BACIAction* action_p = 0;

  bool pushActionBack;
  int callbackID;
  BACICallback* callback_p = 0;
  CBDescOut descOut;
  ActionRequest request = reqNone;

  ACS_DEBUG_PARAM("baci::actionThreadWorker", "%s", component_p->getName());

  while (myself_p->check()==true &&
	 component_p->isInDestructionState()==false)
    {
      if (myself_p->isSuspended()==false &&
	  component_p->isInDestructionState()==false &&
	  (component_p->getActionCount()>0))
	{
	  if ( (action_p = component_p->popAction()) != 0 )
	    {
	      Completion completion;

	      callbackID = action_p->getCallbackID();
	      callback_p = component_p->getCallback(callbackID);

	      if (callback_p==0)
		{
		  //ACS_LOG(LM_RUNTIME_CONTEXT, "baci::actionThreadWorker",
		  //		(LM_CRITICAL, "Callback not found: %s", component_p->getName()));
		  delete action_p;
		  continue;
		}

              if (action_p->isCompleted()==true)
		{
		  completion = action_p->getCompletion();
		  request = reqInvokeDone;
		}
              else
		{
		CompletionImpl co;
		request = action_p->invoke(callback_p->getComponent(),
					   callbackID,
					   callback_p->getDescIn(),
					   action_p->getValueRef(),
					   co,
					   descOut);
		if (co.isErrorFree())
		    {
		    completion = co;
		    }
		else
		    {
		    completion = CouldntPerformActionCompletion(co,
							  __FILE__,
							  __LINE__,
							  "baci::actionThreadWorker");					    }//if-else
		}//if-else

	      pushActionBack = false;

	      switch (request)
		{
		case reqNone: break;
		case reqInvokeWorking:
		  component_p->dispatchCallback(callbackID, action_p->getValue(), descOut, completion);
                  pushActionBack = true;
		  break;
		case reqInvokeDone:
		  pushActionBack = !component_p->finishCallback(callbackID, action_p->getValue(),
							descOut, completion);
		  if (pushActionBack==false)
		    {
		      //component_p->removeCallback(callbackID); /// already removed
		      delete action_p;
		    }
                  else if (callback_p->isOK()==true)
		      {
		      action_p->setCompletion(completion);
		      }
                  else
		    {
		      pushActionBack = false;      // drop message
		      //component_p->removeCallback(callbackID);
		      delete action_p;
		    }
		  break;
		case reqDestroy:
		  component_p->removeCallback(callbackID);
		  delete action_p;
		default:
		  // error msg
		  ACS_LOG(LM_RUNTIME_CONTEXT, "baci::actionThreadWorker",
			  (LM_ERROR, "Wrong request for action in Component %s! Return value must type of 'enum ActionRequest { reqNone, reqInvokeWorking, reqInvokeDone, reqDestroy }'",
			   component_p->getName()));
		  break;
		}

	      if (pushActionBack==true)
		  {
		  component_p->pushAction(action_p);
		  }
	    }
	}
      if (myself_p->exitRequested()==false)
	  {
	  myself_p->sleep();
	  }
    }

  delete baciParameter_p;
  myself_p->setStopped();

  if (BACIThread::DoneThread)
      {
      BACIThread::DoneThread();
      }
}


/////////////////////////////////////////////////
// Monitor Thread
/////////////////////////////////////////////////

void monitorThreadWorker(void* param) {

  if (param==0)
      {
      return;
      }

  BACIThreadParameter* baciParameter_p = static_cast<BACIThreadParameter*>(param);
  BACIThread* myself_p = baciParameter_p->getBACIThread();
  BACIComponent* component_p = (BACIComponent*)baciParameter_p->getParameter();

  if (BACIThread::InitThread)
      {
      BACIThread::InitThread(myself_p->getName().c_str());
      }

  ACS_TRACE("baci::monitorThreadWorker");

  ACS::TimeInterval pollInterval, time, lastPollTime;
  BACIValue value;
  bool timeTriggered;
  BACIProperty* property_p=0;
  CBDescOut descOut;

  ACS_DEBUG_PARAM("baci::monitorThreadWorker", "%s", component_p->getName());

  // already in getPropertAt ?!!!
  //ThreadSyncGuard guard(component_p->&property_mpVectorMutex);

  while (myself_p->check()==true &&
	 component_p->isInDestructionState()==false)
    {
      if (myself_p->isSuspended()==false)
	{
	  // sync. monitors
	  time = getTimeStamp();

	  //guard.acquire();
	  for (int n=0;
	       component_p->isInDestructionState()==false && n < component_p->getPropertyCount() && myself_p->exitRequested()==false;
	       n++)
	    {
	      property_p = component_p->getPropertyAt(n);
	      if (property_p==0 && property_p->isInDestructionState()==false)
		  {
		  continue;
		  }
	      if ((property_p->getMonitorCount() > 0) &&
		  ((property_p->getPollInterval() > 0) || property_p->hasTriggerOnValueMonitor()==true ||
		  property_p->hasTriggerOnValuePercentMonitor()==true))
		  {
		  pollInterval = property_p->getPollInterval();
		  //if we're dealing with a property containing trigger by value monitors AND
		  //the minimum monitor time is NOT default AND
		  //it's less than the main polling interval...

		  if ((property_p->hasTriggerOnValueMonitor()==true ||
				property_p->hasTriggerOnValuePercentMonitor()==true) &&
		      (property_p->getMonMinTriggerTime()!=0) &&
		      (property_p->getMonMinTriggerTime()<pollInterval))
		      {
		      pollInterval = property_p->getMonMinTriggerTime();
		      }


		  lastPollTime = property_p->getLastPollTime();
		  //time = getTimeStamp();

		  // time fix
		  ACS::TimeInterval etime = time;
		  if (pollInterval!=0)
		    {
		      etime -= calculateModulus(time-lastPollTime, pollInterval);
		      timeTriggered = (etime-lastPollTime)>=pollInterval;
		    }
		  else
		      {
		      timeTriggered = false;
		      }

		  //timeTriggered = (time-lastPollTime)>=pollInterval;
		  //if (timeTriggered==true)//property_p->hasTriggerOnValueMonitor() || timeTriggered)
		  if (property_p->hasTriggerOnValueMonitor() || property_p->hasTriggerOnValuePercentMonitor() || timeTriggered == true)
		    {
		    CompletionImpl co;

			  value.reset();
		      property_p->getValue(property_p,
					   (BACIValue*)&value,
					   co,
					   descOut);

		      property_p->setLastValue(value); // !!!
		      //property_p->setLastCompletion(completion); // !!!
		      if (timeTriggered==true)
			  {
			  property_p->setLastPollTime(etime); // !!! do not set in case of error
			  }
		      //property_p->setLastPollTime(time); // !!! do not set in case of error
		      if( co.isErrorFree() )
			  {
			  property_p->dispatchMonitors(co, descOut);
			  }
		      else
			  {
			  CanNotGetValueCompletion errComp(co,
							  __FILE__,
							  __LINE__,
							  "baci::monitorThreadWorker");
			  property_p->dispatchMonitors(errComp, descOut);
			  }//if-else
		    }
		}
	    }
	  //guard.release();

	}
      if (myself_p->exitRequested()==false)
	  {
	  myself_p->sleep();
	  }
    }

  delete baciParameter_p;
  myself_p->setStopped();

  if (BACIThread::DoneThread)
      {
      BACIThread::DoneThread();
      }
}
/////////////////////////////////////////////////
// BACIComponent
/////////////////////////////////////////////////

const ACS::TimeInterval BACIComponent::defaultRTResponseTime_m=5*1000*1000*10;         // 5s.
const ACS::TimeInterval BACIComponent::minRTSleepTime_m=50*1000*10;		          // 50ms

const ACS::TimeInterval BACIComponent::defaultMTResponseTime_m=5*1000*1000*10;    // 5s
const ACS::TimeInterval BACIComponent::minMTSleepTime_m=25*1000*10;	          // 25ms

BACIComponent::BACIComponent( ACS::ThreadManager *thrMgr,
			      const ACE_CString& _name,
			      const ACE_CString& _type,
			      CharacteristicModelImpl *characteristicModel,
			      size_t actionThreadStackSize,
			      size_t monitorThreadStackSize,
			      const ACS::TimeInterval& _actionThreadResponseTime,
			      const ACS::TimeInterval& _actionThreadSleepTime,
			      const ACS::TimeInterval& _monitorThreadResponseTime,
			      const ACS::TimeInterval& _monitorThreadSleepTime) :
    name_m(_name),
    type_m(_type),
    characteristicModel_mp(characteristicModel),
    actionThread_mp(BACIThread::NullBACIThread),
    monitorThread_mp(BACIThread::NullBACIThread),
    threadManager_mp(thrMgr),
    inDestructionState_m(false),
    actionThreadStackSize_m(actionThreadStackSize),
    monitoringThreadStackSize_m(monitorThreadStackSize)
{

  ACS_TRACE("baci::BACIComponent::BACIComponent");
  ACS_DEBUG_PARAM("baci::BACIComponent::BACIComponent", "Creating Component '%s'", name_m.c_str());

  // set action thread sleep time
  setRTResponseTime(_actionThreadResponseTime);
  setRTSleepTime(_actionThreadSleepTime);

  // set monitor thread sleep time
  setMTResponseTime(_monitorThreadResponseTime);
  setMTSleepTime(_monitorThreadSleepTime);
}//BACIComponent

BACIComponent::~BACIComponent()
{
  ACS_TRACE("baci::BACIComponent::~BACIComponent");

  // set destruction flag
  inDestructionState_m = true;

  if (threadManager_mp!=0)
      {
      threadManager_mp->terminateAll();
      }

  // the threads are no more alive, so I can ...

  // remove all left actions (in case it was canceled)
  BACIAction *action_p;
  ThreadSyncGuard guard(&actionQueueMutex_m);
  while (getActionCount() > 0)
    {
      action_p = popAction();
      if (action_p)
	  {
	  delete action_p;
	  }
    }
  guard.release();

  // remove all left callbacks
  BACICallback* callback_p;
  ThreadSyncGuard guard1(&callbackTableMutex_m);
  int h = callbackTable_m.first();
  while (h != 0)
    {
      int nh = callbackTable_m.next(h);
      callback_p = callbackTable_m[h];
      callbackTable_m.deallocate(h);
      delete callback_p;
      h = nh;
    }
  guard1.release();

  ACS_DEBUG_PARAM("baci::BACIComponent::~BACIComponent", "Component '%s' destroyed.", name_m.c_str());
}//~BACIComponent


void BACIComponent::startMonitoringThread() 
{
  ACS_TRACE("baci::BACIComponent::startMonitoringThread");

  if (!threadManager_mp){
	ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "BACIComponent::startMonitoringThread");
	ex.setVariable("threadManager_mp");
	throw ex;
  }//if

  if (monitorThread_mp == BACIThread::NullBACIThread)
      {
      monitorThread_mp=threadManager_mp->create(name_m+"::monitorThread",
						(void*)monitorThreadWorker, (void*)this,
						getMTResponseTime(), getMTSleepTime(),
						(THR_NEW_LWP | THR_DETACHED),
						monitoringThreadStackSize_m);
      }
  if (monitorThread_mp != BACIThread::NullBACIThread)
      {
      monitorThread_mp->resume();
      }
  else
      {
      throw acsthreadErrType::CanNotCreateThreadExImpl(__FILE__, __LINE__, "BACIComponent::startMonitoringThread");
      }//if-else
}//startMonitoringThread

void BACIComponent::startActionThread() 
{
  ACS_TRACE("baci::BACIComponent::startActionThread");

  if (!threadManager_mp)
      {
      ACSErrTypeCommon::NullPointerExImpl ex(__FILE__, __LINE__, "BACIComponent::startActionThread");
      ex.setVariable("threadManager_mp");
      throw ex;
      }//if

  if (actionThread_mp == BACIThread::NullBACIThread)
      {
      actionThread_mp=threadManager_mp->create(name_m+"::actionThread",
					       (void*)actionThreadWorker, (void*)this,
					       getRTResponseTime(), getRTSleepTime(),
					       THR_NEW_LWP | THR_DETACHED,
					       actionThreadStackSize_m);
      }
  if (actionThread_mp != BACIThread::NullBACIThread)
      {
      actionThread_mp->resume();
      }
  else
      {
      throw acsthreadErrType::CanNotCreateThreadExImpl(__FILE__, __LINE__, "BACIComponent::startActionThread");
      }//if-else

}//startActionThread

bool BACIComponent::startAllThreads()
{
  ACS_TRACE("baci::BACIComponent::startAllThreads");

  if (!threadManager_mp)
    return false;

  // action thread
  if (actionThread_mp == BACIThread::NullBACIThread)
      {
      actionThread_mp=threadManager_mp->create(name_m+"::actionThread",
					       (void*)actionThreadWorker, (void*)this,
					       getRTResponseTime(), getRTSleepTime(),
					       THR_NEW_LWP | THR_DETACHED,
					       actionThreadStackSize_m);
      }
  if (actionThread_mp == BACIThread::NullBACIThread)
    {
      return false;
    }
  else
    {
      actionThread_mp->resume();
    }

  // monitor thread
  if (monitorThread_mp == BACIThread::NullBACIThread)
      {
      monitorThread_mp=threadManager_mp->create(name_m+"::monitorThread",
						(void*)monitorThreadWorker, (void*)this,
						getMTResponseTime(), getMTSleepTime(),
						THR_NEW_LWP | THR_DETACHED,
						monitoringThreadStackSize_m);
      }
  if (monitorThread_mp == BACIThread::NullBACIThread)
    {
      delete actionThread_mp;
      actionThread_mp = BACIThread::NullBACIThread;
      return false;
    }
  else
    {
      monitorThread_mp->resume();
    }

  return true;
}//startAllThreads

void BACIComponent::stopMonitoringThread()
{
    ACS_TRACE("baci::BACIComponent::stopMonitoringThread");
    if (monitorThread_mp!=NULL)
	{
	monitorThread_mp->suspend();
	}
}//stopMonitoringThread

void BACIComponent::stopActionThread()
{
    ACS_TRACE("baci::BACIComponent::stopActionThread");
    if (actionThread_mp!=NULL)
	{
	actionThread_mp->suspend();
	}
}//stopActionThread


void BACIComponent::cancelMonitoringThread()
{
	ACS_TRACE("baci::BACIComponent::cancelMonitoringThread");
	if (monitorThread_mp!=NULL)
	{
		monitorThread_mp->cancel();
	}
}//cancelMonitoringThread

void BACIComponent::cancelActionThread()
{
	ACS_TRACE("baci::BACIComponent::cancelActionThread");
	if (actionThread_mp!=NULL)
	{
		actionThread_mp->cancel();
	}
}//cancelActionThread


void BACIComponent::stopAllThreads()
{
  ACS_TRACE("baci::BACIComponent::stopAllThreads");
  // set destruction flag
  inDestructionState_m = true;
  threadManager_mp->terminateAll();
}//stopAllThreads

bool BACIComponent::isMonitoringActive()
{
    if (monitorThread_mp!=NULL)
	{
	return !(monitorThread_mp->isSuspended());
	}
    else
	{
	return false;
	}
}//isMonitoringActive

bool BACIComponent::isActionThreadActive()
{
    if (actionThread_mp!=NULL)
	{
	return !(actionThread_mp->isSuspended());
	}
    else
	{
	return false;
	}
}//isActionThreadActive

void BACIComponent::setRTResponseTime(const ACS::TimeInterval& _actionThreadResponseTime)
{
  ACS_TRACE("baci::BACIComponent::setRTResponseTime");
  actionThreadResponseTime_m=_actionThreadResponseTime;
  if (actionThread_mp!=BACIThread::NullBACIThread)
      {
      actionThread_mp->setResponseTime(actionThreadResponseTime_m);
      }
}

void BACIComponent::setRTSleepTime(const ACS::TimeInterval& _actionThreadSleepTime)
{
  ACS_TRACE("baci::BACIComponent::setRTSleepTime");
  actionThreadSleepTime_m=(_actionThreadSleepTime<minRTSleepTime_m) ? (minRTSleepTime_m):(_actionThreadSleepTime);
  if (actionThread_mp!=BACIThread::NullBACIThread)
      {
      actionThread_mp->setSleepTime(actionThreadSleepTime_m);
      }
}

void BACIComponent::setMTResponseTime(const ACS::TimeInterval& _monitorThreadResponseTime)
{
  ACS_TRACE("baci::BACIComponent::setMTResponseTime");
  monitorThreadResponseTime_m=_monitorThreadResponseTime;
  if (monitorThread_mp!=BACIThread::NullBACIThread)
      {
      monitorThread_mp->setResponseTime(monitorThreadResponseTime_m);
      }
}

void BACIComponent::setMTSleepTime(const ACS::TimeInterval& _monitorThreadSleepTime)
{
  ACS_TRACE("baci::BACIComponent::setMTSleepTime");
  monitorThreadSleepTime_m=(_monitorThreadSleepTime<minMTSleepTime_m) ? (minMTSleepTime_m):(_monitorThreadSleepTime);
  if (monitorThread_mp!=BACIThread::NullBACIThread)
      {
      monitorThread_mp->setSleepTime(monitorThreadSleepTime_m);
      }
}


int BACIComponent::registerCallback(const BACIValue::Type type,
				    Callback_ptr callback_p,
				    const CBDescIn descIn)
{
  ACS_TRACE("baci::BACIComponent::registerCallback");
  ThreadSyncGuard guard(&callbackTableMutex_m);

  int callbackID = callbackTable_m.allocate();
  if (callbackID == 0)
    {
      // error
      return 0;
    }

  BACICallback* cb_p = new BACICallback(callbackID, callback_p, type, descIn, this);
  if (cb_p==0)
    {
      removeCallback(callbackID);
      return 0;
    }

  callbackTable_m[callbackID] = cb_p;
  return callbackID;
}

int BACIComponent::registerAction(const BACIValue::Type type,
				  Callback_ptr callback_p,
				  const CBDescIn descIn,
				  ActionImplementator* actionImplementator_p,
				  int actionFunction)
{
  ACS_TRACE("baci::BACIComponent::registerAction");

  int callbackID = registerCallback(type, callback_p, descIn);
  if (callbackID==0)
      {
      return 0;
      }

  BACIAction* action_p = new BACIAction(actionImplementator_p, actionFunction, callbackID);
  if (action_p==0)
    {
      removeCallback(callbackID);
      return 0;
    }

  pushAction(action_p);

  return callbackID;
}

int BACIComponent::registerAction(const BACIValue::Type type,
				  Callback_ptr callback_p,
				  const CBDescIn descIn,
				  ActionImplementator* actionImplementator_p,
				  int actionFunction,
				  const BACIValue& value)
{
  ACS_TRACE("baci::BACIComponent::registerAction");

  int callbackID = registerCallback(type, callback_p, descIn);
  if (callbackID==0)
      {
      return 0;
      }

  BACIAction* action_p = new BACIAction(actionImplementator_p, actionFunction, callbackID, value);
  if (action_p==0)
    {
      removeCallback(callbackID);
      return 0;
    }
  pushAction(action_p);
  return callbackID;
}

BACICallback* BACIComponent::getCallback(int callbackID)
{
  // ACS_TRACE("baci::BACIComponent::getCallback");
  ThreadSyncGuard guard(&callbackTableMutex_m);
  if (callbackTable_m.isAllocated(callbackID)==true)
      {
      return callbackTable_m[callbackID];
      }
  else
      {
      return 0;
      }
}


void BACIComponent::removeCallback(int callbackID)
{
  ACS_TRACE("baci::BACIComponent::removeCallback");
  ThreadSyncGuard guard(&callbackTableMutex_m);
  if (callbackTable_m.isAllocated(callbackID)==true)
    {
      BACICallback* callback_p = callbackTable_m[callbackID];
      callbackTable_m.deallocate(callbackID);
      delete callback_p;
    }
}

void BACIComponent::pushAction(BACIAction* action)
{
  ACS_TRACE("baci::BACIComponent::pushAction");
  ThreadSyncGuard guard(&actionQueueMutex_m);
  actionQueue_m.push_back(action);
}


BACIAction* BACIComponent::popAction()
{
  ACS_TRACE("baci::BACIComponent::popAction");
  ThreadSyncGuard guard(&actionQueueMutex_m);
  BACIAction* action_p = *actionQueue_m.begin();
  actionQueue_m.pop_front();
  return action_p;
}


class IsActionEqual {
private:
  int callbackID_m;
public:
  IsActionEqual(int _callbackID) : callbackID_m(_callbackID) {}
  bool operator()(BACIAction* action_p) const {
    return (action_p->getCallbackID() == callbackID_m);
  }
};


void BACIComponent::removeAction(int callbackID)
{
  ACS_TRACE("baci::BACIComponent::removeAction");
  ThreadSyncGuard guard(&actionQueueMutex_m);

  BACIActionQueue::iterator i = find_if(actionQueue_m.begin(), actionQueue_m.end(),
					IsActionEqual(callbackID));
  if (i!=actionQueue_m.end())
    {
      BACIAction* action = (BACIAction*)(*i);
      actionQueue_m.erase(i);
      delete action;
    }
}

void BACIComponent::removeCallbackAndAction(int callbackID)
{
  ACS_TRACE("baci::BACIComponent::removeCallbackAndAction");

  removeAction(callbackID);
  removeCallback(callbackID);
}

BACIProperty* BACIComponent::getPropertyAt(int pos) const
{
  // ACS_TRACE("baci::BACIComponent::getPropertyAt");
  //!!!ThreadSyncGuard guard(&propertyVectorMutex_m);
  //if !exists return 0; else
  return propertyVector_m[pos];
}

void BACIComponent::addProperty(BACIProperty* property)
{
  ACS_TRACE("baci::BACIComponent::addProperty");
  ThreadSyncGuard guard(&propertyVectorMutex_m);
  propertyVector_m.push_back(property);
  ACS_TRACE("baci::BACIComponent::addProperty done");
}

void BACIComponent::removeProperty(BACIProperty* property)
{
  ACS_TRACE("baci::BACIComponent::removeProperty");
  ThreadSyncGuard guard(&propertyVectorMutex_m);
  BACIPropertyVector::iterator i = find(propertyVector_m.begin(), propertyVector_m.end(), property);
  if (i!=propertyVector_m.end())
      {
      propertyVector_m.erase(i);
      }
  ACS_TRACE("baci::BACIComponent::removeProperty done");
}

 }; 

/*___oOo___*/



