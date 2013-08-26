#ifndef baciEvent_H
#define baciEvent_H

/*******************************************************************
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
*
* "@(#) $Id: baciEvent.h,v 1.100 2011/09/02 11:39:00 bjeram Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* msekoran  2001/03/11  modified
*/

/** 
 * @file 
 * Header file BACI Events.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>
#include <baci.h>
#include <baciS.h>
#include <vector>
#include <baciRecoverableObject.h>

#include <ace/Synch.h>

namespace baci {

// forward
class EventStrategy;

/**
 * Vector of EventStrategy objects
 */
// get rid of this stl!!!
typedef std::vector<EventStrategy*> EventStrategyVector;  

/**
 * Base class of EventDispatcher
 * EventDispatcher is ... !!!
 * This class provides skleleton for all further event implementations<br>
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */
class baci_EXPORT EventDispatcher
{

public:

  /**
   * Constrcutor
   */
  EventDispatcher();

  /**
   * Destrcutor
   * Destroys all registered events
   */
  virtual ~EventDispatcher();

  /**
   * Subscribe event
   * @param event event to be subscribed
   * @return 0 in success
   */
  int subscribe(EventStrategy * event);

  /**
   * Unsubscribe event
   * @param event event to be unsubscribed
   * @return 0 on success
   */
  int unsubscribe(EventStrategy * event);

  /**
   * Get vector of events
   * @return vector of events
   */
  virtual EventStrategyVector& getSubscribers();

  /**
   * Get subscriber ADT thread-sync mutex.
   */
  virtual ACE_Recursive_Thread_Mutex& getMutex();

  /**
   * Inform dispatcher that one subscreiber has suspended its monitoring
   */
  virtual void suspend() = 0;

  /**
   * Inform dispatcher that one subscreiber has resumed its monitoring
   */
  virtual void resume() = 0;

protected:

  /**
   * Destroy all registered events
   */
  virtual void destroyEvents();

  /**
   * Vector of all registered events
   */
  EventStrategyVector events_m;

  /**
   * Number of active (non-suspened) subscreibers
   */
  int active_m;

  /**
   * Thread-Sync mutex.
   */
  ACE_Recursive_Thread_Mutex mutex_m;

  /**
   * State of the object.
   */
  bool shutdown_m;
  
};

/**
 * Abstract event dispatch strategy
 * This class provides skleleton for all further event implementations<br>
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
class baci_EXPORT EventStrategy : public virtual PortableServer::RefCountServantBase,
		      public POA_ACS::Subscription,
		      public RecoverableObject
{

public:

  /**
   * Default constrcutor
   */
  EventStrategy() {}

  /**
   * Constructor
   * @param property property to be checked for events
   * @param eventDispatcher pointer to EventDispatcher object
   */
  EventStrategy(BACIProperty * property, EventDispatcher * eventDispatcher) {
    ACE_UNUSED_ARG(property);
    ACE_UNUSED_ARG(eventDispatcher);
  }
 
  /**
   * Destructor
   */
  virtual ~EventStrategy() {}

  /**
   * Event cheking method
   * This virtual method is called periodically to check 
   * property state if event should be raised
   */
  virtual void check(BACIValue &value,
		     const ACSErr::Completion & c,
		     const ACS::CBDescOut & desc) = 0;

  /**
   * Is suspended
   */
  virtual bool isSuspended() = 0;

  /* ------------------- [ Recoverable interface ] --------------------*/

  virtual int getId(void) = 0;
  virtual const char* getName(void) = 0;
  virtual const char* getObjectState(void) = 0;
  virtual void setObjectState(const char * state) = 0;

  /* ------------------ [ Subscription interface ] -------------------- */ 
  
  /**
   * Suspend event subscription
   */
  virtual void suspend () = 0;
  
  /**
   * Resume suspended event subscription
   */
  virtual void resume () = 0;
  
  /**
   * Destroy event subscription
   */
  virtual void destroy () = 0;
  
};

 }; 

#endif  /* baciEvent_H */





