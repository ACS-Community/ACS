#ifndef _enumpropAlarm_H
#define _enumpropAlarm_H

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: enumpropAlarm.h,v 1.34 2006/09/01 02:20:55 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  2001/08/27  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciEvent.h>
#include <baciRecoverableObject.h>
#include <ACSErrTypeAlarmC.h>

namespace baci {

template <class T>
class MonitorenumpropEventDispatcher : public EventDispatcher
{
public:
  //  MonitordoubleEventDispatcher();
		       
  virtual ~MonitorenumpropEventDispatcher(){ destroyEvents(); }

  virtual void subscribe(EventStrategy * event){
    EventDispatcher::subscribe(event);
    if (!event->isSuspended()) resume();
  }

  virtual void unsubscribe(EventStrategy * event){
    EventDispatcher::unsubscribe(event);
    if (!event->isSuspended()) suspend();
  }

  virtual void dispatch(T value,
			const ACSErr::Completion & c,
			const ACS::CBDescOut & desc
			 ) 
			{
      //     ACE_OS::printf("...dispatch...\n");

    EventStrategyVector subscribers = EventStrategyVector(getSubscribers());    // copy
    for (EventStrategyVector::iterator iter = subscribers.begin();
	 iter != subscribers.end(); 
	 iter++)
      {
	//	ACE_OS::printf("dispatch %d \n", value);
      CORBA::Any tAny;
      tAny <<= value;
	BACIValue val((ACS::pattern)value, tAny);
	(*iter)->check(val, c, desc);
      }
  }

  virtual void suspend(){
    if (active_m==1)
      //monitor_mp->suspend();
    active_m--;
  }

  virtual void resume(){
    if (active_m==0)
      //monitor_mp->resume();
    active_m++;
  }
};//class

template <class T, class ROT, class AlarmT>
class AlarmenumpropEventStrategy : public EventStrategy
{
public:
  static const int maxFailureCount;
#ifdef MAKE_VXWORKS 
    AlarmenumpropEventStrategy();
#endif
  AlarmenumpropEventStrategy(ROT * property, EventDispatcher * eventDispatcher);
  
  AlarmenumpropEventStrategy(Callback_ptr callback,
			     const CBDescIn& descIn,
			     const TimeInterval& interval,
			     ROT * property,
			     EventDispatcher * eventDispatcher);
  
  virtual ~AlarmenumpropEventStrategy();

  bool failed();

  void succeeded();

  CORBA::Object_ptr getCORBAReference() const 
  { 
    return reference_mp;
  }

  virtual bool isSuspended()
  {
    return suspended_m;
  }

  virtual void check(BACIValue &value,
		     const ACSErr::Completion & c,
		     const ACS::CBDescOut & desc
		       
		     );

  /* ------------------- [ Recoverable interface ] --------------------*/

  virtual int getId(void);
  virtual const char* getName(void);
  virtual char* getObjectState(void);
  virtual void setObjectState(const char * state);

  /* --------------- [ Subscription interface ] --------------- */ 

  virtual void suspend ( ) 
    throw (CORBA::SystemException);
  
  virtual void resume ( ) 
    throw (CORBA::SystemException);
  
  virtual void destroy ( ) 
    throw (CORBA::SystemException);
  
private:
  /// Event name
  ACE_CString name_m;

  /// Suspended state
  bool suspended_m;

  /// Callback failure counter (if exceeded, event is destroyed)
  int failureCount_m;
  
  /// Callback reference to the callback 
  AlarmT *callback_mp;
  
  /// Callback descriptor
  CBDescIn m_descIn;

  /// Cheking interval
  TimeInterval interval_m;

  /// Property to be checked for events
  ROT * property_mp;
  
  /// Pointer to EventDispatcher object 
  EventDispatcher * eventDispatcher_mp;

  /// CORBA reference of this object (subscritpion)
  CORBA::Object_ptr reference_mp;

  /// last alarm state
  int alarmRaised_m;
};

#include "enumpropAlarm.i"
 }; 

#endif
















