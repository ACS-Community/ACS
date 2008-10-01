#ifndef _enumpropAlarm_H
#define _enumpropAlarm_H

/*******************************************************************
* E.S.O. - ACS project
*
* "@(#) $Id: enumpropAlarm.h,v 1.37 2008/10/01 02:33:31 cparedes Exp $"
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
#include <acscommonC.h>

NAMESPACE_BEGIN(baci);

class MonitorenumpropEventDispatcher : 
    public MonitorEventDispatcher<ACS::pattern, ACS::CBpattern, POA_ACS::CBpattern>
{
public:
    MonitorenumpropEventDispatcher(const CBDescIn& descIn,
			   const ACS::TimeInterval& interval,
				   BACIProperty * property) :
	MonitorEventDispatcher<ACS::pattern, ACS::CBpattern, POA_ACS::CBpattern>(descIn, interval, property)
	{
	}

};//class MonitorenumpropEventDispatcher

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
			     const ACS::TimeInterval& interval,
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

  virtual void suspend ( ); 
  
  virtual void resume ( ) ;
  
  virtual void destroy ( ) ;
  
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
  ACS::TimeInterval interval_m;

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
NAMESPACE_END(baci);

#endif
















