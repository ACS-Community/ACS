#ifndef baciBACIMonitor_H
#define baciBACIMonitor_H

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
* "@(#) $Id: baciBACIMonitor.h,v 1.6 2011/03/30 17:57:23 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

/** 
 * @file 
 * Header file BACIMonitor.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciExport.h"
#include "acsutilTimeStamp.h"
#include <acscommonC.h>
#include "baciValue.h"
#include "logging.h"
#include <vector>

namespace baci {



// forwards
class BACIProperty;

/* ------------------------------------------------------------------------ */

/**
 * Abstract class which BACI monitor implementator (e.g. CORBA object) must implement
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */
 
class baci_EXPORT MonitorImplementator 
{
public:

  /**
   * Monitor status method
   * After contruction of a monitor, this method will be called
   * to check if construction was successful; if not, monitor will be destroyed by parent
   * @return 0 on success or any other value (value can indicate cause of faulire) on faulure
   */
  virtual int initialization() { return 0; }

  /**
   * Destructor method
   * This method is called when BACI monitor implementation is beeing destroyed
   * and notifies implementator also to destroy itself.
   */
  virtual void monitorDestroyed(void) = 0;

  /**
   * Monitor state changed motification method
   * This method is called when state of BACI monitor state is changed
   */
  virtual void monitorStateChanged(void) = 0;

  virtual ~MonitorImplementator() {}

};  /* MonitorImplementator */

/* ------------------------------------------------------------------------ */

/**
 * Class represeting BACI Monitor
 * It provides monitoring capability triggered on time or value change,
 * clients are notified using callbacks
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */

class baci_EXPORT BACIMonitor
{

public:

  /**
   * Monitor update mode
   * To have synchorinzed monitors mumLast (default) mode has to be used.
   * BACI Monitors support 2 modes: mumLast, mumTrunc
   * Example: <pre>
   *
   *  monitorThreadSleepTime=100
   *  monitor1TriggerTime=30
   *  monitor2TriggerTime=140
   *  pollInterval=10
   *  
   *  Time\Mode        mumLast                      mumTrunc
   *  
   *  0                M1 (0), M2 (0)               M1 (0), M2 (0)
   *  100              M1 (90)                      M1 (0+90)
   *  200              M1 (180), M2 (140)           M1 (100+90), M2 (0+140)
   *  300              M1 (300), M2 (280)           M1 (200+90)
   *  400              M1 (390)                     M1 (300+90), M2 (200+140)
   *  500              M1 (480), M2 (420)           M1 (400+90)
   *  600              M1 (600), M2 (560)           M1 (500+90), M2 (400+140)
   *  700              M1 (690)                     M1 (600+90)
   * </pre>
   */
  enum UpdateMode { mumNull=0, 
		    mumLast=1, 
		    mumTrunc=2 };
  
  /**
   * Constructor
   */
  BACIMonitor(const ACE_CString& _name, int _callbackID,
	      MonitorImplementator* monitorImplementator_p,
	      const ACS::TimeInterval& _triggerTime, const BACIValue& _triggerValue, 
	      const ACS::TimeInterval& _minTriggerTime, const BACIValue& _minTriggerValue, 
	      BACIProperty* _property, 
	      const ACS::TimeInterval& _transmitTime=0,
	      const UpdateMode& _updateMode=mumLast,
	      const bool _achivingMonitor=false,
	      const bool _suspended=false,
              const bool _deltaValueAndTimerInteraction=false,   // delta trigger reset timer and interval is greather than minTriggerTime
              const unsigned int _priority=3);        // archiver default priority

  /// Destroys monitor. Proxy for ~BACIMonitor.
  void destroy();
  
  void suspend();
  void resume();
  void setLastTime(const ACS::TimeInterval& _lastTime);
  void setTriggerValue(const BACIValue& _triggerValue);
  void setTriggerValuePercent(const BACIValue& _triggerValuePercent);
  void setMinTriggerValue(const BACIValue& _minTriggerValue);
  void setLastValue(const BACIValue& _lastValue);
  void setTriggerOnValue(bool enable);
  void setTriggerOnValuePercent(bool enable);

  /* ---- */

  const char * getName() const { return name_m.c_str(); }
  bool isSuspended() const { return suspended_m; }
 
  BACIMonitor& operator=(const BACIMonitor& mon) 
  {
    if (this!=&mon)
      {
	name_m=mon.name_m; 
	callbackID_m=mon.callbackID_m; 
	triggerTime_m=mon.triggerTime_m; 
	transmitTime_m=mon.transmitTime_m;
	lastTime_m=mon.lastTime_m; 
	triggerValue_m=mon.triggerValue_m; 
	triggerValuePercent_m=mon.triggerValuePercent_m; 
	lastValue_m=mon.lastValue_m; 
	triggerOnValue_m=mon.triggerOnValue_m;
	triggerOnValuePercent_m=mon.triggerOnValuePercent_m;
	property_mp=mon.property_mp;
	suspended_m=mon.suspended_m; 
	archivingMonitor_m=mon.archivingMonitor_m; 
	updateMode_m=mon.updateMode_m;

	monitorImplementator_mp = mon.monitorImplementator_mp;
	minTriggerTime_m = mon.minTriggerTime_m;
	userControlledTransmitTime_m = mon.userControlledTransmitTime_m;
	minTriggerValue_m = mon.minTriggerValue_m;
	deltaValueAndTimerInteraction_m = mon.deltaValueAndTimerInteraction_m;
	priority_m = mon.priority_m;
	inDestructionState_m = mon.inDestructionState_m;
	destroyed_m = mon.destroyed_m;
      } 
    return *this;
  }
  
  bool operator==(const BACIMonitor& mon) const { return callbackID_m==mon.callbackID_m; }

  UpdateMode getUpdateMode() const { return updateMode_m; }
  int getCallbackID() const { return callbackID_m; }
  ACS::TimeInterval getTriggerTime() const { return triggerTime_m; }
  ACS::TimeInterval getMinTriggerTime() const { return minTriggerTime_m; }
  ACS::TimeInterval getLastTime() const { return lastTime_m; }
  ACS::TimeInterval getTransmitTime() const { return transmitTime_m; }
  BACIValue getTriggerValue() const { return triggerValue_m; }
  BACIValue getTriggerValuePercent() const { return triggerValuePercent_m; }
  BACIValue getMinTriggerValue() const { return minTriggerValue_m; }
  BACIValue getLastValue() const { return lastValue_m; }
  bool getTriggerOnValue() const { return triggerOnValue_m; }
  bool getTriggerOnValuePercent() const { return triggerOnValuePercent_m; }

  void setUpdateMode(const UpdateMode& _updateMode);
  void setTriggerTime(const ACS::TimeInterval& _triggerTime);
  void setMinTriggerTime(const ACS::TimeInterval& _minTriggerTime);

  void setTransmitTime(const ACS::TimeInterval& _transmitTime)
  {
    transmitTime_m = _transmitTime;
  }

  /**
   * Notify monitor implementator about monitor state change
   */
    void monitorStateChanged() 
	{
	    ACS_TRACE("baci::BACIMonitor::monitorStateChanged");
	    if (monitorImplementator_mp!=0)
		{
		monitorImplementator_mp->monitorStateChanged();
		}
	}

  MonitorImplementator* getMonitorImplementator() const { return monitorImplementator_mp; }
  BACIProperty* getProperty() const { return property_mp; }

  bool isInDestructionState() const { return inDestructionState_m; };

  bool isArchivingMonitor() const { return archivingMonitor_m; };

  bool isDeltaValueAndTimerInteraction() const { return deltaValueAndTimerInteraction_m; };

  unsigned int getPriority() const { return priority_m; };

private:

  /**
   * Destructor
   * Do not call it directly, call internalDestroy() method instead.
   * @see destroy
   */
  ~BACIMonitor();

  void internalDestroy();


  ACE_CString name_m;
  int callbackID_m;
  
  MonitorImplementator* monitorImplementator_mp;

  UpdateMode updateMode_m;
  
    ACS::TimeInterval triggerTime_m;
    ACS::TimeInterval minTriggerTime_m;
  ACS::TimeInterval transmitTime_m;
  bool userControlledTransmitTime_m;
  ACS::TimeInterval lastTime_m;
    
    BACIValue triggerValue_m;
    BACIValue triggerValuePercent_m;
    BACIValue minTriggerValue_m; 
    BACIValue lastValue_m;
  bool triggerOnValue_m;
  bool triggerOnValuePercent_m;
  
  BACIProperty* property_mp;
  
  bool archivingMonitor_m;

  bool suspended_m;

  bool deltaValueAndTimerInteraction_m;

  unsigned int priority_m;

  bool inDestructionState_m;

  bool destroyed_m;

    friend class BACIProperty;

};   /* BACIMonitor */

/**
 * Vector containing references to BACIMonitor objects
 */
typedef std::vector<BACIMonitor*> BACIMonitorVector;
/* ------------------------------------------------------------------------ */

 }; 

#endif /* baci_H */ 


