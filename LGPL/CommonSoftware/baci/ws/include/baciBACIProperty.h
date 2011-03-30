#ifndef baciBACIProperty_H
#define baciBACIProperty_H

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
* "@(#) $Id: baciBACIProperty.h,v 1.8 2011/03/30 17:57:23 tstaig Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

/** 
 * @file 
 * Header file BACIProperty.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciExport.h"
#include "baciThread.h"
#include "baciValue.h"
#include "logging.h"
#include "baciBACIMonitor.h"
#include "baciCharacteristicModelImpl.h"

#include <vector>

namespace baci {


class BACIComponent;
class BACIProperty;

/* ------------------------------------------------------------------------ */

/**
 * Abstract class which BACI property implementator (e.g. CORBA object) must implement
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */
 
class baci_EXPORT PropertyImplementator 
{
public:

  /**
   * Get value method (value accessor)
   * @param property property which requested value
   * @param value value to be returned
   * @param completion error handling structure
   * @param descOut callback descriptor
   */
  virtual void getValue(BACIProperty* property,
			BACIValue* value, 
			Completion &completion,
			CBDescOut& descOut) = 0;

  /**
   * Property construction status method
   * After contruction of a property, this method will be called
   * to check if construction was successful; if not, monitor will be destroyed by parent
   * @return 0 on success or any other value (value can indicate cause of faulire) on faulure
   */
  virtual int initialization() { return 0; }

  /// Destructor
  virtual ~PropertyImplementator() {}

};  /* PropertyImplementator */

/* ------------------------------------------------------------------------ */



/**
 * Class represeting BACI Property (Paramenter)
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */

class baci_EXPORT BACIProperty
{

public:

  /**
   * Constructor
   * @param name_ property name
   * @param propertyImplementator_ property implementator refernce
   * @param characteristicModel characteristic model to be used to retrieve data
   * @param defaultValue_ property default value (identifies type of this property - value MUST NOT BE NullValue!!!)
   * @param component_p property owner
   */
  BACIProperty(const ACE_CString& name_,
	       PropertyImplementator* propertyImplementator_,
	       CharacteristicModelImpl *characteristicModel_,
	       const BACIValue& defaultValue_,
	       BACIComponent* component_p);

  /**
   * Destructor
   */
  ~BACIProperty();

  /* ---- */

//  ACE_CString getName() const { return name; }
  const char * getName() const { return name_m.c_str(); }

  BACIValue::Type getType() const { return type_m; }

  PropertyImplementator* getPropertyImplementator() const { return propertyImplementator_mp; }

  CharacteristicModelImpl* getCharacteristicModel() const { return characteristicModel_mp; };

  /**
   * Get value method (value accessor)
   * @param property property which requested value
   * @param value value to be returned
   * @param completion error handling structure
   * @param descOut callback descriptor
   */
  void getValue(BACIProperty* property,
		BACIValue* value, 
		Completion &completion,
		CBDescOut& descOut)
  {
    if (propertyImplementator_mp!=0)
      propertyImplementator_mp->getValue(property,
				      value,
				      completion,
				      descOut);
  }

  BACIComponent* getComponent() const { return component_mp; }

  int getMonitorCount() const { return monitorVector_m.size(); }
  BACIMonitor* getMonitorAt(int pos) const { return monitorVector_m[pos]; }
  
  bool hasTriggerOnValueMonitor() const { return triggerOnValueMonitor_m; }
  bool hasTriggerOnValuePercentMonitor() const { return triggerOnValuePercentMonitor_m; }

  ACS::TimeInterval getPollInterval() const { return pollInterval_m; }
  ACS::TimeInterval getLastPollTime() const { return lastPollTime_m; }
  ACS::TimeInterval getMonMinTriggerTime() const { return monMinTriggerTime_m; }
  BACIValue getLastValue() const { return lastValue_m; }
  //Completion getLastCompletion() const { return lastCompletion; }

  void setPollInterval(const ACS::TimeInterval& _pollInterval) { pollInterval_m=_pollInterval; }
  void setLastPollTime(const ACS::TimeInterval& _lastPollTime) { lastPollTime_m=_lastPollTime; }
  void setMonMinTriggerTime(const ACS::TimeInterval& _interval) { monMinTriggerTime_m=_interval; }
  void setLastValue(const BACIValue& _lastValue) {lastValue_m=_lastValue;}
  //void setLastCompletion(const Completion& _lastCompletion) { lastCompletion=_lastCompletion;}

  void dispatchMonitors(Completion& completion, CBDescOut& descOut);
  void updateMonitorStates();

  bool isInDestructionState() const { return inDestructionState_m; };

protected:

  void addMonitor(BACIMonitor* monitor);
  void removeMonitor(BACIMonitor* monitor);

  ACS::TimeInterval GCD(ACS::TimeInterval t1, ACS::TimeInterval t2);


private:
  ACE_CString name_m;

  PropertyImplementator* propertyImplementator_mp;
  CharacteristicModelImpl* characteristicModel_mp;

  BACIValue lastValue_m;

  BACIComponent* component_mp;

  BACIMonitorVector monitorVector_m;

  bool triggerOnValueMonitor_m;
  bool triggerOnValuePercentMonitor_m;
    ACS::TimeInterval pollInterval_m;
    ACS::TimeInterval lastPollTime_m;
    ACS::TimeInterval monMinTriggerTime_m;
  // Completion lastCompletion;

  BACIMonitor* archiver_mp;

  BACIValue::Type type_m;

  bool inDestructionState_m;

  BACIMutex monitorVectorMutex_m;

  friend class BACIComponent;
  friend class BACIMonitor;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const BACIProperty&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    BACIProperty(const BACIProperty&);

};   /* BACIProperty */

/**
 * Property vector
 */
typedef std::vector<BACIProperty*> BACIPropertyVector;


/* ------------------------------------------------------------------------ */

 }; 

#endif /* baci_H */ 


