#ifndef baciBACIComponent_H
#define baciBACIComponent_H

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
* "@(#) $Id: baciBACIComponent.h,v 1.17 2011/02/17 18:25:39 rtobar Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2001-07-12 changed name of parameter in setCompletion
* msekoran  2001/03/04 modified
*/

/** 
 * @file 
 * Header file BACIComponent.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciExport.h"
#include "baciThread.h"
#include "baciValue.h"
#include "logging.h"

#include "baciBACIAction.h"
#include "baciBACICallback.h"
#include "baciBACIMonitor.h"
#include "baciBACIProperty.h"
#include <acsThreadManager.h>
#include "baciCharacteristicModelImpl.h"
#include <baciErrTypeProperty.h>
#include <ACSErrTypeCommon.h>

namespace baci
{


/* ------------------------------------------------------------------------ */



/**
 * Class represeting BACI Component
 * Component is responsible for action, monitor dispatching
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */

class baci_EXPORT BACIComponent
{

public:

    /**
     * Component constructor that allows to pass thread manager
     * This class is a property containder providing action and monitor queues.
     * @param threadManager ACS Thread Manager
     * @param name name of the Component
     * @param characteristicModel characteristic model to be used to retrieve data
     * @param actionThreadResponseTime response time of the action thread in 100ns unit
     * @param actionThreadSleepTime sleep time of the action thread in 100ns unit
     * @param monitorThreadResponseTime response time of the monitor thread in 100ns unit
     * @param monitorThreadSleepTime sleep time of the monitor thread in 100ns unit
     */
    BACIComponent( ACS::ThreadManager *thrMgr,
		   const ACE_CString& name,
		   const ACE_CString& type,
		   CharacteristicModelImpl *characteristicModel,
		   size_t actionThreadStackSize,
		   size_t monitorThreadStackSize,
		   const ACS::TimeInterval& actionThreadResponseTime=defaultRTResponseTime_m, 
		   const ACS::TimeInterval& actionThreadSleepTime=minRTSleepTime_m,
		   const ACS::TimeInterval& monitorThreadResponseTime=defaultMTResponseTime_m,
		   const ACS::TimeInterval& monitorThreadSleepTime=minMTSleepTime_m);

  ~BACIComponent();

//  ACE_CString getName() const { return name; }
  const char * getName() const { return name_m.c_str(); }

  const char * getType() const { return type_m.c_str(); }

  /**
   * Register callback to BACI
   * @param type callback type (e.g. type double means callback is type of CBdouble)
   * @param callback callback reference
   * @param descIn callback descriptor (passed by client)
   * @return callback ID (0 on failure)
   */
  int registerCallback(const BACIValue::Type type,
		       Callback_ptr callback_p, 
		       const CBDescIn descIn);

  /**
   * Register action to BACI
   * @param type callback type (e.g. type double means callback is type of CBdouble)
   * @param callback callback reference
   * @param descIn callback descriptor (passed by client)
   * @param actionImplemenator action implementator
   * @param actionFunction action function to be invoked in execute action
   * @param value action data (e.g. value to be set)
   * @return callback ID (0 on failure)
   */
   int registerAction(const BACIValue::Type type,
		     Callback_ptr callback_p, 
		     const CBDescIn descIn,
		     ActionImplementator* actionImplementator_,
		     int actionFunction_);

  /**
   * Register action to BACI
   * @param type callback type (e.g. type double means callback is type of CBdouble)
   * @param callback callback reference
   * @param descIn callback descriptor (passed by client)
   * @param actionImplemenator action implementator
   * @param actionFunction_ action function to be invoked in execute action
   * @return callback ID (0 on failure)
   */
  int registerAction(const BACIValue::Type type,
		     Callback_ptr callback_p, 
		     const CBDescIn descIn,
		     ActionImplementator* actionImplementator_,
		     int actionFunction,
		     const BACIValue& value);

  BACIThreadManager* getThreadManager() const { return threadManager_mp; }

  ACS::TimeInterval getMTResponseTime() const { return monitorThreadResponseTime_m; }
  void setMTResponseTime(const ACS::TimeInterval& _monitorThreadResponseTime);

  ACS::TimeInterval getMTSleepTime() const { return monitorThreadSleepTime_m; }
  void setMTSleepTime(const ACS::TimeInterval& _monitorThreadSleepTime);
  
  ACS::TimeInterval getRTResponseTime() const { return actionThreadResponseTime_m; }
  void setRTResponseTime(const ACS::TimeInterval& _actionThreadResponseTime);

  ACS::TimeInterval getRTSleepTime() const { return actionThreadSleepTime_m; }
  void setRTSleepTime(const ACS::TimeInterval& _actionThreadSleepTime);

    bool dispatchCallback(int callbackID, 
			  const BACIValue& value, 
			  CBDescOut& descOut,
			  const Completion& completion,
			const BACIMonitor * archiver = 0);
    bool finishCallback(int callbackID, 
			const BACIValue& value, 
		      CBDescOut& descOut, const Completion& completion);


  bool isInDestructionState() const { return inDestructionState_m; };

  
  int getActionCount() const { return actionQueue_m.size(); }
  void pushAction(BACIAction* action);
  BACIAction* popAction();

  void removeCallbackAndAction(int callbackID);
    BACICallback* getCallback(int callbackID);
    void removeCallback(int callbackID);
    
  int getPropertyCount() const { return propertyVector_m.size(); }
  BACIProperty* getPropertyAt(int pos) const;

  void stopAllThreads();
  bool startAllThreads();

  /**
   *  Creates and starts the Monitoring thread.
   *  In case of an error it throws an exception:
   *  @throw ACSErrTypeCommon::NullPointerExImpl if there is no thread manager
   *  @throw acsthreadErrType::CanNotCreateThreadExImpl if the thread can not be created
   */ 
  void startMonitoringThread();

  /**
   * Creates and starts the Action thread. 
   * The thread which is used for executing asyhronous commmand.
   *  In case of an error it throws an exception:
   *  @throw ACSErrTypeCommon::NullPointerExImpl if there is no thread manager
   *  @throw acsthreadErrType::CanNotCreateThreadExImpl if the thread can not be created
   */ 
   void startActionThread();

  /**
   * Stops (suspends) the Monitoring thread
   * If the thread is already suspended, or if it has not been created yet, it just returns.
   */
   void stopMonitoringThread();

    /**
     * Stops (suspends) the Action thread. 
     * If the thread is already suspended, or if it has not been created yet, it just returns.
     */
    void stopActionThread();

    /**
       * Cancels  the Monitoring thread if one has been created.
       * We need to cancel the thread to avoid waiting unneccessary waiting in sleep of the thread,
       * because cancel
    */
    void cancelMonitoringThread();

    /**
       * Cancels the Action thread if one has been created.
       * We need to cancel the thread to avoid waiting unneccessary waiting in sleep of the thread,
       * because cancel
    */
    void cancelActionThread();


    /**
     * Returns true if monitoring is active, i.e. 
     * if monitoring thread is created and resumed.
     */
    bool isMonitoringActive();

    /**
     * Returns true if Action Thread is active, i.e. 
     * if the action thread is created and resumed.
     */
    bool isActionThreadActive();

    CharacteristicModelImpl* getCharacteristicModel() const { return characteristicModel_mp; };

protected:

  static const ACS::TimeInterval defaultRTResponseTime_m;			// RT = Action Thread
  static const ACS::TimeInterval minRTSleepTime_m;
  
  static const ACS::TimeInterval defaultMTResponseTime_m;			// MT = Monitor Thread
  static const ACS::TimeInterval minMTSleepTime_m;


  BACIThread* getActionThread() const { return actionThread_mp; }
  BACIThread* getMonitorThread() const { return monitorThread_mp; }

  int getThreadCount() const { return threadManager_mp->getThreadCount(); }

    void removeAction(int callbackID);

  void addProperty(BACIProperty* property);
  void removeProperty(BACIProperty* property);

private:

  ACE_CString name_m;
  ACE_CString type_m;
  CharacteristicModelImpl* characteristicModel_mp;

  BACICallbackTable callbackTable_m;
  BACIActionQueue actionQueue_m;
  BACIPropertyVector propertyVector_m;

    ACS::TimeInterval actionThreadResponseTime_m;
    ACS::TimeInterval actionThreadSleepTime_m;
    ACS::TimeInterval monitorThreadResponseTime_m;
    ACS::TimeInterval monitorThreadSleepTime_m;

  BACIThread* actionThread_mp;
  BACIThread* monitorThread_mp;
  BACIThreadManager* threadManager_mp;

  bool inDestructionState_m;

  BACIMutex actionQueueMutex_m;
  BACIMutex propertyVectorMutex_m;
  BACIMutex callbackTableMutex_m;

  size_t actionThreadStackSize_m;
  size_t monitoringThreadStackSize_m;

  friend class BACIProperty;

    /**
     * ALMA C++ coding standards state assignment operators should be disabled.
     */
    void operator=(const BACIComponent&);
    
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    BACIComponent(const BACIComponent&);

};   /* BACIComponent */

/* ------------------------------------------------------------------------ */

}//namespace baci


#endif /* baci_H */ 


