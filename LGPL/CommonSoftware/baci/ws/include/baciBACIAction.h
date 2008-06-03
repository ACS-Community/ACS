#ifndef baciBACIAction_H
#define baciBACIAction_H

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
* "@(#) $Id: baciBACIAction.h,v 1.7 2008/06/03 09:14:47 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bgustafs 2001-07-12 changed name of parameter in setCompletion
* msekoran  2001/03/04 modified
*/

/** 
 * @file 
 * Header file BACIAction.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciExport.h"
#include "baciValue.h"

#include <deque>

namespace baci {


// forwards
class BACIComponent;

/**
 * Expected BACI Action Function return value
 */
enum ActionRequest { reqNone=0, 
				 reqInvokeWorking=1, 
				 reqInvokeDone=2, 
				 reqDestroy=3 };

/**
 * Abstract class which every Action caller must implement
 * Direct call is not possbile because method is a member of a class
 * (pointer to member function is actually a class offset)
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */
class baci_EXPORT ActionImplementator 
{
public:
  /**
   * Action dispatcher function
   * @param function action funtion to be invoked<br>
   * Decleraton example of function: <pre>
   *    ActionRequest someAction(BACIComponent* component_p, int callbackID, 
   *			         const CBDescIn& descIn, BACIValue* value, 
   *			         Completion& completion, CBDescOut& descOut);
   * </pre>
   * @param component_p owner of the action
   * @param callbackID id of the callback to be notified
   * @param descIn callback descriptor (passed by client)
   * @param value action data (e.g. value to be set)
   * @param error error handing structure
   * @param descOut callback descriptor which will be passed to client
   * @return request to be performed by BACI
   * <ul>
   *  <li><b><i>reqNone</b></i> - do nothing (action will be kept in queue)
   *  <li><b><i>reqInvokeWorking</b></i> - invoke <type>Callback::<i>working</i>
   *  <li><b><i>reqInvokeDone</b></i> - invoke <type>Callback::<i>done</i> and destroy callback
   *  <li><b><i>reqDestroy</b></i> - destroy callback (callback should has been called already by function)
   * </ul>
   */
  virtual ActionRequest invokeAction(int function,
				     BACIComponent* component_p, 
				     const int &callbackID, 
				     const CBDescIn& descIn, 
				     BACIValue* value, 
				     Completion& completion, 
				     CBDescOut& descOut) = 0;

  virtual ~ActionImplementator() {}

};   /* ActionImplementator */

/* ------------------------------------------------------------------------ */

/**
 * Class representing BACI Action
 * Since all BACI asynchronious class has to keep all needed
 * data to invoke action in the future...
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */

class baci_EXPORT BACIAction 
{

public:

  /**
   * Null contructor
   */
  BACIAction() : actionFunction_m(0), actionImplementator_mp(0),
    callbackID_m(-1), value_m(BACIValue::NullValue), completed_m(false) {}

  /**
   * Null value constructor
   * @param actionFunction_ action function to be invoked in execute action
   * @param callbackID_ id of the callback to be invoked
   */
  BACIAction(ActionImplementator* actionImplementator_,
	     int actionFunction_, 
	     int callbackID_) :
    actionFunction_m(actionFunction_),
    actionImplementator_mp(actionImplementator_),
    callbackID_m(callbackID_), 
    value_m(BACIValue(BACIValue::NullValue)),
    completed_m(false)
  {
  }

  /**
   * Constructor
   * @param actionImplemenator_ action implementator
   * @param actionFunction_ action function to be invoked in execute action
   * @param callbackID_ id of the callback to be invoked
   * @param value_ action data (e.g. value to be set)
   */
  BACIAction(ActionImplementator* actionImplementator_,
	     int actionFunction_, 
	     int callbackID_, const BACIValue& value_): 
    actionFunction_m(actionFunction_), 
    actionImplementator_mp(actionImplementator_),
    callbackID_m(callbackID_), 
    value_m(value_),
    completed_m(false)
  {
  }
  
  /**
   * Assignment operator
   */
  BACIAction& operator=(const BACIAction& action)
  {
    if (this!=&action) 
      { 
	actionFunction_m=action.actionFunction_m;
	actionImplementator_mp=action.actionImplementator_mp;
	callbackID_m=action.callbackID_m;
	value_m=action.value_m;
        completed_m=action.completed_m;
        completion_m=action.completion_m;
      } 
    return *this;
  }
  
  /**
   * Compare (equals) operator
   */
  bool operator==(const BACIAction& action) const { return callbackID_m==action.callbackID_m; }

  /**
   * Invoke action
   * @param component_p owner of the action
   * @param callbackID id of the callback to be notified
   * @param descIn callback descriptor (passed by client)
   * @param value action data (e.g. value to be set)
   * @param completion error handing structure
   * @param descOut callback descriptor which will be passed to client
   * @return request to be performed by BACI
   */
    ActionRequest invoke(BACIComponent* component_p, 
			 int callbackID_, 
			 const CBDescIn& descIn, 
			 BACIValue* value, 
			 Completion& completion, 
			 CBDescOut& descOut) 
  {
    return actionImplementator_mp->invokeAction(actionFunction_m, component_p, callbackID_, descIn, 
					     value, completion, descOut);
  }  

  /**
   * Get BACI Action Function
   * @return action function
   */
  int getActionFunction() const { return actionFunction_m; }

  /**
   * Get BACI Action Implementator
   * @return action implementator
   */
  ActionImplementator* getActionImplementator() const { return actionImplementator_mp; }

  /**
   * Get callback ID
   * @return callback ID
   */
  int getCallbackID() const { return callbackID_m; };

  /**
   * Get action data
   * @return action data (e.g. value to be set)
   */
  BACIValue getValue() const { return value_m; }

  /**
   * Get reference to action data
   * @return action data (e.g. value to be set)
   */
  BACIValue* getValueRef() const { return const_cast<BACIValue*>(&value_m); }

  /**
   * Get completion state
   * @return completion state
   */
  bool isCompleted() const { return completed_m; }

  /**
   * Get completion data
   * @return completion data
   */
  Completion getCompletion() const { return completion_m; }

  /**
   * Set completion state to true
   */
  void setCompletion(const Completion& c)
  { 
    completed_m = true;
    completion_m = c;
  }
	  
private:

  /**
   * BACI Action Function
   */
  int actionFunction_m;		

  /**
   * BACI Action Implementator
   */
  ActionImplementator* actionImplementator_mp;

  /**
   * Callback ID
   */
  int callbackID_m;

  /**
   * Action data (e.g. value to be set)
   */
  BACIValue value_m;

  /**
   * Completion state
   */
  bool completed_m;

  /**
   * Completion data
   */
  Completion completion_m;
 
    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    BACIAction(const BACIAction&);

};   /* BACIAction */

/**
 * Action queue
 */
typedef std::deque<BACIAction*> BACIActionQueue;

/* ------------------------------------------------------------------------ */

 }; 

#endif /* baci_H */ 
