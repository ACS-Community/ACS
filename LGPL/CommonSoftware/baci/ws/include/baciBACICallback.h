#ifndef baciBACICallback_H
#define baciBACICallback_H

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
* "@(#) $Id: baciBACICallback.h,v 1.5 2006/09/01 02:20:54 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* dfugate 2005-01-17 created
*/

/** 
 * @file 
 * Header file BACICallback.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "baciExport.h"
#include "baciValue.h"
#include "baciRegistrar.h"

namespace baci {


// forwards
class BACIComponent;

/**
 * BACI Callback wrapper
 * Callbacks are needed for actions and monitors
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 */

class baci_EXPORT BACICallback 
{

public:

  /**
   * Contructor
   * @param id_ id of the callback to be notified
   * @param callback_ callback reference
   * @param type_ callback type (e.g. type double means callback is type of CBdouble)
   * @param descIn_ callback descriptor (passed by client)
   * @param component_p parent object where callback is saved (owner of the callback)
   */
  BACICallback(const int& id_, Callback_ptr callback_, 
	       const BACIValue::Type type_, 
	       const CBDescIn& descIn_, BACIComponent* component_p): 
    id_m(id_), type_m(type_), descIn_m(descIn_), component_mp(component_p), 
    failureCount_m(0), removeOnFailure_m(true) 
  { 
    callback_mp = Callback::_duplicate(callback_); 
  }

  /**
   * Destructor
   */
  ~BACICallback() { 
    CORBA::release(callback_mp);
  }
  
  /**
   * Assignment operator
   */
  BACICallback& operator=(const BACICallback& cb) 
  {
    if (this!=&cb) 
      { 
	id_m=cb.id_m; 
	type_m=cb.type_m; 
	callback_mp=Callback::_duplicate(cb.callback_mp); 
	descIn_m=cb.descIn_m;
	component_mp=cb.component_mp;
        failureCount_m=cb.failureCount_m;
	removeOnFailure_m=cb.removeOnFailure_m;
      }
    return *this;
  }
  
  /**
   * Compare (equals) operator
   */
  bool operator==(const BACICallback& cb) const { return id_m==cb.id_m; }

  /**
   * Get callback ID
   * @return callback ID
   */
   int getID() const { return id_m; }
 
  /**
   * Report successful invokation
   * Resets failure counter to zero
   */
   void succeeded() { 
     //ACS_TRACE("baci::BACICallback::succeeded");
     failureCount_m=0;
   }

  
  /**
   * Report failed invokation
   * Increases failure counter and destroys callback if 
   * failure limit has been exceeded
   */
  void failed();

  /**
   * Get callback status (if invokations are successful)
   * @return true if OK, otherwise false
   */
  bool isOK() { return (failureCount_m <= failureLimitCount_m); }

  /**
   * Get callback remove on failure state
   * @param removeOnFailure_ true if it can be removed, otherwise false
   */
  void setRemoveOnFailure(bool removeOnFailure_) { removeOnFailure_m=removeOnFailure_; }
  
  /**
   * Set if callback can be removed if invokation failure limit is exceeded
   * @return true if it can be removed, otherwise false
   */
  bool doRemoveOnFailure() const { return removeOnFailure_m; }

  /**
   * Get callback reference
   * @return callback reference
   */
   Callback_ptr getCallback() const { return callback_mp; }

  /**
   * Get callback type
   * @return callback type
   */
   BACIValue::Type getType() const { return type_m; }

  /**
   * Get callback descriptor
   * @return callback descriptor (passed by client)
   */
   CBDescIn getDescIn() const { return descIn_m; }

  /**
   * Get Component (owner of the callback)
   * @return pointer to Component object
   */
   BACIComponent* getComponent() const { return component_mp; }
  
private:

  /**
   * Callback ID
   */
  int id_m;

  /**
   * Callback reference
   */
  Callback_ptr callback_mp;

  /**
   * Callback type
   */
  BACIValue::Type type_m;

  /**
   * Callback descriptor (passed by client)
   */
  CBDescIn descIn_m;

  /**
   * Callback owner
   */
  BACIComponent* component_mp;

  /**
   * Invokation failure counter
   */
  int failureCount_m;

  /**
   * State if callback can be removed if invokation failure counter si exceeded 
   */
  bool removeOnFailure_m;
  
  /**
   * Invokation failure limit
   */
  static const int failureLimitCount_m;

    /**
     * ALMA C++ coding standards state copy constructors should be disabled.
     */
    BACICallback(const BACICallback&);  

};   /* BACICallback */

/* ------------------------------------------------------------------------ */
/** @defgroup BACICallbackTableTemplate BACICallbackTable Class
 * The BACICallbackTable class is a templated typedef so there is no actual inline doc generated for it per-se.
 * @{
 * Callback table
 */
typedef Registrar<int, BACICallback*> BACICallbackTable;
/** @} */

/* ------------------------------------------------------------------------ */

 }; 

#endif /* baciBACICallback_H */ 




