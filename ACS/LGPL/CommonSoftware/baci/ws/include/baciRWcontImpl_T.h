#ifndef _baciRWcontImpl_T_H_
#define _baciRWcontImpl_T_H_

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
* "@(#) $Id: baciRWcontImpl_T.h,v 1.23 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/13  created

*/

/** 
 * @file 
 * Header file for BACI Read-write Cont. Property Template Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPcontImpl_T.h>
#include <baciRWcommonImpl_T.h>
#include <ACSErrTypeCommon.h>

/**
 * Helper macro defines actions used to increment property values.
 */
#define INC_ACTION 2
/**
 * Helper macro defines actions used to decrement property values.
 */
#define DEC_ACTION 3

namespace baci {

/**
 * Implementation of RWcontImpl property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
///! order of inheritance is important: RWcommonImpl has to become befor PcontImpl

template <ACS_RW_C>
class baci_EXPORT RWcontImpl : public virtual POA_SK,
			       public RWcommonImpl<ACS_RW_TL>,
			       public PcontImpl<ACS_P_TL>    
{
    
    
  public:
    /**
     * Constuctor
     * @param name property name (e.q. AMSMount:decliantion)
     * @param component_p parent of the property
     */
    RWcontImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false);
    
    /**
     * Constuctor that has to be called from superclasses
     * @param name property name (e.q. AMSMount:decliantion)
     * @param component_p parent of the property
     */
    RWcontImpl(bool initValue, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO=0, bool flagdeldevIO=false);
    
    /**
     * Destructor
     */
    virtual ~RWcontImpl();
    
    /***
     * Implementation of async. set_value method
     */
/*
  virtual ActionRequest setValueAction(BACIComponent* component_p, int callbackID,
  const CBDescIn& descIn, BACIValue* value,
  Completion& completion, CBDescOut& descOut);
*/
    
    /* --------------- [ Action implementator interface ] -------------- */
    
    /**
     * Action dispatcher function (overrides from Pcommon)
     * @param function action funtion to be invoked
     * @param component_p owner of the action
     * @param callbackID id of the callback to be notified
     * @param descIn callback descriptor (passed by client)
     * @param value action data (e.g. value to be set)
     * @param completion error handing structure
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
				       CBDescOut& descOut);
    
/* ----------------- [ Other interface ] ----------------- */
    
    /**
     * Set value method (value mutator) overriden from RWcommon
     * To make RW property simetric to RO property
     * @param property property which requested value
     * @param value value to be returned
     * @param completion error handling structure
     * @param descOut callback descriptor
     */
    virtual void setValue(BACIProperty* property,
			  BACIValue* value, 
			  Completion &completion,
			  CBDescOut& descOut);
    
    /**
     * Implementation of async. increment method
     */
    virtual ActionRequest incrementAction(BACIComponent* component_p, 
					  int callbackID,
					  const CBDescIn& descIn, 
					  BACIValue* value,
					  Completion& completion, 
					  CBDescOut& descOut);

    /**
     * Implementation of async. decrement method
     */
    virtual ActionRequest decrementAction(BACIComponent* component_p, 
					  int callbackID,
					  const CBDescIn& descIn, 
					  BACIValue* value,
					  Completion& completion, 
					  CBDescOut& descOut);
    
    /* ----------------------------------------------------------------- */
    /* ---------------------- [ CORBA interface ] ---------------------- */
    /* ----------------------------------------------------------------- */
    
    /* -------------------- [ RW (cont) interface ] -------------------- */
    
    virtual TS min_value ();
    
    virtual TS max_value ();
    
    virtual void increment (ACS::CBvoid_ptr cb,
			    const ACS::CBDescIn & desc);
    
    virtual void decrement (ACS::CBvoid_ptr cb,
			    const ACS::CBDescIn & desc); 

  protected:
    virtual bool readCharacteristics();
    
  private:
    // Characteristics
    
    // RW (cont)
    TSM	min_value_m;
    TSM	max_value_m;
};

 }; 

#endif  /* baciRWcontImpl */









