#ifndef _baciRWcommonImpl_T_H_
#define _baciRWcommonImpl_T_H_

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
* "@(#) $Id: baciRWcommonImpl_T.h,v 1.26 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/02/12  created
*/

/** 
 * @file 
 * Header file for BACI Read-write Common Property Template Class.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPcommonImpl_T.h>

/**
 * Helper macro for use with template parameters.
 */
#define ACS_RW_C ACS_P_C, class TIN
/**
 * Helper macro for use with template parameters.
 */
#define ACS_RW_TL ACS_P_TL, TIN
/**
 * Helper macro for use with template parameters.
 */
#define ACS_RW_T(T, TC) ACS_P_T(T, TC), POA_ACS::RW##T, TC
//TC, ACS::CB##T, ACS::T##Seq, ACS::T##Seq_out, ACS::Monitor##T, Monitor##T, TC, TC, TC, TC, POA_ACS::RW##T

/**
 * Helper macro defines actions used to retrieve property values.
 */
#define GET_ACTION 0
/**
 * Helper macro defines actions used to write property values.
 */
#define SET_ACTION 1

namespace baci {

/**
 * Implementation of RWcommonImpl property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */
template <ACS_RW_C>
class baci_EXPORT RWcommonImpl : public virtual POA_SK,
				 public PcommonImpl<ACS_P_TL> 
{


public:
  /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
    RWcommonImpl(const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO/*=0*/, bool flagdeldevIO/*=false*/);

   /**
   * Constuctor that calls only readCharacteristics()
   * @param name property name (e.q. AMSMount:decliantion)
   */
  RWcommonImpl(bool init, const ACE_CString& name, BACIComponent *component_p, DevIO<TM> *devIO, bool flagdeldevIO);

  /**
   * Destructor
   */
  virtual ~RWcommonImpl();
	
    /***
     * Implementation of async. set_value method
     */
    virtual ActionRequest setValueAction(BACIComponent* component_p, 
					 int callbackID,
					 const CBDescIn& descIn, 
					 BACIValue* value,
					 Completion& completion, 
					 CBDescOut& descOut);

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
   * Set value method (value mutator)
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

  /* ----------------------------------------------------------------- */
  /* ---------------------- [ CORBA interface ] ---------------------- */
  /* ----------------------------------------------------------------- */

  /* -------------------- [ RW (common) interface ] -------------------- */

    virtual ACSErr::Completion * set_sync (
	    TIN value
	  );
	
	virtual void set_async (
	    TIN value,
	    ACS::CBvoid_ptr cb,
	    const ACS::CBDescIn & desc
	  );
	
	virtual void set_nonblocking (
	    TIN value
	      
	  );

};

// #include "baciRWcommonImpl_T.i"

 }; 

#endif  /* baciRWcommonImpl */






