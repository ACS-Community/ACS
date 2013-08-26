#ifndef acsexmplPowerSupplyImpl_h
#define acsexmplPowerSupplyImpl_h
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) European Southern Observatory, 2002
*    Copyright by ESO (in the framework of the ALMA collaboration)
*    and Cosylab 2002, All rights reserved
*
*    This library is free software; you can redistribute it and/or
*    modify it under the terms of the GNU Lesser General Public
*    License as published by the Free Software Foundation; either
*    version 2.1 of the License, or (at your option) any later version.
*
*    This library is distributed in the hope that it will be useful,
*    but WITHOUT ANY WARRANTY; without even the implied warranty of
*    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*    Lesser General Public License for more details.
*
*    You should have received a copy of the GNU Lesser General Public
*    License along with this library; if not, write to the Free Software
*    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
*
* "@(#) $Id: acsexmplPowerSupplyImpl.h,v 1.106 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Replaced the macros for the properties with the smart pointers
* david 2002-08-30 removed ActionFunction because its not used in this example
* david 2002-07-02 added GNU license info
* blopez   2002-04-05 Modified for ACSDO usage
* gchiozzi 2002-01-28 cleaned up remaining "mount" strings and replaced with "acsexmpl"
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* gchiozzi 2001-02-15 Added declaration of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created created standard header
* msekoran 2001-03-10 integrated with new baci::BACI; ALMA coding convention used; doc.
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>
#include <acsexmplExport.h>

///CORBA generated servant stub
#include <acsexmplPowerSupplyS.h>

///Includes for each baci::BACI property used in this example
#include <baciROdouble.h>
#include <baciRWdouble.h>
#include <baciROpattern.h>

///Import the template for the smart pointer
#include <baciSmartPropertyPointer.h>

#include "acsexmplPowerSupplyCurrentImpl.h"

/** @file acsexmplPowerSupplyImpl.h
 */

/**
 * One of these function IDs will be passed to invokeAction().
 */
#define ON_ACTION 0
#define OFF_ACTION 1
#define RESET_ACTION 2
/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLPSDOC Power Supply
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class PowerSupply simulates the behavior of a power supply.
&nbsp;PowerSupply implements three asynchrounous methods (on,
off, and reset) and has three RO/RW properties. &nbsp;One noteworthy
item is that some extra functionality has been given to the m_current_p
property (a RWdouble). &nbsp;This property is set to be the 
<a href="classPowerSupplyCurrent.html">PowerSupplyCurrent</a> object
(dervied from RWdouble) which overrides the setValue method. &nbsp;This
allows m_current_p to set the value of itself AND m_readback_p (the
power supply's actual current) at the same time.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
  <li>overriding component lifecycle methods (see execute).</li>
  <li>an understanding of simple asynchronous method implementation accomplished by inheriting methods from the ActionImplementator class.</li>
  <li>read-only and read-write property usage.</li>
  <li>writing values to read-only baci::BACI properties by using the property's underlying DevIO instance.</li>
  <li>standard ACS logging macros.</li>
  <li>limited CORBA error handling.</li>
  <li>asynchronous error handling.</li>
  <li>classes derived from baci::BACI properties and overriding of baci::BACI property methods.</li>
  <li>use of smart pointers for properties</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classPowerSupply.html">Power Supply Class Reference</a></li>
  <li><a href="interfacePS_1_1PowerSupply.html">Power Supply IDL Documentation</a></li>
  <li>Power Supply CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/**
 * Simulates the behavior of a power supply and overwrites an ACS property.
 * The class PowerSupply simulates the behaviour of a power supply.
 * It provides three methods: on, off and reset.
 * It also provides the properties current, readback and status.
 * Asynchronous calls are implemented using the ...... pattern and the ..... support classes.
 * For each xxx action defined in the IDL interface two methods are provided:
 *  - xxx() just registers the action and installs the callback
 *  - xxxAction() performs (asyncronously) the action and invokes the callback when finished.
 * The PowerSupply::invokeAction method is called by the asynchronous dispatcher whenever there is an
 * xxx pending action and it calls the xxxAction corresponding method.
 * 
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: acsexmplPowerSupplyImpl.h,v 1.106 2008/10/09 08:41:11 cparedes Exp $"
 */
class acsexmpl_EXPORT PowerSupply: public baci::CharacteristicComponentImpl,     //Standard component superclass
				   public virtual POA_PS::PowerSupply,    //CORBA servant stub
				   public baci::ActionImplementator    //Asynchronous method helper class
{
  public:
    /**
     * Constructor
     * 
     * @param name component name
     * @param containerService A pointer to the container services
     */
    PowerSupply(
		const ACE_CString &name,
		maci::ContainerServices * containerServices);
  
    /**
     * Destructor
     */
    virtual ~PowerSupply();  
    
    /* --------------- [ Action implementator interface ] -------------- */
    /**
     * Action dispatcher function
     * This function is called whenever an asynchronous request has to be handled.
     * It receives (as parameters) the description of the function and selects the proper 
     * implementation to call.
     * @param function action funtion to be invoked
     * @param component_p owner of the action
     * @param callbackID id of the callback to be notified
     * @param descIn callback descriptor (passed by client)
     * @param value_p action data (e.g. value to be set)
     * @param completion error handing structure
     * @param descOut callback descriptor which will be passed to client
     * @return request to be performed by baci::BACI
     * <ul>
     *  <li><b><i>reqNone</b></i> - do nothing (action will be kept in queue)
     *  <li><b><i>reqInvokeWorking</b></i> - invoke <type>Callback::<i>working</i>
     *  <li><b><i>reqInvokeDone</b></i> - invoke <type>Callback::<i>done</i> and destroy callback
     *  <li><b><i>reqDestroy</b></i> - destroy callback (callback should has been called already by function)
     * </ul>
     */
    virtual baci::ActionRequest 
    invokeAction (int function,
		  baci::BACIComponent *component_p, 
		  const int &callbackID, 
		  const CBDescIn &descIn, 
		  baci::BACIValue *value_p, 
		  Completion &completion, 
		  CBDescOut &descOut);
    
    /**
     * Implementation of async. on() method
     * This is the function that actually switches on the PowerSupply
     * and, when completed, invokes the callback installed by the client
     * when it requested the action.
     * @param component_p Owner of the action.
     * @param callbackID ID of the callback to be notified.
     * @param descIn Callback descriptor (passed by client).
     * @param value_p Action data (e.g. value to be set).
     * @param completion Error handing structure.
     * @param descOut Callback descriptor which will be passed to client.
     * @return Request to be performed by baci::BACI.
     * <ul>
     *  <li><b><i>reqNone</b></i> - Do nothing (action will be kept in queue).
     *  <li><b><i>reqInvokeWorking</b></i> - Invoke <type>Callback::<i>working</i>.
     *  <li><b><i>reqInvokeDone</b></i> - Invoke <type>Callback::<i>done</i> and destroy callback.
     *  <li><b><i>reqDestroy</b></i> - Destroy callback (callback should has been called already by function).
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual baci::ActionRequest 
    onAction (baci::BACIComponent *component_p, 
	      const int &callbackID,
	      const CBDescIn &descIn, 
	      baci::BACIValue *value_p,
	      Completion &completion, 
	      CBDescOut &descOut);
    
    /**
     * Implementation of async. off() method
     * This is the function that actually switches off the PowerSupply
     * and, when completed, invokes the callback installed by the client
     * when it requested the action.
     * @param component_p Owner of the action.
     * @param callbackID ID of the callback to be notified.
     * @param descIn Callback descriptor (passed by client).
     * @param value_p Action data (e.g. value to be set).
     * @param completion Error handing structure.
     * @param descOut Callback descriptor which will be passed to client.
     * @return Request to be performed by baci::BACI.
     * <ul>
     *  <li><b><i>reqNone</b></i> - Do nothing (action will be kept in queue).
     *  <li><b><i>reqInvokeWorking</b></i> - Invoke <type>Callback::<i>working</i>.
     *  <li><b><i>reqInvokeDone</b></i> - Invoke <type>Callback::<i>done</i> and destroy callback.
     *  <li><b><i>reqDestroy</b></i> - Destroy callback (callback should has been called already by function).
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual baci::ActionRequest 
    offAction (baci::BACIComponent *component_p,
	       const int &callbackID,
	       const CBDescIn &descIn, 
	       baci::BACIValue *value_p,
	       Completion &completion, 
	       CBDescOut &descOut);
    
    /**
     * Implementation of async. reset() method
     * This is the function that actually resets the PowerSupply
     * and, when completed, invokes the callback installed by the client
     * when it requested the action.
     * @param component_p Owner of the action.
     * @param callbackID ID of the callback to be notified.
     * @param descIn Callback descriptor (passed by client).
     * @param value_p Action data (e.g. value to be set).
     * @param completion Error handing structure.
     * @param descOut Callback descriptor which will be passed to client.
     * @return Request to be performed by BACI.
     * <ul>
     *  <li><b><i>reqNone</b></i> - Do nothing (action will be kept in queue).
     *  <li><b><i>reqInvokeWorking</b></i> - Invoke <type>Callback::<i>working</i>.
     *  <li><b><i>reqInvokeDone</b></i> - Invoke <type>Callback::<i>done</i> and destroy callback.
     *  <li><b><i>reqDestroy</b></i> - Destroy callback (callback should has been called already by function).
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual baci::ActionRequest 
    resetAction (baci::BACIComponent *component_p, 
		 const int &callbackID,
		 const CBDescIn &descIn,
		 baci::BACIValue *value_p,
		 Completion &completion, 
		 CBDescOut &descOut);
    
    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Switches on the power supply.
     * Implementation of IDL on() interface.
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling PowerSupply::onAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb Callback when action has finished.
     * @param desc Callback used for holding information on timeout periods
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */     
    virtual void 
    on (ACS::CBvoid_ptr cb,
	const ACS::CBDescIn &desc);
    
    /**
     * Switches off the power supply.
     * Implementation of IDL off() interface.
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling PowerSupply::offAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb Callback when action has finished.
     * @param desc Callback used for holding information on timeout periods
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */     
    virtual void 
    off (ACS::CBvoid_ptr cb,
	 const ACS::CBDescIn &desc);
    
    /**
     * Resets the power supply.
     * Implementation of IDL reset() interface.
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling PowerSupply::resetAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb Callback when action has finished.
     * @param desc Callback used for holding information on timeout periods
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    reset (ACS::CBvoid_ptr cb,
	   const ACS::CBDescIn &desc);
    
    /**  
     * Returns a reference to the current_p property (commanded current).
     * Implementation of IDL interface for the property.
     * @return a pointer to the property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWdouble_ptr 
    current ();
    
    /**
     * Returns a reference to the readback_p property (actual current).
     * Implementation of IDL interface for the property.
     * @return a pointer to the property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROdouble_ptr
    readback ();
    
    /**
     * Returns a reference to the status_p property (see acsexmplPowerSupplyImpl.cpp).
     * Implementation of IDL interface for the property.
     * @return a pointer to the property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual ACS::ROpattern_ptr 
    status ();

    /*Overriding component lifecycle methods*/
    /**
     * Called after {@link #initialize} to tell the 
     * component that it has to be ready to accept 
     * incoming functional calls any time. 
     * Must be implemented as a synchronous (blocking) call 
     * (can spawn threads though).
     *
     * @return void
     * @throw ACSErr::ACSbaseExImpl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void execute();
    
  protected:
    /**
     *  m_status_sp is the PowerSupply's state (values are in CDB).
     */
     baci::SmartPropertyPointer<baci::ROpattern> m_status_sp;
  
    
  private:
    /**
     *  m_readback_sp is the actual value of PowerSupply's current.
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_readback_sp;
    
    /**
     *  m_current_sp is the commanded current.
     */
     baci::SmartPropertyPointer<PowerSupplyCurrent> m_current_sp;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const PowerSupply&);
};
/*\@}*/
/*\@}*/

#endif   /* acsexmplPowerSupplyImpl_h */



