#ifndef acsexmplRampedPowerSupplyImpl_h
#define acsexmplRampedPowerSupplyImpl_h
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
* "@(#) $Id: acsexmplRampedPowerSupplyImpl.h,v 1.101 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use of the smart pointer for properties
* david 2002-10-03 added inheritance from ACSDO and removed many methods
* david 2002-07-02 added GNU license info
* gchiozzi 2002-01-28 cleaned up remaining "mount" strings and replaced with "acsexmpl"
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* msekoran 2001-07-17 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsexmplExport.h>

///We subclass from another component implementation
#include "acsexmplPowerSupplyImpl.h"

///CORBA generated servant stub
#include <acsexmplRampedPowerSupplyS.h>

///Includes for each BACI property used in this example
#include <baciROlong.h>
#include <baciRWstring.h>

///This example uses a custom DevIO for a ROlong property
#include "acsexmplLongDevIO.h"

///Include the smart pointer for the properties
#include <baciSmartPropertyPointer.h>

/** @file acsexmplRampedPowerSupplyImpl.h
 */


/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLRPSDOC Ramped Power Supply
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
RampedPowerSupply simulates the behavior of a ramped power supply.
&nbsp;All of this class's methods are asynchronous. &nbsp;What makes
RampedPowerSupply special is that it's IDL interface inherits from
PowerSupply and the C++ implementation is derived from PowerSupply's C++ implementation. 
Another interesting aspect of this example is PowerSupply and RampedPowerSupply are 
defined in two separate IDLs.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>the implementation of a component derived from another C++ component implementation.</li>
  <li>an understanding of simple asynchronous method implementation accomplished by inheriting methods from the ActionImplementator class.</li>
  <li>read-only and read-write property usage.</li>
  <li>writing values to read-only BACI properties by using the property's underlying DevIO instance.</li>
  <li>standard ACS logging macros.</li>
  <li>limited CORBA error handling.</li>
  <li>asynchronous error handling.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classRampedPowerSupply.html">Ramped Power Supply Class Reference</a></li>
  <li><a href="interfaceRampedPS_1_1RampedPowerSupply.html">Ramped Power Supply IDL Documentation</a></li>
  <li>Ramped Power Supply CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/**
 * Simulates the behavior of a ramped power supply by inheriting from Power Supply's IDL interface and C++ implementation.
 *  The class RampedPowerSupply simulates the behaviour of a ramped power supply and is
 *  derived from the PowerSupply IDL interface. This class shows how to utilize inheritance 
 *  in both the C++ and IDL class/interface.
 *
 *  Asynchronous calls are implemented using the ...... pattern and the ..... support classes.
 *  For each xxx action defined in the IDL interface two methods are provided:
 *   - xxx() just registers the action and installs the callback
 *   - xxxAction() performs (asyncronously) the action and invokes the callback when finished.
 *  The RampedPowerSupply::invokeAction method is called by the asynchronous dispatcher whenever there is an
 *  xxx pending action and it calls the xxxAction corresponding method.
 *
 *  Class RampedPowerSupply also provides the properties rampingStatus and rampingSteps in addition to anything
 *  inherited from PowerSupply.
 *  
 *  @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 *  Jozef Stefan Institute, Slovenia<br>
 *  @version "@(#) $Id: acsexmplRampedPowerSupplyImpl.h,v 1.101 2008/10/01 04:30:47 cparedes Exp $"
 */
class acsexmpl_EXPORT RampedPowerSupply: public PowerSupply,     //PowerSupply's superclass is CharacteristicComponent
					 public virtual POA_RampedPS::RampedPowerSupply    //CORBA servant stub
{
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components
     * @param name component name
     */
    RampedPowerSupply (
		       const ACE_CString &name,
		       maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    ~RampedPowerSupply();

/**
 * One of these function IDs will be passed to invokeAction().
 */
    const static int START_RAMPING=3;
    const static int STOP_RAMPING=4;

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
     * @return request to be performed by BACI
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
     * Implementation of async. startRamping() method
     * This is the function that actually starts ramping
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
    startRampingAction (baci::BACIComponent *component_p, 
			const int &callbackID,
			const CBDescIn &descIn, 
			baci::BACIValue *value_p,
			Completion &completion, 
			CBDescOut &descOut);
    /**
     * Implementation of async. stopRamping() method
     * This is the function that actually stops ramping
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
    stopRampingAction (baci::BACIComponent *component_p,
		       const int &callbackID,
		       const CBDescIn &descIn,
		       baci::BACIValue *value_p,
		       Completion &completion,
		       CBDescOut &descOut);
    
    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Starts ramping the power supply.
     * Implementation of IDL startRamping() interface.
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling RampedPowerSupply::startRampingAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb Callback when action has finished.
     * @param desc Callback used for holding information on timeout periods
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */     
    virtual void 
    startRamping (CORBA::Long rampingSteps,
		  ACS::CBvoid_ptr cb,
		  const ACS::CBDescIn &desc);
    
    /**
     * Stops ramping the power supply.
     * Implementation of IDL stopRamping() interface.
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling RampedPowerSupply::stopAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb Callback when action has finished.
     * @param desc Callback used for holding information on timeout periods
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */     
    virtual void 
    stopRamping (ACS::CBvoid_ptr cb,
		 const ACS::CBDescIn &desc);
    
    /**
     * Returns a reference to the rampingStatus property (status).
     * Implementation of IDL interface for the property.
     * @return a pointer to the BACI property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWstring_ptr
    rampingStatus ();
    
    /**
     * Returns a reference to the rampingStep property.
     * Implementation of IDL interface for the property.
     * @return a pointer to the BACI property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROlong_ptr 
    rampingStep ();
    
  private:
    /**
     *  m_rampingStep_p is the RampedPowerSupply's present step in the ramping process.
     */
     baci::SmartPropertyPointer<baci::ROlong> m_rampingStep_sp;

    /**
     * m_rampingStep_devio_p is the DevIO subclass passed to the RampingStep property when
     * it is created.  Essentially the DevIO is used to interact with hardware directly without
     * tying ACS to any specific hardware drivers resulting in added flexibility.  Please see
     * acsexmplLongDevIO.h to understand exactly how this is accomplished.
     */
    DevIO<CORBA::Long> *m_rampingStep_devio_p;

    /**
     *  m_rampingStatus_p is the RampedPowerSupply's status in the ramping process.
     */
     baci::SmartPropertyPointer<baci::RWstring> m_rampingStatus_sp;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const RampedPowerSupply&);
};
/*\@}*/
/*\@}*/

#endif   /* acsexmplRampedPowerSupplyImpl_h */



