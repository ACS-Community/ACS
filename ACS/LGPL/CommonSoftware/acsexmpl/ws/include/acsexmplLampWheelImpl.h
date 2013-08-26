#ifndef acsexmplLampWheelImpl_h
#define acsexmplLampWheelImpl_h
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
* "@(#) $Id: acsexmplLampWheelImpl.h,v 1.16 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 20041124   Created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicComponentImpl.h>
#include <acsexmplExport.h>
#include <acsexmplLampWheelS.h>

#include <baciROdouble.h>
#include <baciROlong.h>
#include <baciROstring.h>

///Import the smart pointer for the property
#include <baciSmartPropertyPointer.h>

#include <expat.h>
#include <list>

/// The description of each slot (read from CDB)
typedef struct {
    unsigned int num; // The number of the slot
    char lampName[128]; // The name of the lamp in the slot
    unsigned int warmUpTime; // The warm-up time
    unsigned int watt; // The watt of the lamp 
    unsigned int pos; // The position in the lamp (step)
} SlotDescriptor;

/** @file acsexmplLampWheelImpl.h
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLLAMPWHEELDOC Lamp
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class LampWheel is a very simple example of a component that simulates
the behavior of a generic lamp wheel. &nbsp;<br>
The description of the lamp wheel is on the CDB using the format dictated
by the schema definition.

It has one asynchronous method: move(...) that rotates the lamp wheel to the
position of the selected slot. 
It also has three read-only properties: position, desc, slots.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>the example shows how to read specific informations from the CDB</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classLampWheel.html">LampWheel Class Reference</a></li>
  <li><a href="interfaceacsexmplLampWheel_1_1LampWheel.html">Lamp IDL Documentation</a></li>
  <li>Lamp CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/**
 * The class LampWheel class simulates the behavior of a very simple lamp wheel using an asynchronous method to rotate it.
 *
 * The component reads the wheel data from the CDB.
 * The lamp wheel is caracterized by a description, a number of available slots
 * and a description of the used slots. All these data reside on the CDB.
 * In particular, the number of defined slots is not defined a priori
 * but is retrieved by parsing the CDB record for the component.
 *
 * At startup, the component read the description of the wheel from the CDB
 * and stores the configuration into a list.
 * The move method moves the wheel to the position defined for the passed
 * slot number. The position is read from the list (i.e. from the CDB).
 * A log message inform the user about the selected lamp.
 * 
 * @author <a href=mailto:acaproni@eso.org>Alessandro Caproni</a>,
 * @version "@(#) $Id: acsexmplLampWheelImpl.h,v 1.16 2008/10/09 08:41:11 cparedes Exp $"
 */
class acsexmpl_EXPORT LampWheel: public baci::CharacteristicComponentImpl,//Standard component superclass
	    public virtual POA_acsexmplLampWheel::LampWheel,//CORBA servant stub
	    public baci::ActionImplementator //Asynchronous method helper class
{
  public:
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. 
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     */
    LampWheel(
	      const ACE_CString& name,
	      maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~LampWheel();

    /* --------------------- [ Life cycle methods ] -------------------- */

    /**
   * Called to give the component time to initialize itself. 
   * For instance, the component could retrieve connections, read in 
   * configuration files/parameters, build up in-memory tables, ...
   * Called before {@link #execute}.
   * In fact, this method might be called quite some time before 
   * functional requests can be sent to the component.
   * Must be implemented as a synchronous (blocking) call.
   * If this method is overwritten in a chain of subclasses,
   * the developer has to make sure that all initialisations performed
   * by the implementation of the base class take place.
   * Check the documentation of direct parent class to know if this
   * is necessary (and do not forget to document each class for the need
   * to chain lifecycle methods in subclasses).
   * If necessary, the best way to do this is to call the implementation of the base
   * itself explicitly, as would be done implicitly in a constructor chain.
   * In this class the default implementation only sets the state
   * of the component first to acscomponent::ACS::COMPSTATE_INITIALIZING and then 
   * to acscomponent::ACS::COMPSTATE_INITIALIZED.
   * 
   * @return void
   * @throw ACSErr::ACSbaseExImpl
   * @htmlonly
   * <br><hr>
   * @endhtmlonly
   */
    virtual void initialize();

     /**
     * Called after {@link #initialize} to tell the 
     * component that it has to be ready to accept 
     * incoming functional calls any time. 
     * Must be implemented as a synchronous (blocking) call 
     * (can spawn threads though).
     * If this method is overwritten in a chain of subclasses,
     * the developer might need to make sure that all activities performed
     * by the implementation of the base class take place.
     * Check the documentation of direct parent class to know if this
     * is necessary (and do not forget to document each class for the need
     * to chain lifecycle methods in subclasses).
     * The best way to do this is to call the implementation of the base
     * itself explicitly, as would be done implicitly in a constructor chain.
     * In this class the default implementation only sets the state
     * of the component to acscomponent::ACS::COMPSTATE_OPERATIONAL
     *
     * @return void
     * @throw ACSErr::ACSbaseExImpl
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void execute();

     /**
     * Called after the last functional call to the component has finished.
     * The component should then orderly release resources etc.
     * If this method is overwritten in a subclass,
     * the developer has to make sure that all cleanup performed
     * by the implementation of the base class take place.
     * The best way to do this is to call the implementation of the base
     * itself explicitly, as would be done implicitly in a destructor chain.
     * If this method is overwritten in a chain of subclasses,
     * the developer might need to make sure that all activities performed
     * by the implementation of the base class take place.
     * Check the documentation of direct parent class to know if this
     * is necessary (and do not forget to document each class for the need
     * to chain lifecycle methods in subclasses).
     * The best way to do this is to call the implementation of the base
     * itself explicitly, as would be done implicitly in a destructor chain.
     * In this class the default implementation only sets the state
     * of the component to acscomponent::ACS::COMPSTATE_DESTROYING
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void cleanUp();

    /**
     * Called when due to some error condition the component is about 
     * to be forcefully removed
     * some unknown amount of time later (usually not very much...).
     * The component should make an effort to die as neatly as possible.
     * Because of its urgency, this method will be called asynchronously 
     * to the execution of any other method of the component.
     * If this method is overwritten in a chain of subclasses,
     * the developer might need to make sure that all activities performed
     * by the implementation of the base class take place.
     * Check the documentation of direct parent class to know if this
     * is necessary (and do not forget to document each class for the need
     * to chain lifecycle methods in subclasses).
     * The best way to do this is to call the implementation of the base
     * itself explicitly, as would be done implicitly in a constructor chain.
     * In this class the default implementation only sets the state
     * of the component to acscomponent::ACS::ACS::COMPSTATE_ABORTING
     *
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual void aboutToAbort();
   
    /* --------------- [ Action implementator interface ] -------------- */    
    /**
     * Action dispatcher function
     * This function is called whenever an asynchronous request has to be handled.
     * It receives (as parameters) the description of the function and selects the proper 
     * implementation to call.
     * @param function Action funtion to be invoked.
     * @param cob Owner of the action.
     * @param callbackID ID of the callback to be notified.
     * @param descIn Callback descriptor (passed by client).
     * @param value Action data (e.g. value to be set).
     * @param completion Error handing structure.
     * @param descOut Callback descriptor which will be passed to client.
     * @return Request to be performed by BACI
     * <ul>
     *  <li><b><i>reqNone</b></i> - Do nothing (action will be kept in queue).
     *  <li><b><i>reqInvokeWorking</b></i> - Invoke <type>Callback::<i>working</i>.
     *  <li><b><i>reqInvokeDone</b></i> - Invoke <type>Callback::<i>done</i> and destroy callback.
     *  <li><b><i>reqDestroy</b></i> - Destroy callback (callback should have been called already by function).
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual baci::ActionRequest
    invokeAction (int function,
                  baci::BACIComponent *cob_p, 
                  const int &callbackID,
                  const CBDescIn &descIn,
                  baci::BACIValue *value_p,
                  Completion &completion,
                  CBDescOut &descOut);

    /**
     * Implementation of asynchrnous acsexmplLamp::Lamp::off() method
     * This is the function that actually switches off the Lamp
     * and, when completed, invokes the callback installed by the client
     * when it requested the action.
     * @param cob Owner of the action.
     * @param callbackID ID of the callback to be notified.
     * @param descIn Callback descriptor (passed by client).
     * @param value Action data (e.g. value to be set).
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
    moveAction (baci::BACIComponent *cob_p, 
	       const int &callbackID,
	       const CBDescIn &descIn, 
	       baci::BACIValue *value_p,
	       Completion &completion, 
	       CBDescOut &descOut);    
    /* --------------------- [ CORBA interface ] ----------------------*/
    
    /**
     * Move the lamp wheel to a specification slot.
     * The position of the slot is read from the database
     * Implementation of IDL move(...) interface.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling LampWhell::moveAction
     * The given callback is used to inform the caller when the action is
     * performed.
     * @param n The number of the slot to move to
     * @param callBack Callback when action has finished.
     * @param desc Callback used for holding information on timeout periods.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
 	virtual void move(CORBA::Short, ACS::CBvoid_ptr,
		const ACS::CBDescIn&); 

    /**
     * Returns a reference to the position property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROdouble_ptr position ();
    
    /**
     * Returns a reference to the desc property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROstring_ptr desc ();

    /**
     * Returns a reference to the descavailableSlots property
     * Implementation of IDL interface for the property.
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROlong_ptr slots ();

  private:

    // The name of the component with the alma/ prefix
    // We need the name to access the CDB
    ACE_CString m_fullName;

    
    /**
     *  m_position_sp is the position of the wheel.
     */
     baci::SmartPropertyPointer<baci::ROdouble> m_position_sp;

    /**
     *  m_desc_sp is the description of the wheel.
     */
     baci::SmartPropertyPointer<baci::ROstring> m_desc_sp;

    /**
     * m_slots_sp is the number of available slots
     * in the wheel
     */
    baci::SmartPropertyPointer<baci::ROlong>m_slots_sp;

    /**
     * The list of descriptions of each slot of the wheel:
     * it is the configuration of the lampWheel
     * It is built reading the CDB
     */
    std::list<SlotDescriptor> m_lampWheelConfiguration;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const LampWheel&);

    /**
     * Read the CDB and fill the list with the actual configuration
     * of each slot
     *
     * @param config The list describing the configuration
     * @return The number of slots found in the CDB
     */
    int retrieveConfigurationFromCDB(std::list<SlotDescriptor>& config);

    /**
     * The handler for the start element
     *
     * @see libexpat documentation for further info
     */
    static void start_hndl(void *data, const XML_Char *el, const XML_Char **attr);

    /**
     * The handler for the end element
     *
     * @see libexpat documentation for further info
     */
    static void end_hndl(void *data, const XML_Char *el);
    
   /**
    * The handler for the char element
    *
    * @see libexpat documentation for further info
    */
    static void char_hndl(void *data, const XML_Char *s, int len);

};
/*\@}*/
/*\@}*/

#endif   /* acsexmplLampWheelImpl_h */



