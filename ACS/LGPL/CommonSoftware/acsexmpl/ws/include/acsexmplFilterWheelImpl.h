#ifndef acsexmplFilterWheelImpl_h
#define acsexmplFilterWheelImpl_h
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
* "@(#) $Id: acsexmplFilterWheelImpl.h,v 1.8 2008/10/09 08:41:11 cparedes Exp $"
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
#include <acsexmplFilterWheelS.h>

#include <baciROdouble.h>
#include <baciROlong.h>
#include <baciROstring.h>

///Import the smart pointer for the property
#include <baciSmartPropertyPointer.h>

/** 
 * The description of each slots in the actual configuration
 * The struct if filled by reading the values (filter and slots) 
 * from the CDB
 * The number of each slot is its position in the vector
 */
typedef struct {
	// The step to move the wheel to in such a way the slot
	// is centered in the beam
	int step;
	// The name/type of the filter mounted in the slot
    char filterName[128]; 
    // The steps adjust the position for a perfect 
    // alignement for this filter
    int delta; 
} Descriptor;

/** @file acsexmplFilterWheelImpl.h
 */
/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLFILTERWHEELDOC Filter Wheel
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class FilterWheel is a very simple example of a component that simulates
the behavior of a generic filter wheel. &nbsp;<br>
The description of the wheel is on the CDB using the format dictated
by the schema definition.

It has one asynchronous method: moveFilterInBeam(...) 
that rotates the wheel to put selected filter in the beam. 
The adjust method moves the whell of the amount of steps passed as parameter. 
The adjust is used to calibrate the position of the filters and the slots.
It also has three read-only properties: position, desc, slots.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>the example shows how to read and write specific informations from the CDB using DAO, DAL and WDAL</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classFilterWheel.html">FilterWheel Class Reference</a></li>
  <li><a href="interfaceacsexmplFilterWheel_1_1FilterWheel.html">FilterWheel IDL Documentation</a></li>
  <li>FilterWheel CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/**
 * The class FilterWheel class simulates the behavior of a very simple filter wheel.
 *
 * The configuration of the wheel is on the CDB.
 * First of all the number of available slots of the wheel. This is read at startup.
 * In the CDB there is a list of position of the slots. This is the step to rotate
 * the wheel to have the center of the slot in the beam.
 * The adjust method allows the user to move the wheel of some step in order to center
 * the slot in the beam. When the center has been found, the calibrateWheel method
 * store the position as the center for the selected slot.
 * 
 * In the CDB there is also the definition for each possible filter.
 * For each filter there is its name, the slot where it is inserted in and the
 * delta i.e. the deviation of the center of the filter in respect of the
 * center of the slot (it could be a problem in building the filter for example,
 * storing this number we could mount the filter in every slot)
 * To found the center of the filter the operator uses again the adjust method.
 * When the center is found the delta can be stored in the filter definition
 * in the CDB using the calibrateFilter method.
 * 
 * The component reads the configuration of the wheel from the CDB at startup 
 * and stores number of the slots in the wheel, the position of the slots and
 * the definition of each filter are together in the same array of structs (the number
 * of the slot is represented by its position in the array).
 * When one of the two calibrate methods is executed the internal data
 * as well as the CDB values are updated. 
 * 
 * @author <a href=mailto:acaproni@eso.org>Alessandro Caproni</a>,
 * @version "@(#) $Id: acsexmplFilterWheelImpl.h,v 1.8 2008/10/09 08:41:11 cparedes Exp $"
 */
class acsexmpl_EXPORT FilterWheel: public baci::CharacteristicComponentImpl,//Standard component superclass
	    public virtual POA_acsexmplFilterWheel::FilterWheel,//CORBA servant stub
	    public baci::ActionImplementator //Asynchronous method helper class
{
  public:
    /**
     * Constructor
     * 
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     * @param containerService The pointer to the container services
     */
    FilterWheel(const ACE_CString& name,maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~FilterWheel();

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
   * @param containerServices  pointer to services provided by the container
   * @throw ACSErr::ACSbaseExImpl
   * @return void
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
     * @throw ACSErr::ACSbaseExImpl
     * @return void
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
     * Implementation of asynchrnous moveFilterInBeam method
     * This is the function that actually rotate the wheel to the slot of the 
     * selected filter (it also take in account the per-filter adjustament
     * represented by the delta filed in the description of the filter in the CDB)
     * When completed, invokes the callback installed by the client
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
    moveFilterAction (baci::BACIComponent *cob_p, 
	       const int &callbackID,
	       const CBDescIn &descIn, 
	       baci::BACIValue *value_p,
	       Completion &completion, 
	       CBDescOut &descOut);
          
        /**
     * Implementation of asynchrnous moveSlotInBeam method
     * This is the function that actually rotate the wheel to the selected slot
     * When completed, invokes the callback installed by the client
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
    moveSlotAction (baci::BACIComponent *cob_p, 
         const int &callbackID,
         const CBDescIn &descIn, 
           baci::BACIValue *value_p,
        Completion &completion, 
           CBDescOut &descOut);
    
    /**
     * Implementation of asynchrnous adjust() method
     * This is the function that moves the wheel of some steps
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
    adjustAction (baci::BACIComponent *cob_p, 
	       const int &callbackID,
	       const CBDescIn &descIn, 
	       baci::BACIValue *value_p,
	       Completion &completion, 
	       CBDescOut &descOut);
    /* --------------------- [ CORBA interface ] ----------------------*/
    
    /**
     * Move the filter wheel to a specification slot.
     * Implementation of IDL moveFilterInBeam(...) interface.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling LampWhell::moveAction
     * The given callback is used to inform the caller when the action is
     * performed.
     */ 
 	virtual void moveFilterInBeam(const char* , ACS::CBvoid_ptr,
		const ACS::CBDescIn&); 
    
    /**
     * Move the filter wheel to a specific slot.
     * Implementation of IDL moveSlotInBeam(...) interface.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling LampWhell::moveAction
     * The given callback is used to inform the caller when the action is
     * performed.
     */ 
    virtual void moveSlotInBeam(int, ACS::CBvoid_ptr, const ACS::CBDescIn&);

    /**
     * Move the wheel of the specified number of step
     *
     * @param the step to move the wheel
     */
    virtual void adjust(int, ACS::CBvoid_ptr, const ACS::CBDescIn&);

    /**
     * Calibrate the delta of the filter by reading the actual position
     * and the step of the slot where the filter is mounted
     * (it also updates the value in the CDB)
     * 
     * @param name The name of the filter to calibrate
     * @return The new delta for the filter
     */
    virtual CORBA::Long calibrateFilter(const char* name); 
    
    /**
     * Calibrate the step for the slot by reading the actual position
     * (it also updates the value in the CDB)
     * 
     * @param slot The number of the slot to calibrate
     * @return The new step for the slot
     */
    virtual CORBA::Long calibrateWheel(int slot);


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
     * The array describing the actual configuration of the wheel:
     * It is built by reading the CDB
     * The number of item in the array is given by the number of available slots
     * defined in the CDB
     */
    Descriptor* m_wheelConfiguration;

	/**
	 * Read the CDB to build the actual configuration of the wheel
	 * 
	 * @param descr The list of descriptor describing the actual 
	 *              configuration of the wheel
	 */
    void readConfiguration(Descriptor* descr);

    /**
     * Permanently write the new delta for the given filter in the CDB
     *
     * @param name The name of the filter
     * @param delta The new offset for the filter
     */
     void updateFilter(ACE_CString name, int delta);
     
     /**
      * Permanently write the new position for the given slot in the CDB
      * 
      * @param slot: The number of the slot to update
      * @param step: the new position of the slot
      */
     void updateWheel(int slot, int step);

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const FilterWheel&);

};
/*\@}*/
/*\@}*/

#endif   /* acsexmplLampWheelImpl_h */

