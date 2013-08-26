#ifndef fridgeImpl_h
#define fridgeImpl_h
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
*
* "@(#) $Id: acsexmplFridgeImpl.h,v 1.115 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------\
* acaproni 2004-04-06 Use of smart pointer for properties
* david 2002-07-02 added GNU license info
* gchiozzi 2002-03-18 Replaced includes of fridge*.* with acsexmplFridge*.*
* gchiozzi 2001-02-15 Added declaration of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created created standard header
* msekoran 2001-03-10 integrated with new BACI; ALMA coding convention used; doc.
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <acsexmplFridgeS.h>

///Includes for each BACI property used in this example
#include <baciRWdouble.h>
#include <baciROdouble.h>
#include <enumpropROImpl.h>

///In this example, events are published to a channel implying we need this header
#include <acsncSimpleSupplier.h>

///Include the smart pointer for the properties
#include <baciSmartPropertyPointer.h>

///Include the acs thread header 
#include <acsThread.h>
 
using ACS::ThreadBase;

/** @file acsexmplFridgeImpl.h
 */


/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/
/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLFRIDGEDOC Fridge
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
FridgeControl is designed to simulate the behavior of a fridge. This example is 
fairly complex because it deals with the ACS event channel API, 
ACS threads, and enumerated properties. &nbsp;There are a total of four 
methods and four properties (two of which are enums).
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
  <li>thread management using ACS.</li>
  <li>writing values to read-only BACI properties by using the property's underlying DevIO instance.</li>
  <li>usage of enum properties via the ACS enum template.</li>
  <li>standard ACS logging macros.</li>
  <li>limited CORBA error handling.</li>
  <li>the implementation and use of an event channel supplier.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classFridgeControl.html">Fridge Class Reference</a></li>
  <li><a href="interfaceFRIDGE_1_1FridgeControl.html">Fridge IDL Documentation</a></li>
  <li>Fridge CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

// forward declaration
class FridgeControl;

/**
 * The class FridgeThread is a thread used by the FridgeControl class
 * to update the fridge's temperature.
 */
class FridgeThread : public ACS::Thread
{
	public:
		FridgeThread(const ACE_CString& name, 
			FridgeControl  * fridgeControl, 
			const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
			ACS::Thread(name)
		{
			ACS_TRACE("FridgeThread::FridgeThread");
			loopCounter_m = 0;
			fridgeControl_p = fridgeControl;
		}

		~FridgeThread() { ACS_TRACE("FridgeThread::~FridgeThread"); }

		virtual void runLoop();

	protected:
		int loopCounter_m;
		FridgeControl * fridgeControl_p;
};

/**
 * FridgeControl shows BACI threads as well as a notification channel supplier.
 * The class FridgeControl simulates the behaviour of a fridge.
 * It provides four synchronous methods: on, off, open, and close.
 * It also provides the properties: refTemperature, powerStatus, doorStatus, and currTemperature.
 * powerStatus and doorStatus are both enums. Finally, FridgeControl is also a Supplier
 * for the "fridge" notification channel.
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: acsexmplFridgeImpl.h,v 1.115 2008/10/01 04:30:47 cparedes Exp $"
 */
class FridgeControl: public baci::CharacteristicComponentImpl,    //Standard component superclass
		     public virtual POA_FRIDGE::FridgeControl    //CORBA servant stub
{
  public:   

    /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. 
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     */
    FridgeControl(
		  const ACE_CString& name,
		  maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~FridgeControl();
    
    /*Override component lifecycle methods*/
    /**
     * Called after the last functional call to the component has finished.
     * The component should then orderly release resources etc.
     *
     * As required by the CharacteristicComponentImpl class,
     * I call explicitly the cleanUp() of the parent class.
     * This makes sure that all threads are stopped and the 
     * Component's state set.
     * Depending on what resources are used by a class implementing a 
     * Component and by the implementation of the parent class (if it does 
     * not inherit directly from acscomponent::ACSComponentImpl 
     * or baci:: CharacteristicComponentImpl) it might be
     * necessary to call the cleanuUp() method of the base class 
     * AFTER having released resources allocated by  the current class.
     * This is demonstrated in this example where we want to make sure
     * that the powerStatus is FRIDGE::OFF before letting the
     * cleanUp() of the base class stopping all the 
     * threads, including the one that regulates the temperature.
     * For an example where the cleanUp() of the parent class
     * is called before any other step, see the  Building class.
     * Always check the documentation of the parent class
     * and consider what resources are allocated by this class
     * to extablish the requirements for the execution of lifecycle
     * chained methods.
     *
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
     virtual void cleanUp();
    
    /**
     * Updates the temperature from the physical device
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly 
     */  
    void updateTemperature();

    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Turn on the fridge.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual void 
    on ();

    /**
     * Turn off the fridge.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual void 
    off ();
    
    /**
     * Open the door.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual void 
    open ();
    
    /**
     * Close the door.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual void 
    close ();
    
    /**
     * Property refTemperature is like a thermostat for the fridge.
     * @return Reference to the BACI property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::RWdouble_ptr 
    refTemperature ();
    
    /**
     * Property powerStatus shows whether the power is on or off.
     * @return Reference to the BACI property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual FRIDGE::ROOnOffStates_ptr 
    powerStatus ();
    
    /**
     * Property doorStatus shows the position of the door.
     * @return Reference to the BACI property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual FRIDGE::ROOpClStates_ptr 
    doorStatus ();
    
    /**
     * Property currTemperature shows the fridge's actual temperature.
     * @return Reference to the BACI property
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROdouble_ptr 
    currTemperature ();

  private:
    /**
     * Converts this devices useful data into a temperatureDataBlockEvent 
     * structure and then publishes that onto an event channel.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void
    loadData();

    /**
     *  m_refTemperature_sp is the temperature we want the fridge to be.
     */
     baci::SmartPropertyPointer<baci::RWdouble> m_refTemperature_sp;

    /**
     *  The fridge can be either turned on or off.
     */
     baci::SmartPropertyPointer<ROEnumImpl<
     	ACS_ENUM_T(FRIDGE::OnOffStates), POA_FRIDGE::ROOnOffStates>
     > m_powerStatus_sp;

    /**
     *  The fridge's door can be either opened or closed.
     */
     baci::SmartPropertyPointer<
    	ROEnumImpl<ACS_ENUM_T(FRIDGE::OpClStates), POA_FRIDGE::ROOpClStates>
    > m_doorStatus_sp;

    /**
     *  m_currTemperature_p is the temperature the fridge actually is.
     */
     baci::SmartPropertyPointer<baci::ROdouble> m_currTemperature_sp;
    
    /**
     *  m_controlLoop_p is only started once the fridge has been turned on().  This thread 
     *  is used to simulate the fridge's temperature trying to reach equillibrium with 
     *  the reference temperature. Once the fridge has been turned off(), this thread is
     *  suspended.
     */
    FridgeThread *m_controlLoop_p;

    /**
     *  This is the Supplier derived class used to publish data to the event channel.
     */
    nc::SimpleSupplier *m_FridgeSupplier_p;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const FridgeControl&);
};
/*\@}*/
/*\@}*/

#endif   /* acsexmplFridgeImpl_h */



