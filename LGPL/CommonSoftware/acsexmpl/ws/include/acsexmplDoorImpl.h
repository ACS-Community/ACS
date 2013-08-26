#ifndef acsexmplDoorImpl_h
#define acsexmplDoorImpl_h
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
* "@(#) $Id: acsexmplDoorImpl.h,v 1.108 2008/10/09 08:41:11 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use the smart pointer for the properties
* david 2002-07-02 added GNU license info
* blopez  2002-04-05  m_poa declaration removed
* blopez  2002-04-04  Modified for ACSDO usage
* blopez  2002-03-27  Comments changed to doxygen format
* blopez  2002-03-11  Created
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>

///CORBA generated servant stub
#include <acsexmplBuildingS.h>

///Includes for each BACI property used in this example
#include <baciROdouble.h>
#include <baciRWdouble.h>
#include <baciROlong.h>
#include <baciROstring.h>

///Include the smart prointer for the properties
#include <baciSmartPropertyPointer.h>

///Include the acs thread header 
#include <acsThread.h>

/** @file acsexmplDoorImpl.h
 */

class Door; // declaration

// DoorThread - an ACS thread used by the Door class.
// When the reference position changes the door is moved to this
// new position, and the device state is defined.
class DoorThread : public ACS::Thread
{
	public: 

	DoorThread(const ACE_CString &name, Door * door_ptr, 
		const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime) :
		ACS::Thread(name) 
	{
		door_p = door_ptr;
	}

	~DoorThread() 
	{ 
		ACS_TRACE("DoorThread::~DoorThread"); 
	}
    
        virtual void onStart();

	virtual void runLoop();

    virtual void onStop();
	
	private:
		Door * door_p;
};

/** @addtogroup ACSEXMPLTOC
 * @{
 * @htmlonly
  @endhtmlonly
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLDOORDOC Door
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
Door, as defined by the IDL interface, is another simple example of a
component like Lamp except that it also contains one synchronous method, 
move(...). &nbsp;The two asynchronous methods are open() and close() 
respectively. &nbsp;This example has four properties and uses an ACS thread.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
  <li>overriding component lifecycle methods (see execute).</li>
  <li>asynchronous method implementation accomplished by inheriting methods from the ActionImplementator class.</li>
  <li>read-only and read-write property usage.</li>
  <li>writing values to read-only BACI properties by using the property's underlying DevIO instance.</li>
  <li>limited exception handling.</li>
  <li>extensive CORBA error handling.</li>
  <li>thread management using ACS.</li>
  <li>standard ACS logging macros.</li>
  <li>very simple example of a state machine (i.e., the Door's positions).</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classDoor.html">Door Class Reference</a></li>
  <li><a href="interfaceacsexmplBuilding_1_1Door.html">Door IDL Documentation</a></li>
  <li>Door CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/** 
 *  Door is a simple component that utilizes just about every ACS API.
 *  This class implements an example device "Door".  It provides both synchronous (move(...))
 *  and asynchronous methods (open() and close()).  A control loop thread
 *  simulates the door's movements. Error and exception handling have been
 *  included.
 *  Asynchronous calls are implemented using the callback pattern via a support class.
 *  For each xxx action defined in the IDL interface two methods are provided:
 *   - xxx() just registers the action and installs the callback
 *   - xxxAction() performs (asyncronously) the action and invokes the callback when finished.
 *  The Door::invokeAction method is called by the asynchronous dispatcher whenever there is a
 *  xxx pending action and it calls the corresponding xxxAction method.
 * @version "@(#) $Id: acsexmplDoorImpl.h,v 1.108 2008/10/09 08:41:11 cparedes Exp $"
 */
class Door: public baci::CharacteristicComponentImpl,     //Standard component superclass
            public virtual POA_acsexmplBuilding::Door,    //CORBA servant stub
            public baci::ActionImplementator    //ACS class used for asynchronous methods
{
  friend void DoorThread::runLoop();

  public:   
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components
     * @param name component name
     */
    Door(
	 const ACE_CString& name,
	 maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~Door();
    
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
     * @param value_p action data (e.g., parameters to the original CORBA method)
     * @param completion error handing structure
     * @param descOut callback descriptor which will be passed to client
     * @return request to be performed by BACI
     * <ul>
     *  <li><b><i>reqNone</b></i> - do nothing (action will be kept in queue)
     *  <li><b><i>reqInvokeWorking</b></i> - invoke <type>Callback::<i>working</i>
     *  <li><b><i>reqInvokeDone</b></i> - invoke <type>Callback::<i>done</i> and destroy callback
     *  <li><b><i>reqDestroy</b></i> - destroy callback (callback should has been called already by function)
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
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
     * Implementation of asynch. open() method
     * This is the function that actually opens the Door
     * and, when completed, invokes the callback installed by the client
     * when it requested the action.
     * @param component_p owner of the action
     * @param callbackID id of the callback to be notified
     * @param descIn callback descriptor (passed by client)
     * @param value_p action data (e.g., parameters to the original CORBA method)
     * @param completion error handing structure
     * @param descOut callback descriptor which will be passed to client
     * @return request to be performed by BACI
     * <ul>
     *  <li><b><i>reqNone</b></i> - do nothing (action will be kept in queue)
     *  <li><b><i>reqInvokeWorking</b></i> - invoke <type>Callback::<i>working</i>
     *  <li><b><i>reqInvokeDone</b></i> - invoke <type>Callback::<i>done</i> and destroy callback
     *  <li><b><i>reqDestroy</b></i> - destroy callback (callback should has been called already by function)
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual baci::ActionRequest 
    openAction (baci::BACIComponent *component_p, 
		const int &callbackID,
		const CBDescIn &descIn, 
		baci::BACIValue *value_p,
		Completion &completion, 
		CBDescOut &descOut);

    /**
     * Implementation of asynch. close() method
     * This is the function that actually closes the Door
     * and, when completed, invokes the callback installed by the client
     * when it requested the action.
     * @param component_p owner of the action
     * @param callbackID id of the callback to be notified
     * @param descIn callback descriptor (passed by client)
     * @param value action data (e.g., parameters to the original CORBA method)
     * @param completion error handing structure
     * @param descOut callback descriptor which will be passed to client
     * @return request to be performed by BACI
     * <ul>
     *  <li><b><i>reqNone</b></i> - do nothing (action will be kept in queue)
     *  <li><b><i>reqInvokeWorking</b></i> - invoke <type>Callback::<i>working</i>
     *  <li><b><i>reqInvokeDone</b></i> - invoke <type>Callback::<i>done</i> and destroy callback
     *  <li><b><i>reqDestroy</b></i> - destroy callback (callback should has been called already by function)
     * </ul>
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual baci::ActionRequest 
    closeAction (baci::BACIComponent *component_p, 
		 const int &callbackID,
		 const CBDescIn &descIn, 
		 baci::BACIValue *value_p,
		 Completion &completion, 
		 CBDescOut &descOut);
    

    /* --------------------- [ CORBA interface ] ----------------------*/
    /**
     * Opens the door
     * Implementation of IDL open() interface.
     * This method just registers the request in the asyncronous queue together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling Door::openAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb callback when action has finished
     * @param desc callback used for holding information on timeout periods
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void
    open (ACS::CBvoid_ptr cb,
	  const ACS::CBDescIn &desc);
    
    /**
     * Closes the door
     * Implementation of IDL close() interface.
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.
     * The actual action will be invoked asyncronously by the asynchonous call manager by calling Door::closeAction
     * The given callback is used to inform the caller when the action is performed.
     * @param cb callback when action has finished
     * @param desc callback used for holding information on timeout periods
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual void 
    close (ACS::CBvoid_ptr cb,
	   const ACS::CBDescIn &desc);
    
    /**
     * Moves the door to the given position
     * Implementation of the IDL move(...) interface.
     * This method, while synchronous, only changes the reference position (i.e., m_ref_position_p) 
     * while DoorThread actually handles the change in the door position via an ACS thread. 
     * @param pos requested position - must be between 0 and a 100.
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    move (CORBA::Double pos);
    
    /**
     * Property for readback m_position_p
     * Implementation of IDL interface for the property.
     * @return Read-only double pointer to the door's current position
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROdouble_ptr 
    position ();
    
    /**
     * Property for reference position
     * Implementation of IDL interface for the property. 
     * @return Read/write double pointer to where the door should be physically located
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual ACS::RWdouble_ptr
    ref_position ();
    
    /**
     * Property for substate
     * Implementation of IDL interface for the property.
     * @return Read-only long integer pointer to the door's present state
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual ACS::ROlong_ptr 
    substate ();
    
    /**
     * Property for version
     * Implementation of IDL interface for the property.
     * @return Read-only string pointer to the door implementation's version
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual ACS::ROstring_ptr 
    version ();

    /*Override component lifecycle methods*/
    /**
     * Called after {@link #initialize} to tell the 
     * component that it has to be ready to accept 
     * incoming functional calls any time. 
     * Must be implemented as a synchronous (blocking) call 
     * (can spawn threads though).
     *
     * @throw ACSErr::ACSbaseExImpl
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void execute();
    
  private:
    /**
     * Checks substate
     * Checks for the device's substate. If the device is busy it returns
     * an error.  The purpose of this function is to show how to handle
     * local error information.
     * @param error completion return variable 
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    checkSubstate(CompletionImpl *&error_p);
    
    /**
     *  m_ref_position_sp is the position the Door should be in 
     */
     baci::SmartPropertyPointer<baci::RWdouble> m_ref_position_sp;

    /**
     *  m_position_sp is the Door's actual position
     */
     baci::SmartPropertyPointer<baci::ROdouble> m_position_sp;

    /**
     *  m_substate_sp is the state the door is currently in
     *  @see acsexmplDoorImpl.cpp
     */
     baci::SmartPropertyPointer<baci::ROlong> m_substate_sp;

    /**
     *  m_version_sp is the Door's current version 
     */
     baci::SmartPropertyPointer<baci::ROstring> m_version_sp;

    /*
     * m_door_thread_p is the pointer to the DoorThread
     */
     DoorThread * m_doorThread_p;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Door&);
}; 

/*\@}*/
/*\@}*/
#endif   /* acsexmplDoorImpl_h */



