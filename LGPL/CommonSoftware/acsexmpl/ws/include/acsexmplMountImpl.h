#ifndef acsexmplMountImpl_h
#define acsexmplMountImpl_h
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
* "@(#) $Id: acsexmplMountImpl.h,v 1.100 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni 2004-04-06 Use of smart pointer for properties
* david 2002-07-02 added GNU license info
* bjeram 2002-06-25 added  const ACS::CBDescIn & desc,
* david 2002-06-10 Added doxygen specific comments
* blopez   2002-04-05 Modified for ACSDO usage
* gchiozzi 2002-01-28 cleaned up remaining "mount" strings and replaced with "acsexmpl"
* almamgr 2002-01-22 Replaced old include files with new axsexmpl... files
* msekoran 2001-06-23 minor changes to work with acsutil module
* msekoran 2001-04-03 new baci
* gchiozzi 2001-02-15 Added declaration of get_interface() method for Object Explorer
* gchiozzi 2001-02-15 created standard header
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

///Contains the defintion of the standard superclass for C++ components
#include <baciCharacteristicComponentImpl.h>
#include <acsexmplExport.h>

///CORBA generated servant stub
#include <acsexmplMountS.h>

///Includes for each BACI property used in this example
#include <baciROdouble.h>

///Include the smart pointer for properties
#include <baciSmartPropertyPointer.h>

/** @file acsexmplMountImpl.h
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOCCOMPONENTS
*/
/*@{
*/

/** @defgroup ACSEXMPLMOUNTDOC Mount
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class Mount simulates the behaviour of an antenna interface.
&nbsp;It provides only two asynchronous methods: objstar(...) and
objfix(...). &nbsp;Both of these methods only write data into virtual
read-only double properties: actAz, actEl, cmdAz, and cmdEl.
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::CharacteristicComponent IDL interface.</li>
  <li>understanding of asynchronous method implementation accomplished by inheriting methods from the baci::ActionImplementator class.</li>
  <li>asynchronous methods which take in (multiple) parameters.</li>
  <li>clean implementation of the invokeAction method using an array of function pointers.</li>
  <li>a "real-world" example dealing with radio astronomy.</li>
  <li>writing values to read-only BACI properties by using the property's underlying DevIO instance.</li>
  <li>standard ACS logging macros.</li>
  <li>ACS asynchronous error handling.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classMount.html">Mount Class Reference</a></li>
  <li><a href="interfaceMOUNT__ACS_1_1Mount.html">Mount IDL Documentation</a></li>
  <li>Mount CDB XML Schema</li>
</ul>
</div>
   @endhtmlonly
 * @}
 */

/** 
 * Simulates the behavior of an antenna interface.
 * The class Mount is a good example of a component and simulates the behaviour of an antenna interface.
 * It provides two asynchronous methods: objstar and objfix.  The methods only write the data into 
 * virtual properties instead of hardware.
 * Asynchronous calls are implemented using the ...... pattern and the ..... support classes.
 * For each xxx action defined in the IDL interface two methods are provided:
 *  - xxx() just registers the action and installs the callback
 *  - xxxAction() performs (asyncronously) the action and invokes the callback when finished.
 * The Mount::invokeAction method is called by the asynchronous dispatcher whenever there is an
 * xxx pending action and it calls the xxxAction corresponding method.  Also, invokeAction calls 
 * these methods by simply using a function pointer (ActionFunction) instead of specifying each method.
 * @version "@(#) $Id: acsexmplMountImpl.h,v 1.100 2008/10/01 04:30:47 cparedes Exp $"
 */
class acsexmpl_EXPORT Mount: public baci::CharacteristicComponentImpl,     //Standard component superclass
			     public virtual POA_MOUNT_ACS::Mount,   //CORBA servant stub
			     public baci::ActionImplementator    //Asynchronous method helper class
{
  public:
     /**
     * Constructor
     * 
     * @param name component's name. This is also the name that will be used to find the
     * configuration data for the component in the Configuration Database.
     * @param containerService The pointer to the container services
     */
    Mount(
	  ACE_CString name,
	  maci::ContainerServices * containerServices);
    
    /**
     * Destructor
     */
    virtual ~Mount();
    
    /* --------------- [ Action implementator interface ] -------------- */
    /**
     * Action dispatcher function
     * This function is called whenever an asynchronous request has to be handled.
     * It receives (as parameters) the description of the function and selects the proper 
     * implementation to call.
     * @param function Action funtion to be invoked.
     * @param component_p Owner of the action.
     * @param callbackID ID of the callback to be notified.
     * @param descIn Callback descriptor (passed by client).
     * @param value_p Action data (e.g. value to be set).
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
		  baci::BACIComponent *component_p, 
		  const int &callbackID, 
		  const CBDescIn &descIn, 
		  baci::BACIValue *value_p, 
		  Completion &completion, 
		  CBDescOut &descOut);
    
    /**
     * Implementation of asynchronous MOUNT_ACS::Mount::obstar() method
     * This is the function that actually runs obstar(...)
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
    obstarAction (baci::BACIComponent *component_p, 
		  const int &callbackID,
		  const CBDescIn &descIn, 
		  baci::BACIValue *value_p,
		  Completion &completion, 
		  CBDescOut &descOut);
        
    /**
     * Implementation of asynchronous MOUNT_ACS::Mount::objfix() method
     * This is the function that actually runs objfix(...)
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
    objfixAction (baci::BACIComponent *component_p, 
		  const int &callbackID,
		  const CBDescIn &descIn, 
		  baci::BACIValue *value_p,
		  Completion &completion, 
		  CBDescOut &descOut);
    
    /* --------------------- [ CORBA interface ] ----------------------*/   
    /**
     * (Pre)sets a new equatorial source for the antenna to track.
     * The source position is given in geocentric equatorial J2000 
     * coordinates.  The actual ra and dec coordinates are written 
     * to the properties cmdAz, cmdEl, actAz and actEl.
     *
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.  The actual action will be invoked asyncronously by the asynchonous 
     * call manager by calling Mount::obstarAction(...).  The given callback is used to inform the caller 
     * when the action is performed.
     * @param ra        source right ascension (hour)
     * @param dec       source declination (hour)
     * @param pmRa      source sky proper motion in right ascension (arc-sec/year)
     * @param pmDec     source sky proper motion in declination (arc-sec/year)
     * @param radVel    source radial velocity (kilometer/sec)
     * @param par       source parallax correction (arc-sec)
     * @param type      Apparent or Mean
     * @param callBack  Callback when action has finished.
     * @param desc      Callback used for holding information on timeout periods
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */   
    virtual void 
    obstar (CORBA::Double ra,
	    CORBA::Double dec,
	    CORBA::Double pmRa,
	    CORBA::Double pmDec,
	    CORBA::Double radVel,
	    CORBA::Double par,
	    MOUNT_ACS::Mount::coordType type,
	    ACS::CBvoid_ptr callBack,
	    const ACS::CBDescIn &desc);
    
    /**
     * (Pre)sets a new non-moving position for the antenna.
     * The position coordinates are given in azimuth and elevation.
     * The actual az and elev values are written to the properties
     * cmdAz, cmdEl, actAz and actEl.
     *
     * This method just registers the request in the asyncronous queue, together with the associated callback 
     * and returns control immediatly.  The actual action will be invoked asyncronously by the asynchonous 
     * call manager by calling Mount::objfixAction(...).  The given callback is used to inform the caller 
     * when the action is performed.
     * @param az        position azimuth (degree)
     * @param elev      position elevation (degree)
     * @param callBack  Callback when action has finished.
     * @param desc      Callback used for holding information on timeout periods
     * @return void
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */    
    virtual void 
    objfix (CORBA::Double az,
	    CORBA::Double elev,
	    ACS::CBvoid_ptr callBack,
	    const ACS::CBDescIn &desc);
    
    /**
     * Returns a reference to the cmdAz property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only double property cmdAz
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */ 
    virtual ACS::ROdouble_ptr 
    cmdAz ();
    
    /**
     * Returns a reference to the cmdEl property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only double property cmdEl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual ACS::ROdouble_ptr 
    cmdEl ();
    
    /**
     * Returns a reference to the actAz property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only double property actAz
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */   
    virtual ACS::ROdouble_ptr 
    actAz ();
    
    /**
     * Returns a reference to the actEl property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only double property actEl
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */    
    virtual ACS::ROdouble_ptr 
    actEl ();
    
  private:
    /**
     * Definition of ActionFunction (member function of Mount class)
     * ActionFunction is used to run the asynchronous methods from within 
     * invokeAction(...) without actually having to specify 
     * calls to each ...Action(...) method.  This is easily accomplished since 
     * all ...Action(...) methods return an baci::ActionRequest.
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
    typedef baci::ActionRequest (Mount::*ActionFunction)(baci::BACIComponent *component_p, 
						   const int &callbackID,
						   const CBDescIn &descIn, 
						   baci::BACIValue *value_p,
						   Completion &completion, 
						   CBDescOut &descOut);
    
    /**
     *  Please see ActionFunction.
     *  m_actions[0] = obstarAction(...)
     *  m_actions[1] = objfixAction(...)
     */
    ActionFunction m_actions[2];
    
    /**
     *  m_cmdAz_sp is the antenna's commanded azimuth
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_cmdAz_sp;

    /**
     *  m_cmdEl_sp is the antenna's commanded elevation
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_cmdEl_sp;

    /**
     *  m_actAz_sp is the antenna's actual azimuth
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_actAz_sp;

    /**
     *  m_actEl_sp is the antenna's actual elevation
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_actEl_sp;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Mount&);
};
/*\@}*/
/*\@}*/

#endif /*!acsexmplMountImpl_H*/



