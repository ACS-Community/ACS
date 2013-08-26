#ifndef acscourseMount4Impl_h
#define acscourseMount4Impl_h
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
* "@(#) $Id: acscourseMount4Impl.h,v 1.6 2008/10/02 08:58:19 cparedes Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicComponentImpl.h>
#include <baciROdouble.h>
#include <baciRWdouble.h>

///Include the smart pointer for properties
#include <baciSmartPropertyPointer.h>

/**
 *  POA_acsexmplHelloWorld::HelloWorld is obtained from this header file and is
 *  automatically generated from HelloWorld's Interface Definition File 
 *  (i.e., acsexmplHelloWorld.idl) by CORBA.
 */
#include <acscourseMountS.h>
#include "acsThreadManager.h"


/** @file acscourseMount4Impl.h
 */

/** @defgroup ACSCOURSEMOUNTDOC Mount4
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class Mount4 simulates the behaviour of an antenna interface.
&nbsp;It provides only one synchronous methods:
objfix(...). &nbsp;
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::Component IDL interface.</li>
  <li>understanding of synchronous method implementation.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classMount4.html">Mount4 Class Reference</a></li>
  <li><a href="interfaceMOUNT__ACS_1_1Mount4.html">Mount4 IDL Documentation</a></li>
  <li>Mount4 CDB XML Schema</li>
</ul>
</div>
 * @endhtmlonly
 * @}
 */
class Mount4Impl;

class PositionControlThread :public ACS::Thread
{
  public:
    PositionControlThread(const ACE_CString& name, 
		  Mount4Impl *mount_p, 
		  const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
			  const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime);
    ~PositionControlThread();

    virtual void runLoop();

  protected:
    Mount4Impl *mount_p;
};

/** @class Mount4
 * The class Mount4 is a basic example of a component and simulates the behaviour of an antenna interface.
 * It provides one asynchronous methods: objfix.  The methods only writes the data into 
 * virtual properties.
 * @version "@(#) $Id: acscourseMount4Impl.h,v 1.6 2008/10/02 08:58:19 cparedes Exp $"
 */
class Mount4Impl: public virtual baci::CharacteristicComponentImpl,
		  public virtual POA_ACSCOURSE_MOUNT::Mount4,
                  public baci::ActionImplementator
{
  public:
     /**
     * Constructor
     * @param poa Poa which will activate this and also all other components. 
     * @param name component's name. This is also the name that will be used to find the
     * @param containerServices the container services object for this component
     */
    Mount4Impl(const ACE_CString &name, maci::ContainerServices *containerServices);
    
    /**
     * Destructor
     */
    virtual ~Mount4Impl();
    
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
     * <br><hr>
     * @endhtmlonly
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
     * Implementation of asynchronous objfix() method
     * This is the function that actually switches on the Lamp
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
     * <br><hr>
     * @endhtmlonly
     */
    virtual baci::ActionRequest 
    objfixAction (baci::BACIComponent *cob_p, 
	      const int &callbackID,
	      const CBDescIn &descIn, 
	      baci::BACIValue *value_p,
	      Completion &completion, 
	      CBDescOut &descOut);
    
    /**
     * (Pre)sets a new non-moving position for the antenna.
     * The position coordinates are given in azimuth and elevation.
     * The actual az and elev values are written to the properties
     * cmdAz, cmdEl, actAz and actEl.
     *
     * @param az        position azimuth (degree)
     * @param elev      position elevation (degree)
     * @return void
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */    
    virtual void 
    objfix (CORBA::Double az,
	    CORBA::Double elev);

    virtual void 
    objfix_async (
	CORBA::Double az,
	CORBA::Double elev,
	ACS::CBvoid_ptr cb,
	const ACS::CBDescIn &desc);
    
    /**
     * Returns a reference to the cmdAz property
     * Implementation of IDL interface for the property.
     * @return pointer to read-write double property cmdAz
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual ACS::ROdouble_ptr 
    cmdAz ();
    
    /**
     * Returns a reference to the cmdEl property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only write property cmdEl
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual ACS::ROdouble_ptr 
    cmdEl ();
    
    /**
     * Returns a reference to the actAz property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only double property actAz
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */   
    virtual ACS::ROdouble_ptr 
    actAz ();
    
    /**
     * Returns a reference to the actEl property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only double property actEl
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */    
    virtual ACS::ROdouble_ptr 
    actEl ();
    

  protected:
    
    struct __objfix_action {
	double az;
	double elev;
    };

    /**
     *  m_cmdAz_p is the antenna's commanded azimuth
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_cmdAz_sp;

    /**
     *  m_cmdEl_p is the antenna's commanded elevation
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_cmdEl_sp;

    /**
     *  m_actAz_p is the antenna's actual azimuth
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_actAz_sp;

    /**
     *  m_actEl_p is the antenna's actual elevation
     */
    baci::SmartPropertyPointer<baci::ROdouble> m_actEl_sp;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Mount4Impl&);

    /**
     * The PositionControlThread class needs to access
     * the private members of this class, and therefore
     * is given friend status
     */
    friend class PositionControlThread;
};

#endif /*!acscourseMount4Impl_H*/
