#ifndef acscourseMount2Impl_h
#define acscourseMount2Impl_h
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
* "@(#) $Id: acscourseMount2Impl.h,v 1.6 2008/10/02 08:58:19 cparedes Exp $"
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


/** @file acscourseMount2Impl.h
 */

/** @defgroup ACSCOURSEMOUNTDOC Mount2
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class Mount2 simulates the behaviour of an antenna interface.
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
  <li><a href="classMount2.html">Mount2 Class Reference</a></li>
  <li><a href="interfaceMOUNT__ACS_1_1Mount2.html">Mount2 IDL Documentation</a></li>
  <li>Mount2 CDB XML Schema</li>
</ul>
</div>
 * @endhtmlonly
 * @}
 */

/** @class Mount2
 * The class Mount2 is a basic example of a component and simulates the behaviour of an antenna interface.
 * It provides one asynchronous methods: objfix.  The methods only writes the data into 
 * virtual properties.
 * @version "@(#) $Id: acscourseMount2Impl.h,v 1.6 2008/10/02 08:58:19 cparedes Exp $"
 */
class Mount2Impl: public virtual baci::CharacteristicComponentImpl,
		  public virtual POA_ACSCOURSE_MOUNT::Mount2
{
  public:
     /**
     * Constructor
     * @param name component's name. This is also the name that will be used to find the
     * @param containerServices the container services object for this component
     */
    Mount2Impl(const ACE_CString &name, maci::ContainerServices *containerServices);
    
    /**
     * Destructor
     */
    virtual ~Mount2Impl();
    
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

    /**
     * Returns a reference to the cmdAz property
     * Implementation of IDL interface for the property.
     * @return pointer to read-write double property cmdAz
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */ 
    virtual ACS::RWdouble_ptr 
    cmdAz ();
    
    /**
     * Returns a reference to the cmdEl property
     * Implementation of IDL interface for the property.
     * @return pointer to read-only write property cmdEl
     * @htmlonly
     * <br><hr>
     * @endhtmlonly
     */
    virtual ACS::RWdouble_ptr 
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
    
    /**
     *  m_cmdAz_p is the antenna's commanded azimuth
     */
    baci::SmartPropertyPointer<baci::RWdouble> m_cmdAz_sp;

    /**
     *  m_cmdEl_p is the antenna's commanded elevation
     */
    baci::SmartPropertyPointer<baci::RWdouble> m_cmdEl_sp;

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
    void operator=(const Mount2Impl&);
};

#endif /*!acscourseMount2Impl_H*/
