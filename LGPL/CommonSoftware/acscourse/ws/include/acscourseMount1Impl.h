#ifndef acscourseMount1Impl_h
#define acscourseMount1Impl_h
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
* "@(#) $Id: acscourseMount1Impl.h,v 1.4 2008/10/02 08:58:19 cparedes Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acscomponentImpl.h>

/**
 *  POA_acsexmplHelloWorld::HelloWorld is obtained from this header file and is
 *  automatically generated from HelloWorld's Interface Definition File 
 *  (i.e., acsexmplHelloWorld.idl) by CORBA.
 */
#include <acscourseMountS.h>


/** @file acscourseMount1Impl.h
 */

/** @defgroup ACSCOURSEMOUNTDOC Mount1
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class Mount1 simulates the behaviour of an antenna interface.
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
  <li><a href="classMount1.html">Mount1 Class Reference</a></li>
  <li><a href="interfaceMOUNT__ACS_1_1Mount1.html">Mount1 IDL Documentation</a></li>
  <li>Mount1 CDB XML Schema</li>
</ul>
</div>
 * @endhtmlonly
 * @}
 */

/** @class Mount1
 * The class Mount1 is a basic example of a component and simulates the behaviour of an antenna interface.
 * It provides one asynchronous methods: objfix.  The methods only writes the data into 
 * virtual properties.
 * @version "@(#) $Id: acscourseMount1Impl.h,v 1.4 2008/10/02 08:58:19 cparedes Exp $"
 */
class Mount1Impl: public virtual acscomponent::ACSComponentImpl,
		  public virtual POA_ACSCOURSE_MOUNT::Mount1
{
  public:
     /**
     * Constructor
     * @param name component's name. This is also the name that will be used to find the
     * @param containerServices the container services object for this component
     */
    Mount1Impl(const ACE_CString &name, maci::ContainerServices *containerServices);
    
    /**
     * Destructor
     */
    virtual ~Mount1Impl();
    
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

  private:
    
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Mount1Impl&);
};

#endif /*!acscourseMount1Impl_H*/
