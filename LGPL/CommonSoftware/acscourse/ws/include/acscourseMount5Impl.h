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
* "@(#) $Id: acscourseMount5Impl.h,v 1.6 2008/10/02 08:58:19 cparedes Exp $"
*
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicComponentImpl.h>
#include <baciROdouble.h>
#include <baciRWdouble.h>
#include <baciSmartPropertyPointer.h>

#include <acscourseMountS.h>

///In this example, events are published to a channel implying we need this header
#include <acsncSimpleSupplier.h>
///In this example, events are also consumed implying we need this header
#include <acsncSimpleConsumer.h>


/** @file acscourseMount5Impl.h
 */

/** @defgroup ACSCOURSEMOUNT5DOC Mount5
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
The class Mount5 is identical to Mount1 in nearly all aspects except:
<ul>
  <li>it publishes and consumes data from an event channel using the ACS event channel API.</li>
  <li>exception handling is not used.</li>
</ul>
<br>
<br>
<h2>What can I gain from this example?</h2>
<ul>
  <li>an example derived from the ACS::Component IDL interface.</li>
  <li>understanding of synchronous method implementation.</li>
  <li>publishing events (in the form of user-defined IDL structs) to consumers.</li>
  <li>processing events (in the form of user-defined IDL structs) sent by suppliers.</li>
</ul>
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classMount5.html">Mount5 Class Reference</a></li>
  <li><a href="interfaceMOUNT__ACS_1_1Mount5.html">Mount5 IDL Documentation</a></li>
</ul>
</div>
 * @endhtmlonly
 * @}
 */

/** @class Mount5
 * The class Mount5 is identical to Mount1 except that in this case, it supplies/consumes
 * events from an event channel.
 * @version "@(#) $Id: acscourseMount5Impl.h,v 1.6 2008/10/02 08:58:19 cparedes Exp $"
 */
class Mount5Impl: public virtual baci::CharacteristicComponentImpl,
		  public virtual POA_ACSCOURSE_MOUNT::Mount5
{
  public:
     /**
     * Constructor
     * Nearly identical to the Mount1 C++ class's implementation except that 
     * it instantiates both event supplier and consumer objects.
     * @param name component's name.
     * @param containerServices the container services object for this component
     */
    Mount5Impl(const ACE_CString &name, maci::ContainerServices *containerServices);
    
    /**
     * Destructor
     * Nearly identical to the Mount1 C++ class's implementation except that 
     * it destroys the event supplier and consumer objects.
     */
    virtual ~Mount5Impl();
    
    /**
     * (Pre)sets a new non-moving position for the antenna.
     * The position coordinates are given in azimuth and elevation.
     * The actual az and elev values are written to the properties
     * cmdAz, cmdEl, actAz and actEl.
     *
     * @param az        position azimuth (degree)
     * @param elev      position elevation (degree)
     * @throw ACSErrTypeACSCourse::TargetOutOfRangeEx
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


  private:

    /**
     *  This is the Supplier class used to publish data to the event channel.
     */
    nc::SimpleSupplier *m_MountSupplier_p;


    /**
     *  This is the Consumer class used to consumer data from the event channel.
     *  The templated parameter is the type of event that will be subscribed
     *  to.
     */
    nc::SimpleConsumer<ACSCOURSE_MOUNT::MountEventData> *m_simpConsumer_p;
    
    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const Mount5Impl&);
};

#endif /*!acscourseMount5Impl_H*/
