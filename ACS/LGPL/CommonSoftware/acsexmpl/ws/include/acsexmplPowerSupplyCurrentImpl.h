#ifndef acsexmplPowerSupplyCurrentImpl_h
#define acsexmplPowerSupplyCurrentImpl_h
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
* "@(#) $Id: acsexmplPowerSupplyCurrentImpl.h,v 1.88 2008/07/25 07:37:04 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* david 2002-07-02 added GNU license info
* gchiozzi 2002-03-04 Instead of writing in the CDB, now gets a reference to readback and calls set_sync().
* msekoran 2002-02-15 created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsexmplExport.h>

///Includes for each baci::BACI property used in this example
#include <baciRWdouble.h>
#include <baciROdouble.h>

/** @file acsexmplPowerSupplyCurrentImpl.h
 */

/** @addtogroup ACSEXMPLTOC
*/
/*@{
*/

/** @addtogroup ACSEXMPLTOMISCELLANEOUS
*/
/*@{
*/

/** @defgroup ACSEXMPLPSCURRENTDOC Power Supply Current
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 PowerSupplyCurrent is provided to show how flexible baci::BACI properties can be.  You 
 can also gain a litle insight into how properties are implemented in ACS by 
 reviewing this example.
 <br>
 <br>
 <h2>What can I gain from this example?</h2>
 <ul>
   <li>an example which overrides the ACS implementation of a baci::BACI property.</li>
   <li>writing values to read-only baci::BACI properties by using the property's underlying DevIO instance.</li>
   <li>limited ACS logging macros.</li>
 </ul>
 <br>
 <br>
 <h2>Links</h2>
 <ul>
   <li><a href="classPowerSupplyCurrent.html">Power Supply Current Class Reference</a></li>
 </ul>
 <br>
 <br>
 </div>
   @endhtmlonly
 * @}
 */

/* @}*/
/* @}*/

/**
 * This class provides simulation of PowerSupply current/readback interraction.
 * It extends the basic baci::BACI baci::RWdouble by accepting at construction time the reference 
 * to a baci::ROdouble that represents the actual readback value of the current.
 * It overrides the setValue method so that it sets at the same time the value of
 * the property itself AND the readback propery.
 *
 * @author <a href=mailto:matej.sekoranja@ijs.si>Matej Sekoranja</a>,
 * Jozef Stefan Institute, Slovenia<br>
 * @version "@(#) $Id: acsexmplPowerSupplyCurrentImpl.h,v 1.88 2008/07/25 07:37:04 cparedes Exp $"
 */
class acsexmpl_EXPORT PowerSupplyCurrent : public baci::RWdouble
{
  public:   
    /**
     * Constuctor
     * @param name property name (e.q. PowerSupply:m_current_p)
     * This is also the name that will be used to find the
     * configuration data for the property in the Configuration Database.
     * @param component_p parent of the property
     * @param readback_p reference to the associated readback property
     */
    PowerSupplyCurrent (const ACE_CString &name, 
			baci::BACIComponent *component_p, 
			baci::ROdouble *readback_p);
    
    /**
     * Set value method (value mutator)
     * We add this to override the setValue method inherited from baci::RWdouble.  We do 
     * this to make the baci::RW property symetric to the baci::RO property passed to the constructor.
     * @param property_p property which requested value
     * @param value_p value to be returned
     * @param completion error handling structure
     * @param descOut callback descriptor
     * @htmlonly
       <br><hr>
       @endhtmlonly
     */
    virtual void 
    setValue (baci::BACIProperty *property_p,
	      baci::BACIValue *value_p, 
	      Completion &completion,
	      CBDescOut &descOut);
    
  private:
    /** 
     * m_readback_p is never created in this class.
     * It just points to the baci::ROdouble passed to this class's constructor.  This is done so 
     * that we can synchronize this class and the baci::ROdouble's value. 
     */
    baci::ROdouble *m_readback_p;

    /**
     * ALMA C++ coding standards state copy operators should be disabled.
     */
    void operator=(const PowerSupplyCurrent&);
};

#endif   /* acsexmplPowerSupplyCurrentImpl_h */


