#ifndef _baciPpatternImpl_H_
#define _baciPpatternImpl_H_

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2003 
*
*This library is free software; you can redistribute it and/or
*modify it under the terms of the GNU Lesser General Public
*License as published by the Free Software Foundation; either
*version 2.1 of the License, or (at your option) any later version.
*
*This library is distributed in the hope that it will be useful,
*but WITHOUT ANY WARRANTY; without even the implied warranty of
*MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
*Lesser General Public License for more details.
*
*You should have received a copy of the GNU Lesser General Public
*License along with this library; if not, write to the Free Software
*Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: baciPpatternImpl.h,v 1.19 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram  2003/02/20    created
*/

/** 
 * @file 
 * Header file for BACI PPattern Template Class.
 * Add some functionality to baciPcommonImpl_T to manage pattern
 * properties (like strings), adding some useful characteristics.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciMonitor_T.h>
#include <baciPcommonImpl_T.h>

namespace baci {

/** @defgroup MonitorpatternTemplate Monitorpattern Class
 * The Monitorpattern class is a templated typedef so there is no actual inline doc generated for it per-se.
 *  @{
 * The Monitorpattern class is an implementation of the ACS::Monitorpattern IDL interface.
 */
typedef  Monitor<ACS_MONITOR(pattern, ACS::pattern)> Monitorpattern;
/** @} */

/**
 * Implementation of Ppattern property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075)
 */

class baci_EXPORT PpatternImpl : public virtual POA_ACS::Ppattern
{
    
  public:
    /**
     * Constuctor
     * @param name property name (e.q. AMSMount:decliantion)
     */
    PpatternImpl(const ACE_CString& name, BACIProperty *property_p /*, BACIComponent *component_p, DevIO<ACS::pattern>* devIO=0*/); 
    /**
     * Destructor
     */
    virtual ~PpatternImpl();
    
    
    /* ----------------------------------------------------------------- */
    /* ---------------------- [ CORBA interface ] ---------------------- */
    /* ----------------------------------------------------------------- */
    /* -------------------- [ Ppattern interface ] -------------------- */
    
    virtual ACS::stringSeq * bitDescription ();
    
    virtual ACS::ConditionSeq * whenSet ();
    
    virtual ACS::ConditionSeq * whenCleared ();
    
    
  protected:
    
    /**
     * Read characteristics from CDB
     * @param propertyName name of the property whose characteristics to read
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics(CharacteristicModelImpl *model);
    
    
  private:
    
    ///
    /// Characteristics
    ///
    
    // Ppattern			
    
    ACS::stringSeq    bitDescription_m;
    ACS::ConditionSeq whenSet_m;	
    ACS::ConditionSeq whenCleared_m;
    
};

 }; 

#endif  /* baciPpatternImpl */






