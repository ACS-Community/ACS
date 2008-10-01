#ifndef _baciPcontImpl_T_H_
#define _baciPcontImpl_T_H_
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
* "@(#) $Id: baciPcontImpl_T
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    07/02/2003  created
*/

/** 
 * @file 
 * Header file for BACI Continued Property Template Class.
 * This file add some functionality to baciPcommonImpl_T, to manage continued
 * properties, adding some useful characteristics.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPcommonImpl_T.h>

namespace baci {

/**
 * Implementation of P (cont) property
 * @warning We have virtual inheritance from PortableServer::RefCountServantBase
 * because of a bug in gcc 2.95 (see SPR. ALMASW2001075) */
template<ACS_P_C>
class baci_EXPORT PcontImpl:	public virtual POA_SK 
/*				public virtual PcommonImpl<ACS_P_TL> */
{


public:
   
  /**
   * Constuctor
   * @param name property name (e.q. AMSMount:decliantion)
   * @param component_p parent of the property
   */
    PcontImpl(const ACE_CString& name, BACIProperty *property_p, BACIComponent *component_p, DevIO<TM> *devIO/*=0*/, bool flagdeldevIO/*=false*/);

  /**
   * Destructor
   */
  virtual ~PcontImpl();


  /* ----------------------------------------------------------------- */
  /* ---------------------- [ CORBA interface ] ---------------------- */
  /* ----------------------------------------------------------------- */
  
  /* -------------------- [ P interface ] -------------------- */
    
    virtual TS min_delta_trigger ();

    virtual TS graph_min ();
    
    virtual TS graph_max ();
    
    virtual TS min_step ();
    
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

    // P
    TSM	graph_min_m; 			
    TSM	graph_max_m; 			
    TSM	min_step_m; 			
    TSM	min_delta_trig_m; 	
};

// #include "baciPcontImpl_T.i"

 }; 


#endif









