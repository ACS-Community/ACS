#ifndef _baciTypelessPropertyImpl_H
#define _baciTypelessPropertyImpl_H

/*******************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2004 
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
* "@(#) $Id: baciTypelessPropertyImpl.h,v 1.14 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* bjeram    2003/01/21  created

*/

/** 
 * @file 
 * Header file for BACI Typeless Property.
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciPropertyImpl.h>

namespace baci {

class baci_EXPORT TypelessPropertyImpl : public virtual POA_ACS::TypelessProperty,
					public PropertyImpl
{
  public:

    TypelessPropertyImpl(const ACE_CString& name, BACIComponent* component_p);

    ~TypelessPropertyImpl();

/* ------------------ [ TypelessProperty interface ] ------------------ */
    virtual char * description ();
	
    virtual char * format ();
    virtual CORBA::Boolean initialize_devio ();
 
    virtual char * units ();
    
    virtual ACS::pattern resolution ();
    
  protected:
    /**
     * Read characteristics from CDB
     * @param propertyName name of the property whose characteristics to read
     * @return true on success, false on failure
     */
    virtual bool readCharacteristics();

  private:

 // Characteristics Typeless property
  bool initializeDevIO_m;
  ACE_CString  desc_mription; 			
  ACE_CString  format_m; 				
  ACE_CString  units_m; 				
  ACS::pattern resolution_m; 		

}; //class TypelessPropertyImpl

 }; 

#endif










