#ifndef baciPropertyImpl_H
#define baciPropertyImpl_H

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
* "@(#) $Id: baciPropertyImpl.h,v 1.14 2008/10/01 02:26:45 cparedes Exp $"
*
* who       when        what
* --------  ----------  ----------------------------------------------
* acaproni  2004-04-07  added getComponent 
* bjeram    2003/01/21  created

*/

/** 
 * @file 
 * Header file for BACI Property Template Class.
 * 
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baciCharacteristicModelImpl.h>

namespace baci {

class baci_EXPORT PropertyImpl : public virtual POA_ACS::Property,
				 public CharacteristicModelImpl
{
  public:
    
    PropertyImpl(const ACE_CString& name, BACIComponent* component_p);

    virtual ~PropertyImpl();

/* ------------------ [ Property interface ] ------------------ */
    virtual char * name (
        ACE_ENV_SINGLE_ARG_DECL_WITH_DEFAULTS
      );
 
    virtual char * characteristic_component_name ();
    
	///Return the BACI Component    
	BACIComponent* getComponent() { return component_mp; }
	
	
private:
  /// BACI Component
  BACIComponent* component_mp;
}; //class PropertyImpl

 }; 

#endif







