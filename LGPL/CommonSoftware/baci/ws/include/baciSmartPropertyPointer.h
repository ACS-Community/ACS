#ifndef SMARTPROPERTYPOINTER_H
#define SMARTPROPERTYPOINTER_H
/*******************************************************************************
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
* "@(#) $Id: baciSmartPropertyPointer.h,v 1.4 2005/01/15 00:20:38 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-04-06  created
*/

/** 
 * @file 
 * Header file for BACI Smart Property Pointers.
 */

#include <baciCharacteristicComponentImpl.h>

#include <baciSmartServantPointer.h>

namespace baci {

/** 
 * The smart pointer for a property
 * It is derived form the SmartServantProperty class to ensure to destroy
 * the object when it is deleted.
 */
    template<class T>
    class SmartPropertyPointer: public SmartServantPointer<T> {
      private:

	/// The Characteristic Component that owns the property
	CharacteristicComponentImpl* component_mp;

      public:
	/**
	 * Constructor
	 * 
	 * @htmlonly
	 * <BR><HR>
	 * @endhtmlonly
	 */
	SmartPropertyPointer(CharacteristicComponentImpl* component_p);
	
	/**
	 * Constructor
	 * @param prop The pointer to the property
	 * 
	 * @htmlonly
	 * <BR><HR>
	 * @endhtmlonly
	 */
	SmartPropertyPointer(T* prop,CharacteristicComponentImpl* component_p);
			
	/**
	 * Destructor
	 *  @htmlonly
	 * <BR><HR>
	 * @endhtmlonly
	 */
	~SmartPropertyPointer();
		
	/** 
	 * Redefine the equal operator initing adding the property
	 * to the descriptor struct of the Characteristic Component
	 */
	SmartPropertyPointer<T>* operator=(const T*);
	
      private:
	/**
	 * Declared to inhibit the usage of the smart pointer
	 * without passing the component
	 */
	SmartPropertyPointer();

	/**
	 * Add the characteristics of the property to the component
	 * It calls the addPropertyDesc method of the Characteristic Component
	 */
	void init();

    };
} // End of namespace baci

#include <baciSmartPropertyPointer.i>


#endif /*SMARTPROPERTYPOINTER_H*/
