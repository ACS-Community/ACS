#ifndef SMARTSERVANTPOINTER_H
#define SMARTSERVANTPOINTER_H
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
* "@(#) $Id: baciSmartServantPointer.h,v 1.4 2005/01/07 23:41:17 dfugate Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-04-06  created
*/

/** 
 * @file 
 * Header file for BACI Smart Servant Pointers.
 */

#include "lokiSmartPtr.h"

namespace baci 
{

	/**
	* The smart pointer for a CORBA servant
	* The class is derived from Loki::SmartPtr and ensures to call
	* the destroy method when the pointer is deleted
	* 
	* @author <a href=mailto:acaproni@eso.org>Alessandro Caproni</a>
	*/
	template <class T> class CORBAServantSPStorage:
	public Loki::DefaultSPStorage<T>
		{
		public:
		    /** @defgroup StoredTypeSmartPointerTemplate StoredType Pointer (from CORBAServantSPStorage)
		     * The StoredType pointer is a templated typedef so there is no actual inline doc generated for it per-se.
		     *  @{
		     * the type of the pointee_ object
		     */
		    typedef T* StoredType;
		    /** @} */


		CORBAServantSPStorage() : Loki::DefaultSPStorage<T>() 
		{}

		// The storage policy doesn't initialize the stored pointer 
		//     which will be initialized by the OwnershipPolicy's Clone fn
		CORBAServantSPStorage(const CORBAServantSPStorage& st) : 
			Loki::DefaultSPStorage<T>(st)
		{}

		template <class U>
			CORBAServantSPStorage(const CORBAServantSPStorage<U>& st) : 
			Loki::DefaultSPStorage<U>(st) 
                {}
        
		CORBAServantSPStorage(const StoredType& p) : 
			Loki::DefaultSPStorage<T>(p) {}
        

        protected:
            // CORBA Servants cannot be deleted.
            // One must call Destroy()
            void Destroy()
                {
		    if(GetImpl(*this) != 0)
			{
			(*this)->destroy(); 
			}
		}  
        };

	template<class T>
	class SmartServantPointer: public  Loki::SmartPtr<
	    T, 
	    Loki::RefCounted, 
	    Loki::AllowConversion,
	    Loki::AssertCheck,
	    CORBAServantSPStorage
	    >
	{
	  public:
	    SmartServantPointer();
	    SmartServantPointer(T* prop);
	};
	
} // End of namespace baci

#include "baciSmartServantPointer.i"

#endif /*SMARTSERVANTPOINTER_H*/
