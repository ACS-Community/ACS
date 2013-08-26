#ifndef ACSEXMPLASYNCCALLBACKS_H
#define ACSEXMPLASYNCCALLBACKS_H
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
* "@(#) $Id: acsexmplAsyncCallbacks.h,v 1.2 2008/10/01 04:30:47 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2004-08-10  created
*/

// We need CORBA stuffs for baci classes
#include <baciS.h>

#include <ace/SString.h>

/** 
 * A redefinition of the CBdouble class to check how it works
 * It is specialized for our needs (in this demo it only prints 
 * log messages)
 */
class AsyncCBdouble: public virtual POA_ACS::CBdouble  {
	private:
		// The name of the property
		ACE_CString propertyName;
		
		// The variable to update when the asynchronous call terminates
		// It is a pointer to a variable of the main program to show 
		// how to pass information between the callback and the rest
		// of the world
		double* varToUpdate;

	public:
		/**
		 * Constructor
		 * @param property The name of the property
		 * @param var The poinetr to the variable in the main program
		 */
		 AsyncCBdouble(ACE_CString property, double* var) {
		 	propertyName=property;
		 	varToUpdate=var;
		 }
		 
		 /** 
		  * Destructor (nothing to do here)
		  */
		  ~AsyncCBdouble() { }
		  
		void working (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc);
		
		void done (CORBA::Double value, const ACSErr::Completion &c, const ACS::CBDescOut &desc);
			
		CORBA::Boolean negotiate (ACS::TimeInterval time_to_transmit, const ACS::CBDescOut &desc); 
};

#endif
