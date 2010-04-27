#ifndef _PARAMSTR_X_H
#define _PARAMSTR_X_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: ParamStrX.h,v 1.1 2010/04/27 12:20:58 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  11/16/04  created
*/

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/util/XMLString.hpp>

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#if defined(XERCES_NEW_IOSTREAMS)
#include <iostream>
#else
#include <iostream.h>
#endif

namespace Parameters {

	//using XERCES_CPP_NAMESPACE_QUALIFIER XMLString;

	// ---------------------------------------------------------------------------
	//  This is a simple class that lets us do easy (though not terribly efficient)
	//  trancoding of XMLCh data to local code page for display.
	// ---------------------------------------------------------------------------
	class StrX
	{
		public :

		// -----------------------------------------------------------------------
		//  Constructors and Destructor
		// -----------------------------------------------------------------------
		StrX(const char* const toTranscode)
		{
			// Call the private transcoding method
			fUnicodeForm = XERCES_CPP_NAMESPACE::XMLString::transcode(toTranscode);
			fLocalForm =  XERCES_CPP_NAMESPACE::XMLString::transcode(fUnicodeForm);
		}

		StrX(const XMLCh * const toTranscode)
		{
			// Call the private transcoding method
			fLocalForm =  XERCES_CPP_NAMESPACE::XMLString::transcode(toTranscode);
			fUnicodeForm = XERCES_CPP_NAMESPACE::XMLString::transcode(fLocalForm);
		}

		~StrX()
		{
			if(NULL != fLocalForm) {
				XERCES_CPP_NAMESPACE::XMLString::release(&fLocalForm);
			}
			if(NULL != fUnicodeForm) {
				XERCES_CPP_NAMESPACE::XMLString::release(&fUnicodeForm);
			}
		}


		// -----------------------------------------------------------------------
		//  Getter methods
		// -----------------------------------------------------------------------
		const char* localForm() const
		{
			return fLocalForm;
		}

		const XMLCh* unicodeForm() const
		{
			return fUnicodeForm;
		}


		private :

		// -----------------------------------------------------------------------
		//  Private data members
		//
		//  fLocalForm
		//      This is the local code page form of the string.
		// -----------------------------------------------------------------------
		char*   fLocalForm;
		XMLCh*  fUnicodeForm;
	};

	inline XERCES_STD_QUALIFIER ostream& operator<<(std::ostream& target, const Parameters::StrX& toDump)
	{
		if(NULL != toDump.localForm()) {
			target << toDump.localForm();
		}
		else if(NULL != toDump.unicodeForm()) {
			target << toDump.unicodeForm();
		}
		return target;
	}
}

#endif /*!_PARAMSTR_X_H*/



