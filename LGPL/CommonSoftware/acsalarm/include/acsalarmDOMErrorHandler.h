#ifndef _ACSALARMDOMERROR_HANDLER_H
#define _ACSALARMDOMERROR_HANDLER_H

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
* "@(#) $Id: acsalarmDOMErrorHandler.h,v 1.1 2010/04/27 12:06:51 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <xercesc/util/XercesDefs.hpp>
#include <xercesc/dom/DOMError.hpp>
#include <xercesc/dom/DOMErrorHandler.hpp>
#include <iostream>

/** @file acsalarmDOMErrorHandler.h */

//using XERCES_CPP_NAMESPACE_QUALIFIER DOMErrorHandler;
//using XERCES_CPP_NAMESPACE_QUALIFIER DOMError;

namespace acsalarm {

	/**
	 * acsDOMErrorHandler class - class to handle DOM parsing errors
	 */
	class acsDOMErrorHandler : public XERCES_CPP_NAMESPACE::DOMErrorHandler
	{ 			
	   public: 		
	    /**
	     * Constructor
	     */
	    acsDOMErrorHandler();
	    
	    /**
	     * Destructor
	     */
	    virtual ~acsDOMErrorHandler();

	    virtual bool handleError(const XERCES_CPP_NAMESPACE::DOMError& domError);
	}; 	

}
#endif /*!_ACSALARMDOMERROR_HANDLER_H*/

