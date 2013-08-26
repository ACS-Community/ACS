#ifndef _MYDOMERROR_HANDLER_H
#define _MYDOMERROR_HANDLER_H

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
* "@(#) $Id: MyDOMErrorHandler.h,v 1.2 2008/07/25 07:52:19 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <xercesc/dom/DOMError.hpp>
#include <xercesc/dom/DOMErrorHandler.hpp>
#include <iostream>

/** @file MyDOMErrorHandler.h */

XERCES_CPP_NAMESPACE_USE

/**
 * MyDOMErrorHandler class - class to handle DOM parsing errors
 */
class MyDOMErrorHandler : public DOMErrorHandler
{ 			
   public: 		
    /**
     * Constructor
     */
    MyDOMErrorHandler();
    
    /**
     * Destructor
     */
    virtual ~MyDOMErrorHandler();

    virtual bool handleError(const DOMError& domError);
}; 	

#endif /*!_MYDOMERROR_HANDLER_H*/
