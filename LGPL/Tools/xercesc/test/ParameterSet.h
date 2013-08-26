#ifndef _PARAMETER_SET_H
#define _PARAMETER_SET_H
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
* "@(#) $Id: ParameterSet.h,v 1.2 2008/07/25 07:52:19 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  09/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <memory>
#include <string.h>
#include <stdexcept>
#include <stdlib.h>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <StrX.h>

XERCES_CPP_NAMESPACE_USE

/** @file ParameterSet.h */

/**
 * ParameterSet class used to support OFFLINE tasks
 */
class ParameterSet
{    
	public:

		/********************** Constructors/Destructors ***************************/

		/**
		 * Constructor
		 * @param xmlFile the name of the file containing the XML document defining the parameter set
		 */
		ParameterSet(const char* xmlFile);

		/**
		 * Destructor
		 */
		virtual ~ParameterSet();


	private:

		int parseInputString(const char* parameterSet);
		int parseDOM(const char* xmlParamSet);
		void processParamNodes(DOMNodeList *paramNodes);

		// Constants
		XMLCh* PARAMETER_TAG_NAME;
		XMLCh* NAME_TAG_NAME;
		XMLCh* VALUE_TAG_NAME;
		XMLCh* UNITS_TAG_NAME;

		XMLCh* INT_PARAM_TYPE;
		XMLCh* DOUBLE_PARAM_TYPE;
		XMLCh* STRING_PARAM_TYPE;
		XMLCh* BOOL_PARAM_TYPE;
		XMLCh* INT_ARRAY_PARAM_TYPE;
		XMLCh* DOUBLE_ARRAY_PARAM_TYPE;
		XMLCh* STRING_ARRAY_PARAM_TYPE;

};
#endif /*!_PARAMETER_SET_H*/
