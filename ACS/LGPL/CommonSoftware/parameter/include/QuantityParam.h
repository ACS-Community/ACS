#ifndef _QUANTITY_PARAM_H
#define _QUANTITY_PARAM_H
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
* "@(#) $Id: QuantityParam.h,v 1.2 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  08/15/05  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <Param.h>
#include <string>
#include <memory>

using std::string;
using std::auto_ptr;

/** @file QuantityParam.h */

namespace Parameters {

	/**
	 * QuantityParam class - abstract base class used to store information about params having units 
	 * (e.g. Mhz, cm, seconds, etc.) within a ParameterSetDef as defined by the task author of a 
	 * particular OFFLINE task.
	 */
	class QuantityParam : public Parameters::Param
	{    
		public:

			/**
			 * Constructor
			 */
			QuantityParam();

			/**
			 * Constructor
			 * @param name the name of the parameter
			 * @unitsVal units for the quantity, if any
			 */
			QuantityParam(const string & nameVal, auto_ptr<string> unitsVal);
	    
			/**
			 * Destructor
			 */
			virtual ~QuantityParam() = 0;

			/*
			 * Accessor for the units.
			 * @return the units as an auto_ptr to a string
			 * if defined for this pdef, then the auto_ptr will 
			 * point to something meaningful, else null.
			 */
			virtual auto_ptr< string > getUnits();

			/*
			 * Accessor for flag indicating if there are units.
			 * @return the flag indicating if there are units.
			 */
			virtual bool getHasUnits();

			/**
 		 	 * Returns an XML string representation of the param.
 			 * @return the param as an XML string
 			 * NOTE: the XML is not a complete document, just a fragment.
 			 */
			virtual string toString();

		protected:
			string units_m;
			bool hasUnits_m;
	};
}
#endif /*!_QUANTITY_PARAM_H*/



