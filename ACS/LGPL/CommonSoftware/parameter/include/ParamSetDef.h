#ifndef _PARAM_SET_H
#define _PARAM_SET_H
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
* "@(#) $Id: ParamSetDef.h,v 1.10 2010/04/27 12:20:58 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/26/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <map>
#include <vector>
#include <string.h>
#include <stdexcept>
#include <stdlib.h>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/dom/DOMBuilder.hpp>
#include <xercesc/util/PlatformUtils.hpp>

#include <IntParamDef.h>
#include <IntArrayParamDef.h>
#include <DoubleParamDef.h>
#include <DoubleArrayParamDef.h>
#include <StringParamDef.h>
#include <StringArrayParamDef.h>
#include <BoolParamDef.h>
#include <ParamStrX.h>
#include <parameterConstants.h>

using std::auto_ptr;
using std::domain_error;
using std::map;
using std::string;
using std::vector;
using XERCES_CPP_NAMESPACE_QUALIFIER DOMNodeList;
using XERCES_CPP_NAMESPACE_QUALIFIER DOMElement;
using XERCES_CPP_NAMESPACE_QUALIFIER DOMBuilder;

/** @file ParamSetDef.h */

namespace Parameters {

	/**
	 * ParamSetDef class used to support OFFLINE tasks
	 */
	class ParamSetDef
	{    
		public:
		
		enum paramTypesEnum { BOOL, DOUBLE, DOUBLE_ARRAY, INT, INT_ARRAY, STRING, STRING_ARRAY};

		/**
		 * Constructor
		 * @param xmlParamSetDef the parameter set definition as an XML string
		 */
		ParamSetDef(const string & xmlParamSetDef);
		
		/**
		 * Destructor
		 */
		virtual ~ParamSetDef();

		/**
		 * Gets the type of a parameter by name.
		 * @param paramName the name of the parameter for which the type is desired.
         * @throw domain_error
		 * @return the type as a ParamSetDef::paramType enum.
		 */
		paramTypesEnum getParamTypeForName(string paramName);

		/**
		 * get a bool param by name.
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		BoolParamDef getBoolParamDef(string paramName);
		
		/**
		 * get an int param by name.
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		IntParamDef getIntParamDef(string paramName);

		/**
		 * get a double param by name.
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		DoubleParamDef getDoubleParamDef(string paramName);

		/**
		 * get a string param by name.
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		StringParamDef getStringParamDef(string paramName);

		/**
		 * get an array of int params by name
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		IntArrayParamDef getIntArrayParamDef(string paramName);

		/**
		 * get an array of double params by name
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		DoubleArrayParamDef getDoubleArrayParamDef(string paramName);

		/**
		 * get an array of string params by name
		 * @param paramName the name of the parameter desired.
         * @throw domain_error
		 */
		StringArrayParamDef getStringArrayParamDef(string paramName);

		/**
		 * gets all the bool param defs for this psetdef
		 */
		auto_ptr< vector<BoolParamDef> > getBoolParamDefs();

		/**
		* gets all the int param defs for this psetdef
		*/
		auto_ptr< vector<IntParamDef> > getIntParamDefs();

		/**
		* gets all the string param defs for this psetdef
		*/
		auto_ptr< vector<StringParamDef> > getStringParamDefs();

		/**
		* gets all the double param defs for this psetdef
		*/
		auto_ptr< vector<DoubleParamDef> > getDoubleParamDefs();

		/**
		* gets all the int array param defs for this psetdef
		*/
		auto_ptr< vector<IntArrayParamDef> > getIntArrayParamDefs();

		/**
		* gets all the double array param defs for this psetdef
		*/
		auto_ptr< vector<DoubleArrayParamDef> > getDoubleArrayParamDefs();

		/**
		* gets all the string array param defs for this psetdef
		*/
		auto_ptr< vector<StringArrayParamDef> > getStringArrayParamDefs();

		
		private:

		int parseFile(const string & xmlFile);
		int parseDOM(const string & xmlFile);
		int parseSAX(const string & xmlFile);
        /**
         * @throw domain_error
         */
		void processParamDefNodes(DOMNodeList *paramNodes);
        /**
         * @throw domain_error
         */
		void handleBoolParamDef(DOMElement *paramElem);
        /**
         * @throw domain_error
         */
		void handleIntParamDef(DOMElement *paramElem);
        /**
         * @throw domain_error
         */
		void handleIntArrayParamDef(DOMElement *paramElem);
        /**
         * @throw domain_error
         */
		void handleDoubleParamDef(DOMElement *paramElem);
        /**
         * @throw domain_error
         */
		void handleDoubleArrayParamDef(DOMElement *paramElem);
        /**
         * @throw domain_error
         */
		void handleStringParamDef(DOMElement *paramElem);
        /**
         * @throw domain_error
         */
		void handleStringArrayParamDef(DOMElement *paramElem);
		void setSchemaLocation(DOMBuilder * parser);

		map<string, BoolParamDef> boolParamDefMap;
		map<string, IntParamDef> intParamDefMap;
		map<string, DoubleParamDef> doubleParamDefMap;
		map<string, StringParamDef> stringParamDefMap;
		map<string, IntArrayParamDef> intArrayParamDefMap;
		map<string, DoubleArrayParamDef> doubleArrayParamDefMap;
		map<string, StringArrayParamDef> stringArrayParamDefMap;

		// Constants
		XMLCh* PARAMETER_TAG_NAME;
		XMLCh* NAME_TAG_NAME;
		XMLCh* REQUIRED_TAG_NAME;
		XMLCh* PROMPT_TAG_NAME;
		XMLCh* HELP_TAG_NAME;
		XMLCh* DEFAULT_TAG_NAME;
		XMLCh* STRING_DEFAULT_TAG_NAME;
		XMLCh* LENGTH_TAG_NAME;
		XMLCh* VALID_VALUES_TAG_NAME;
		XMLCh* VALUE_TAG_NAME;
		XMLCh* MAX_TAG_NAME;
		XMLCh* MIN_TAG_NAME;
		XMLCh* UNITS_TAG_NAME;
		XMLCh* MAXLEN_TAG_NAME;

		XMLCh* INT_PARAM_TYPE;
		XMLCh* DOUBLE_PARAM_TYPE;
		XMLCh* STRING_PARAM_TYPE;
		XMLCh* BOOL_PARAM_TYPE;
		XMLCh* INT_ARRAY_PARAM_TYPE;
		XMLCh* DOUBLE_ARRAY_PARAM_TYPE;
		XMLCh* STRING_ARRAY_PARAM_TYPE;
	};
}
#endif /*!_PARAM_SET_H*/


