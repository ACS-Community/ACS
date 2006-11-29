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
* "@(#) $Id: ParameterSet.h,v 1.10 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  09/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <map>
#include <memory>
#include <string.h>
#include <stdexcept>
#include <stdlib.h>
#include <xercesc/dom/DOMBuilder.hpp>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMNode.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <StrX.h>
#include <InMemoryXmlData.h>
#include <ParamSetDef.h>
#include <BoolParam.h>
#include <IntParam.h>
#include <IntArrayParam.h>
#include <DoubleArrayParam.h>
#include <DoubleParam.h>
#include <StringParam.h>
#include <StringArrayParam.h>
#include <parameterConstants.h>

using std::auto_ptr;
using std::domain_error;
using std::invalid_argument;
using std::map;
using std::string;
using XERCES_CPP_NAMESPACE_QUALIFIER DOMElement;
using XERCES_CPP_NAMESPACE_QUALIFIER DOMNodeList;
using XERCES_CPP_NAMESPACE_QUALIFIER DOMBuilder;

/** @file ParameterSet.h */

namespace Parameters {

	/**
	 * ParameterSet class used to support OFFLINE tasks
	 */
	class ParameterSet
	{    
		public:
			/********************** Constructors/Destructors ***************************/

			/**
			 * Constructor
			 * @param xmlFileName the name of the file containing the XML document defining the parameter set
			 * Note: the file name must exist on the machine where the Task component is running. If this is 
			 * not the case, the file name will be meaningless and you should, instead, use the other constructor, 
			 * below, deals with the data solely in memory. 
			 */
			ParameterSet(string xmlFileName);

			/**
			 * Constructor
			 * @param InMemoryXmlData contains the XML data (as a InMemoryXmlData object) for the parameter set for a task.
			 */
			ParameterSet(InMemoryXmlData  * fileInfo);

			/**
			 * Destructor
			 */
			virtual ~ParameterSet();

			/*************************** getters ***************************************/

			/**
			 * get an IntParam by name.
			 * @param paramName the name of the parameter desired.
			 */
			IntParam getIntParam(string paramName) throw(domain_error);

			/**
			 * get a double param by name.
			 * @param paramName the name of the parameter desired.
			 */
			DoubleParam getDoubleParam(string paramName) throw(domain_error);

			/**
			 * get a string param by name.
			 * @param paramName the name of the parameter desired.
			 */
			StringParam getStringParam(string paramName) throw(domain_error);

			/**
			 * get a bool param by name.
			 * @param paramName the name of the parameter desired.
			 */
			BoolParam  getBoolParam(string paramName) throw(domain_error);

			/**
			 * get an array of int params by name 
			 * @param paramName the name of the parameter desired.
			 */
			IntArrayParam getIntArrayParam(string paramName) throw(domain_error);

			/**
			 * get an array of double params by name
			 * @param paramName the name of the parameter desired.
			 */
			DoubleArrayParam getDoubleArrayParam(string paramName) throw(domain_error);

			/**
			 * get an array of string params by name
			 * @param paramName the name of the parameter desired.
			 */
			StringArrayParam getStringArrayParam(string paramName) throw(domain_error);

			/*************************** setters ***************************************/

			/**
			 * set an int param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, IntParam value);

			/**
			 * set a double param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, DoubleParam value);

			/**
			 * set a string param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, StringParam value);

			/**
			 * set a bool param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, BoolParam value);

			/**
			 * set an int array param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, IntArrayParam value);

			/**
			 * set a double array param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, DoubleArrayParam value);

			/**
			 * set a string array param by name.
			 * @param paramName the name of the parameter to be set.
			 * @param value the value of the parameter.
			 */
			void setParam(string paramName, StringArrayParam value);

			/**
			 * Returns the ParamSetDef, i.e. the parameter set definition for the ParameterSet. From
			 * the ParamSetDef, you can get things like the "help" and "prompt" text, etc.
			 */
			ParamSetDef* getParamSetDef();		

			/**
			 * Returns the name of the parameter set as a string.
			 */
			string getName();

			/**
			 * Converts the ParameterSet in-memory representation to an XML string.
			 */
			string toString();

		private:

			/**
			 * Sets the name for the parameter set.
			 */
			void setName(string psetName);

			/**
			 * Returns the file name for the psetdef associated with this ParameterSet,
			 * as defined by the <psetdef> tag in the ParameterSet XML instance document.
			 */
			string getParamSetDefFileName();

			/**
			 * Validates the parameter set
			 */
			void validate() throw(invalid_argument);

			/**
			 * Validates the bool params
			 */
			void validateBoolParams() throw(invalid_argument);
			void validateBoolParam(BoolParamDef pDef) throw(invalid_argument);

			/**
			 * Validates the int parameters 
			 */
			void validateIntParams() throw(invalid_argument);
			void validateIntParam(IntParamDef pDef) throw(invalid_argument);

			/**
			 * Validates the double parameters 
			 */
			void validateDoubleParams() throw(invalid_argument);
			void validateDoubleParam(DoubleParamDef pDef) throw(invalid_argument);

			/**
			 * Validates the string parameters 
			 */
			void validateStringParams() throw(invalid_argument);
			void validateStringParam(StringParamDef pDef) throw(invalid_argument);

			/**
			 * Validates the int array parameters 
			 */
			void validateIntArrayParams() throw(invalid_argument);
			void validateIntArrayParam(IntArrayParamDef pDef) throw(invalid_argument);

			/**
			 * Validates the string array parameters 
			 */
			void validateStringArrayParams() throw(invalid_argument);
			void validateStringArrayParam(StringArrayParamDef pDef) throw(invalid_argument);

			/**
			 * Validates the double array parameters 
			 */
			void validateDoubleArrayParams() throw(invalid_argument);
			void validateDoubleArrayParam(DoubleArrayParamDef pDef) throw(invalid_argument);

			int parseDOM(const char* xmlParamSet, InMemoryXmlData * fileInfo) throw(domain_error);
			int parseFile(const string & xmlFile);
			int parseSAX(const string & xmlParamSet);
			void handleBoolParam(DOMElement *paramElem) throw(domain_error);
			void handleIntParam(DOMElement *paramElem) throw(domain_error);
			void handleIntArrayParam(DOMElement *paramElem) throw(domain_error);
			void handleDoubleParam(DOMElement *paramElem) throw(domain_error);
			void handleDoubleArrayParam(DOMElement *paramElem) throw(domain_error);
			void handleStringParam(DOMElement *paramElem) throw(domain_error);
			void handleStringArrayParam(DOMElement *paramElem) throw(domain_error);
			void processParamNodes(DOMNodeList *paramNodes) throw(domain_error);
			void initialize();
			void setSchemaLocation(DOMBuilder * parser);

			string name;
			string psetDefFileName;
			map<string, IntParam> intParamMap;
			map<string, IntArrayParam> intArrayParamMap;
			map<string, DoubleParam> doubleParamMap;
			map<string, DoubleArrayParam> doubleArrayParamMap;
			map<string, StringParam> stringParamMap;
			map<string, StringArrayParam> stringArrayParamMap;
			map<string, BoolParam> boolParamMap;

			// Constants
			XMLCh* PSETDEF_TAG_NAME;
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

			auto_ptr<ParamSetDef> ParameterSetDef;
	};

}
#endif /*!_PARAMETER_SET_H*/

