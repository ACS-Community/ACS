/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 *
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
*
*
* "@(#) $Id: ParameterSet.cpp,v 1.30 2010/04/27 12:20:58 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <fstream>
#include <iterator>
#include <sstream>
#include <vector>

#include <ParameterSet.h>
#include <acsDOMErrorHandler.h>
#include <ParamStrX.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/framework/MemBufInputSource.hpp>
#include <xercesc/framework/Wrapper4InputSource.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLDouble.hpp>

#include <logging.h>

#ifndef MEMPARSE_ENCODING
   #if defined(OS390)
      #define MEMPARSE_ENCODING "ibm-1047-s390"
   #elif defined(OS400)
      #define MEMPARSE_ENCODING "ibm037"
   #else
      #define MEMPARSE_ENCODING "ascii"
   #endif
#endif 

using namespace std;
using namespace Parameters;
XERCES_CPP_NAMESPACE_USE

typedef map< string, IntParam >::value_type intParamValType;
typedef map< string, DoubleParam >::value_type doubleParamValType;
typedef map< string, StringParam >::value_type stringParamValType;
typedef map< string, IntArrayParam >::value_type intArrayParamValType;
typedef map< string, DoubleArrayParam >::value_type doubleArrayParamValType;
typedef map< string, StringArrayParam >::value_type stringArrayParamValType;
typedef map< string, BoolParam >::value_type boolParamValType;

/**
 * Constructor.
 * 
 * @param xmlFileName the string containing a filename 
 * which contains the XML representation of the parameter set for a task.
 */
ParameterSet::ParameterSet(string xmlFileName)
{
	// Initialize the XML4C2 system
	try
	{
		initialize();
		parseFile(xmlFileName);
		validate();
	}
	catch (const XMLException& toCatch)
	{
		ACS_LOG(LM_ERROR, "ParameterSet::ParameterSet", 
			(LM_ERROR, "XMLException during initialization! Message: %s\n", StrX(toCatch.getMessage()).localForm()))
	}
	catch (const invalid_argument argError)
	{
		ACS_LOG(LM_ERROR, 
			"ParameterSet::ParameterSet", (LM_ERROR, 
				"Error validating against parameter set definition\n*****\n\nMessage:\n\n%s\n*****\n",
				argError.what()))
	}
}

/**
 * Constructor.
 * 
 * @param InMemoryXmlData contains the XML data (as a InMemoryXmlData object) for the parameter set for a task.
 */
ParameterSet::ParameterSet(InMemoryXmlData * xmlData)
{
	try
	{
		initialize();
		// NOTE: parsing is currently done with DOM. If SAX parsing is desired,
		// then parseSAX method must be implemented and the line below changed
		// to invoke parseSAX instead of parseDOM. 
		parseDOM(NULL, xmlData);
		validate();
	}
	catch (const XMLException& toCatch)
	{
		ACS_LOG(LM_ERROR, "ParameterSet::ParameterSet", (LM_ERROR, 
			"***** XMLException message: ***** \n\n%s \n *****\n", StrX(toCatch.getMessage()).localForm()))
	}
	catch (const invalid_argument argError)
	{
		ACS_LOG(LM_ERROR, 
			"ParameterSet::ParameterSet", (LM_ERROR, 
				"Error validating against parameter set definition\n*****\n\nMessage:\n\n%s\n*****\n",
				argError.what()))
	}
}

/**
 * Destructor.
 */
ParameterSet::~ParameterSet()
{
	XMLString::release(&PSETDEF_TAG_NAME);
	XMLString::release(&PARAMETER_TAG_NAME);
	XMLString::release(&NAME_TAG_NAME);
	XMLString::release(&VALUE_TAG_NAME);
	XMLString::release(&UNITS_TAG_NAME);

	XMLString::release(&INT_PARAM_TYPE);
	XMLString::release(&DOUBLE_PARAM_TYPE);
	XMLString::release(&STRING_PARAM_TYPE);
	XMLString::release(&BOOL_PARAM_TYPE);
	XMLString::release(&INT_ARRAY_PARAM_TYPE);
	XMLString::release(&DOUBLE_ARRAY_PARAM_TYPE);
	XMLString::release(&STRING_ARRAY_PARAM_TYPE);
	XMLPlatformUtils::Terminate();
}

/**
 * Returns the name of the parameter set as a string.
 */
string ParameterSet::getName() 
{
	return name;
}

/**
 * Converts the ParameterSet object to an XML string.
 */
string ParameterSet::toString()
{
	string retVal;

	// create the xml header:
	// <?xml version="1.0" encoding="ISO-8859-1"?>
	retVal.append(XML_STANDARD_HEADER);
	retVal.append(NEWLINE);

	// create the paramset stanza:
	//	<pset:paramset xmlns:ps="urn:schemas-cosylab-com:pset:1.0" 
	//		xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" 
	//		xsi:schemaLocation="urn:schemas-cosylab-com:pset:1.0 pset.xsd">
	retVal.append(LESS_THAN_SIGN);
	retVal.append(PARAMETERSET_NAMESPACE_PREFIX);
	retVal.append(COLON);
	retVal.append(PARAMETERSET_STRING);
	retVal.append(SPACE);
	retVal.append(XML_NAMESPACE_STRING);
	retVal.append(COLON);
	retVal.append(PARAMETERSET_NAMESPACE_PREFIX);
	retVal.append(EQUALS);
	retVal.append(QUOTE);
	retVal.append(PSET_NAMESPACE_URI);
	retVal.append(QUOTE);
	retVal.append(NEWLINE);
	retVal.append(TAB);
	retVal.append(TAB);
	retVal.append(XML_NAMESPACE_STRING);
	retVal.append(COLON);
	retVal.append(XML_SCHEMA_INSTANCE_PREFIX);
	retVal.append(EQUALS);
	retVal.append(QUOTE);
	retVal.append(XML_SCHEMA_URI);
	retVal.append(QUOTE);
	retVal.append(NEWLINE);
	retVal.append(TAB);
	retVal.append(TAB);
	retVal.append(XML_SCHEMA_INSTANCE_PREFIX);
	retVal.append(COLON);
	retVal.append(XML_SCHEMA_LOCATION_HINT_STRING);
	retVal.append(EQUALS);
	retVal.append(QUOTE);
	retVal.append(PSET_NAMESPACE_URI);
	retVal.append(SPACE);
	retVal.append(PARAMETERSET_SCHEMA_NAME);
	retVal.append(QUOTE);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(NEWLINE);

	// create the psetdef element
	// e.g. <psetdef>psetdef.xml</psetdef>
	retVal.append(TAB);
	retVal.append(LESS_THAN_SIGN);
	retVal.append(PSETDEF_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(getParamSetDefFileName());
	retVal.append(LESS_THAN_SIGN);
	retVal.append(SLASH_STRING);
	retVal.append(PSETDEF_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(NEWLINE);

	// create the paramset name element
	// e.g. <name>VLAFillerTestValues</name>
	retVal.append(TAB);
	retVal.append(LESS_THAN_SIGN);
	retVal.append(NAME_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(getName());
	retVal.append(LESS_THAN_SIGN);
	retVal.append(SLASH_STRING);
	retVal.append(NAME_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(NEWLINE);
	retVal.append(NEWLINE);

	// call toString on all of the bool params in the parameter set
	map<string, BoolParam>::iterator boolParamIter = boolParamMap.begin();
	while(boolParamIter != boolParamMap.end()) 
	{
		retVal.append((*boolParamIter).second.toString());
		retVal.append(NEWLINE);
		++boolParamIter;
	}

	// call toString on all of the int params in the parameter set
	map<string, IntParam>::iterator intParamIter = intParamMap.begin();
	while(intParamIter != intParamMap.end()) 
	{
		retVal.append((*intParamIter).second.toString());
		retVal.append(NEWLINE);
		++intParamIter;
	}

	// call toString on all of the double params in the parameter set
	map<string, DoubleParam>::iterator doubleParamIter = doubleParamMap.begin();
	while(doubleParamIter != doubleParamMap.end()) 
	{
		retVal.append((*doubleParamIter).second.toString());
		retVal.append(NEWLINE);
		++doubleParamIter;
	}

	// call toString on all of the string params in the parameter set
	map<string, StringParam>::iterator stringParamIter = stringParamMap.begin();
	while(stringParamIter != stringParamMap.end()) 
	{
		retVal.append((*stringParamIter).second.toString());
		retVal.append(NEWLINE);
		++stringParamIter;
	}

	// call toString on all of the int array params in the parameter set
	map<string, IntArrayParam>::iterator intArrayParamIter = intArrayParamMap.begin();
	while(intArrayParamIter != intArrayParamMap.end()) 
	{
		retVal.append((*intArrayParamIter).second.toString());
		retVal.append(NEWLINE);
		++intArrayParamIter;
	}

	// call toString on all of the double array params in the parameter set
	map<string, DoubleArrayParam>::iterator doubleArrayParamIter = doubleArrayParamMap.begin();
	while(doubleArrayParamIter != doubleArrayParamMap.end()) 
	{
		retVal.append((*doubleArrayParamIter).second.toString());
		retVal.append(NEWLINE);
		++doubleArrayParamIter;
	}

	// call toString on all of the string array params in the parameter set
	map<string, StringArrayParam>::iterator stringArrayParamIter = stringArrayParamMap.begin();
	while(stringArrayParamIter != stringArrayParamMap.end()) 
	{
		retVal.append((*stringArrayParamIter).second.toString());
		retVal.append(NEWLINE);
		++stringArrayParamIter;
	}

	// create the closing tag for the paramset
	// e.g. </pset:paramset>
	retVal.append(LESS_THAN_SIGN);
	retVal.append(SLASH_STRING);
	retVal.append(PARAMETERSET_NAMESPACE_PREFIX);
	retVal.append(COLON);
	retVal.append(PARAMETERSET_STRING);
	retVal.append(GREATER_THAN_SIGN);

	return retVal;
}

/**
 * Returns the file name for the psetdef associated with this ParameterSet,
 * as defined by the <psetdef> tag in the ParameterSet XML instance document.
 */
string ParameterSet::getParamSetDefFileName()
{
   return psetDefFileName;
}

/**
 * get an int param by name.
 * @param paramName the name of the parameter desired.
 */
IntParam ParameterSet::getIntParam(string paramName) 
{
   IntParam retVal;
   map<string, IntParam >::iterator mapEntry = intParamMap.find(paramName);
   if(mapEntry != intParamMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get a double param by name.
 * @param paramName the name of the parameter desired.
 */
DoubleParam ParameterSet::getDoubleParam(string paramName) 
{
   DoubleParam retVal;
   map<string, DoubleParam >::iterator mapEntry = doubleParamMap.find(paramName);
   if(mapEntry != doubleParamMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get an string param by name.
 * @param paramName the name of the parameter desired.
 */
StringParam ParameterSet::getStringParam(string paramName) 
{
   StringParam retVal;
   map<string, StringParam >::iterator mapEntry = stringParamMap.find(paramName);
   if(mapEntry != stringParamMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get an bool param by name.
 * @param paramName the name of the parameter desired.
 */
BoolParam ParameterSet::getBoolParam(string paramName) 
{
   BoolParam retVal;
   map<string, BoolParam>::iterator mapEntry = boolParamMap.find(paramName);
   if(mapEntry != boolParamMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get an array of int params by name
 * @param paramName the name of the parameter desired.
 */
IntArrayParam ParameterSet::getIntArrayParam(string paramName) 
{
   IntArrayParam retArray;
   map<string, IntArrayParam >::iterator mapEntry = intArrayParamMap.find(paramName);
   if(mapEntry != intArrayParamMap.end()) 
   {
      retArray = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retArray;
}

/**
 * get an array of double params by name
 * @param paramName the name of the parameter desired.
 */
DoubleArrayParam ParameterSet::getDoubleArrayParam(string paramName) 
{
   DoubleArrayParam retArray;
   map<string, DoubleArrayParam >::iterator mapEntry = doubleArrayParamMap.find(paramName);
   if(mapEntry != doubleArrayParamMap.end()) 
   {
      retArray = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retArray;
}

/**
 * get an array of string param by name
 * @param paramName the name of the parameter desired.
 */
StringArrayParam ParameterSet::getStringArrayParam(string paramName) 
{
   StringArrayParam retArray;
   map<string, StringArrayParam >::iterator mapEntry = stringArrayParamMap.find(paramName);
   if(mapEntry != stringArrayParamMap.end()) 
   {
      retArray = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for parameter \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retArray;
}

/**
 * set an int param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, IntParam value)
{
	// TODO - consider having a "default" flag that is passed into setParam methods
	// to denote whether the error msg should say "when defaulting" rather than "when setting"
	map<string, IntParam >::iterator mapEntry = intParamMap.find(paramName);

	auto_ptr<IntParam> previousValue;
	if(mapEntry != intParamMap.end()) 
	{
		// there was an existing entry, so save it in case we need to "roll back" due to error
		IntParam prevEntry = (*mapEntry).second;
		previousValue.reset(new IntParam(prevEntry.getValue(), prevEntry.getName(), prevEntry.getUnits()));
		(*mapEntry).second = value;
	}
	else 
	{
		// there was no existing entry, so just insert straight away
		intParamMap.insert(intParamValType( paramName, value ) );
	}
	try {
		// check to make sure that value is valid
		IntParamDef pDef;
		try {
			pDef = ParameterSetDef->getIntParamDef(paramName);
		}
		catch(domain_error toCatch) {
			// no param def for this parameter, rethrow as invalid arg error
			throw invalid_argument(toCatch.what());
		}
		// validate int param against param def
		validateIntParam(pDef);
	}
	catch(invalid_argument toCatch) {
		// caught an exception, which means the value was not valid
		if(mapEntry != intParamMap.end()) {
			// restore previous value
			(*mapEntry).second = *previousValue;
		}
		else {
			// else remove the new invalid entry if there was no previous entry
			intParamMap.erase(paramName);
		}
		// TODO - rethrow??
		ACS_LOG(LM_ERROR, 
			"ParameterSet::setParam", (LM_ERROR, 
			"Error setting parameter: *****\n\nMessage:\n\n%s\n*****\n",
			toCatch.what()))
	}
}

/**
 * set a double param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, DoubleParam value) 
{
   map<string, DoubleParam >::iterator mapEntry = doubleParamMap.find(paramName);
   auto_ptr<DoubleParam> previousValue;
   if(mapEntry != doubleParamMap.end()) 
   {
		// there was an existing entry, so save it in case we need to "roll back" due to error
      DoubleParam prevEntry = (*mapEntry).second;
      previousValue.reset(new DoubleParam(prevEntry.getValue(), prevEntry.getName(), prevEntry.getUnits()));
      (*mapEntry).second = value;
   }
   else 
   {
		// there was no existing entry, so just insert straight away
      doubleParamMap.insert(doubleParamValType( paramName, value ) );
   }
   try {
      // check to make sure that value is valid
      DoubleParamDef pDef;
      try {
         pDef = ParameterSetDef->getDoubleParamDef(paramName);
      }
      catch(domain_error toCatch) {
         // no param def for this parameter, rethrow as invalid arg error
         throw invalid_argument(toCatch.what());
      }
      // validate int param against param def
      validateDoubleParam(pDef);
   }
   catch(invalid_argument toCatch) {
      // caught an exception, which means the value was not valid
      if(mapEntry != doubleParamMap.end()) {
         // restore previous value
         (*mapEntry).second = *previousValue;
      }
      else {
         // else remove the invalid entry if it didn't already exist
	 doubleParamMap.erase(paramName);
      }
      ACS_LOG(LM_ERROR, "ParameterSet::setParam", (LM_ERROR, 
          "Error setting parameter: *****\n\nMessage:\n\n%s\n*****\n", toCatch.what()))
   }
}

/**
 * set a string param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, StringParam value) 
{
   map<string, StringParam >::iterator mapEntry = stringParamMap.find(paramName);
   auto_ptr<StringParam> previousValue;
   if(mapEntry != stringParamMap.end()) 
   {
		// there was an existing entry, so save it in case we need to "roll back" due to error
      StringParam prevEntry = (*mapEntry).second;
      previousValue.reset(new StringParam(prevEntry.getName(), prevEntry.getValue()));
      (*mapEntry).second = value;
   }
   else 
   {
		// there was no existing entry, so just insert straight away
      stringParamMap.insert(stringParamValType( paramName, value ) );
   }
   try {
      // check to make sure that value is valid
      StringParamDef pDef;
      try {
         pDef = ParameterSetDef->getStringParamDef(paramName);
      }
      catch(domain_error toCatch) {
         // no param def for this parameter, rethrow as invalid arg error
         throw invalid_argument(toCatch.what());
      }
      // validate int param against param def
      validateStringParam(pDef);
   }
   catch(invalid_argument toCatch) {
      // caught an exception, which means the value was not valid
      if(mapEntry != stringParamMap.end()) {
         // restore previous value, if there was one
         (*mapEntry).second = *previousValue;
      }
      else {
         // else erase value, if there was no previous value
			stringParamMap.erase(paramName);
      }
      ACS_LOG(LM_ERROR, "ParameterSet::setParam", (LM_ERROR, 
        "Error setting parameter: *****\n\nMessage:\n\n%s\n****\n", toCatch.what()))
   }
}

/**
 * set a bool param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, BoolParam value) 
{
   map<string, BoolParam >::iterator mapEntry = boolParamMap.find(paramName);
   if(mapEntry != boolParamMap.end()) 
   {
      (*mapEntry).second = value;
   }
   else 
   {
      boolParamMap.insert(boolParamValType( paramName, value ) );
   }
}

/**
 * set an int array param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, IntArrayParam value) 
{
   map<string, IntArrayParam >::iterator mapEntry = intArrayParamMap.find(paramName);
   auto_ptr<IntArrayParam> previousValue;
   if(mapEntry != intArrayParamMap.end()) 
   {
		// there was an existing entry, so save it in case we need to "roll back" due to error
      IntArrayParam prevEntry = (*mapEntry).second;
      previousValue.reset(new IntArrayParam(prevEntry.getValues(), prevEntry.getName(), prevEntry.getUnits()));
      (*mapEntry).second = value;
   }
   else 
   {
		// there was no existing entry, so just insert straight away
      intArrayParamMap.insert(intArrayParamValType( paramName, value ) );
   }
   try {
      // check to make sure that value is valid
      IntArrayParamDef pDef;
      try {
         pDef = ParameterSetDef->getIntArrayParamDef(paramName);
      }
      catch(domain_error toCatch) {
         // no param def for this parameter, rethrow as invalid arg error
         throw invalid_argument(toCatch.what());
      }
      // validate int param against param def
      validateIntArrayParam(pDef);
   }
   catch(invalid_argument toCatch) {
      // caught an exception, which means the value was not valid
      if(mapEntry != intArrayParamMap.end()) {
         // restore previous value
         (*mapEntry).second = *previousValue;
      }
      else {
         // else if there was no previous value, erase invalid param
	 intArrayParamMap.erase(paramName);
      }
      ACS_LOG(LM_ERROR, "ParameterSet::setParam", (LM_ERROR, 
        "Error setting parameter: *****\n\nMessage:\n\n%s\n*****\n", toCatch.what()))
   }
}

/**
 * set a double array param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, DoubleArrayParam value) 
{
   map<string, DoubleArrayParam >::iterator mapEntry = doubleArrayParamMap.find(paramName);
   auto_ptr<DoubleArrayParam> previousValue;
   if(mapEntry != doubleArrayParamMap.end()) 
   {
		// there was an existing entry, so save it in case we need to "roll back" due to error
      DoubleArrayParam prevEntry = (*mapEntry).second;
      previousValue.reset(new DoubleArrayParam(prevEntry.getValues(), prevEntry.getName(), prevEntry.getUnits()));
      (*mapEntry).second = value;
   }
   else 
   {
		// there was no existing entry, so just insert straight away
      doubleArrayParamMap.insert(doubleArrayParamValType( paramName, value ) );
   }
   try {
      // check to make sure that value is valid
      DoubleArrayParamDef pDef;
      try {
         pDef = ParameterSetDef->getDoubleArrayParamDef(paramName);
      }
      catch(domain_error toCatch) {
         // no param def for this parameter, rethrow as invalid arg error
         throw invalid_argument(toCatch.what());
      }
      // validate int param against param def
      validateDoubleArrayParam(pDef);
   }
   catch(invalid_argument toCatch) {
      // caught an exception, which means the value was not valid
      if(mapEntry != doubleArrayParamMap.end()) {
         // restore previous value
         (*mapEntry).second = *previousValue;
      }
      else {
         // else remove the invalid param if it previously didn't exist
	 doubleArrayParamMap.erase(paramName);
      }
      ACS_LOG(LM_ERROR, "ParameterSet::setParam", (LM_ERROR, 
         "Error setting parameter: *****\n\nMessage:\n\n%s\n*****\n", toCatch.what()))
   }
}

/**
 * set a string array param by name.
 *
 * NOTE: the setParam methods must be called after, not before, the parseDOM method; 
 * However, this doesn't add any burden to users of this class; this should always be true, 
 * because the parseDOM method is called by the constructor. It is noted here just in case
 * someone refactors the logic such that the constructor doesn't call parseDOM.
 *
 * @param paramName the name of the parameter to be set.
 * @param value the value of the parameter.
 */
void ParameterSet::setParam(string paramName, StringArrayParam value) 
{
	map<string, StringArrayParam >::iterator mapEntry = stringArrayParamMap.find(paramName);
	auto_ptr<StringArrayParam> previousValue;
	if(mapEntry != stringArrayParamMap.end()) 
	{
		// there was an existing entry, so save it in case we need to "roll back" due to error
		StringArrayParam prevEntry = (*mapEntry).second;
		previousValue.reset(new StringArrayParam(prevEntry.getValues(), prevEntry.getName()));
		(*mapEntry).second = value;
	}
	else 
	{
		// there was no existing entry, so just insert straight away
		stringArrayParamMap.insert(stringArrayParamValType( paramName, value ) );
	}
	try {
		// check to make sure that value is valid
		StringArrayParamDef pDef;
		try {
			pDef = ParameterSetDef->getStringArrayParamDef(paramName);
		}
		catch(domain_error toCatch) {
			// no param def for this parameter, rethrow as invalid arg error
			throw invalid_argument(toCatch.what());
		}
		// validate int param against param def
		validateStringArrayParam(pDef);
	}
	catch(invalid_argument toCatch) {
		// caught an exception, which means the value was not valid
		if(mapEntry != stringArrayParamMap.end()) {
		// restore previous value
		(*mapEntry).second = *previousValue;
		}
		else {
			// else remove the new invalid entry if it didn't already exist
			stringArrayParamMap.erase(paramName);
		}
		ACS_LOG(LM_ERROR, 
			"ParameterSet::setParam", (LM_ERROR, 
			"Error setting parameter: *****\n\nMessage:\n\n%s\n*****\n", toCatch.what()))
	}
}

/**
 * Private method to orchestrate the XML parsing from a file.
 */
int ParameterSet::parseFile(const string & xmlFile)
{
	// if the file exists, use it 
	ifstream xmlFileStream(xmlFile.c_str());
	string fileToParse = xmlFile;
	if(!xmlFileStream) {
		// else, try prepending TASK_DIR/xml
		// if TASK_DIR env var is defined
		char *taskDir = getenv(TASK_DIR_ENV_VAR_NAME);
		if(NULL != taskDir) {
			string schemaDirStr(taskDir);
			schemaDirStr.append(SLASH_STRING).append(INSTANCE_DOC_SUBDIR_NAME).append(SLASH_STRING);
			fileToParse.insert(0, schemaDirStr);
			ACS_DEBUG_PARAM("ParameterSet::parseFile", "Appended TASK_DIR to file name: %s", fileToParse.c_str())
		}
		else {
			string errorMsg = "ParameterSet::parseFile - cannot locate file of name: " + xmlFile;
			throw invalid_argument(errorMsg);
		}
	}
	// NOTE: parsing is currently done with DOM. If SAX parsing is desired,
	// then parseSAX method must be implemented and the line below changed
	// to invoke parseSAX instead of parseDOM. 
	return parseDOM(fileToParse.c_str(), NULL);
}

/**
 * Private method to orchestrate the XML parsing using DOM.
 * @param xmlFile - ptr to character string representing a file name (OR NULL if we are parsing from memory)
 * @param xmlData - ptr to an InMemoryXmlData object (OR NULL if we are using a file)
 * NOTE: one of the params, but not both, should be NULL for a given parse - i.e.
 * either we are parsing from a file or parsing from memory, but not both (nor neither - 
 * i.e. NULL for both params should not occur either).
 */
int ParameterSet::parseDOM(const char* xmlFile, InMemoryXmlData * xmlData) 
{
	bool doNamespaces = true;
	bool doSchema = true;
	bool schemaFullChecking = true;

	// Instantiate the DOM parser.
	static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
	DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(gLS);
	DOMBuilder        *parser = ((DOMImplementationLS*)impl)->createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS, 0);

	// configure the DOM parser
	parser->setFeature(XMLUni::fgDOMNamespaces, doNamespaces);
	parser->setFeature(XMLUni::fgXercesSchema, doSchema);
	parser->setFeature(XMLUni::fgXercesSchemaFullChecking, schemaFullChecking);
	parser->setFeature(XMLUni::fgDOMValidation, true);
	parser->setFeature(XMLUni::fgXercesDOMHasPSVIInfo, true);
	parser->setFeature(XMLUni::fgDOMDatatypeNormalization, true);

	setSchemaLocation(parser);

	// Set the error handler to an instance of acsDOMErrorHandler
	acsDOMErrorHandler* errHandler = new acsDOMErrorHandler();
	parser->setErrorHandler(errHandler);

	DOMDocument *doc = 0;
	MemBufInputSource *mbis = NULL;
	
	try {
		if(NULL != xmlFile) {
			doc = parser->parseURI(xmlFile);
			ACS_DEBUG_PARAM("ParameterSet::parseDOM", "parsed file: %s", xmlFile)
		}
		else if(NULL != xmlData) {
			mbis = new  MemBufInputSource((const XMLByte *) xmlData->getContent().c_str(),
				xmlData->getSize(), xmlData->getId().c_str(), false);
			Wrapper4InputSource wrapper4InputSource(mbis);
			doc = parser->parse(wrapper4InputSource);
			ACS_DEBUG_PARAM("ParameterSet::parseDOM", "parsed xml in memory of id: %s", xmlData->getId().c_str())
		}
		else {
			// invalid - both params NULL - should not happen
			throw invalid_argument("ParameterSet::parseDOM - XML neither in file or memory!");
		}

		// Get the psetdef element and set the psetDefFileName from it
		DOMNodeList * psetDefNodes = doc->getElementsByTagName(PSETDEF_TAG_NAME);
		if(NULL != psetDefNodes) {
			DOMNode * psetDefItem = psetDefNodes->item(0);
			if(NULL != psetDefItem) {
				DOMNode * psetDefTextNode = psetDefItem->getFirstChild();
				if(NULL != psetDefTextNode) {
					const XMLCh * psetDefValue = psetDefTextNode->getNodeValue();
					psetDefFileName = string(StrX(psetDefValue).localForm());
					if(NULL != psetDefFileName.c_str()) {
						ParameterSetDef.reset(new ParamSetDef(psetDefFileName));
					}
				}
				else {
					string errorMsg = "parameter set must have non-null psetdef value";
					throw domain_error(errorMsg);
				}
			}
		}

		// Get the name element for the parameter set
		DOMNodeList * psetNameNodes = doc->getElementsByTagName(NAME_TAG_NAME);
		if(NULL != psetNameNodes) {
			DOMNode * psetNameItem = psetNameNodes->item(0);
			if(NULL != psetNameItem) {
				DOMNode * psetNameNode = psetNameItem->getFirstChild();
				if(NULL != psetNameNode) {
					const XMLCh * psetNameValue = psetNameNode->getNodeValue();
					string psetName = string(StrX(psetNameValue).localForm());
					setName(psetName);
				}
				else {
					string errorMsg = "parameter set must have non-null name value";
					throw domain_error(errorMsg);
				}
			}
		}

		// Get all parameters
		DOMNodeList * paramNodes = doc->getElementsByTagName(PARAMETER_TAG_NAME);
		if(NULL != paramNodes) {
			processParamNodes(paramNodes);
		}
	}
	catch (const XMLException& toCatch) {
		char* message = XMLString::transcode(toCatch.getMessage());
		ACS_LOG(LM_ERROR, "ParameterSet::parseDOM", (LM_ERROR, 
			"***** XMLException message: ***** \n\n%s \n *****\n", message))
		XMLString::release(&message);
		return -1;
	}
	catch (const DOMException& toCatch) {
		char* message = XMLString::transcode(toCatch.msg);
		ACS_LOG(LM_ERROR, "ParameterSet::parseDOM", (LM_ERROR, "***** DOMException message: %s *****", message))
		XMLString::release(&message);
		return -1;
	}
	catch (const std::domain_error& toCatch) {
		ACS_LOG(LM_ERROR, "ParameterSet::parseDOM", (LM_ERROR, "***** Error in parameter set: %s *****", toCatch.what()))
		return -1;
	}
	catch (...) {
		ACS_LOG(LM_ERROR, "ParameterSet::parseDOM", (LM_ERROR, "***** Unexpected exception! *****"))
		return -1;
	}

	parser->release();
	delete errHandler;
	return 0;
}


/**
 * Private method to orchestrate the XML parsing using SAX.
 */
int ParameterSet::parseSAX(const string & xmlFileName)
{
	// TODO - implement this, if desired to parse with SAX
	return 0;
}

/**
 * Private method to handle a BoolParam
 */
void ParameterSet::handleBoolParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleBoolParam", "param name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "boolean parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	DOMNode *valueNode = valueNodes->item(0);
	DOMNode *valueTextNode = valueNode->getFirstChild();
	string boolString;
	if(NULL != valueTextNode) {
		tmpString = XMLString::transcode(valueTextNode->getTextContent());
		boolString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleBoolParam", "bool string: %s", boolString.c_str())
	}
	else {
		string errorMsg = "boolean parameter \"" + paramName + "\" must have non-null value value";
		throw domain_error(errorMsg);
	}
	bool boolVal = ((0 == XMLString::compareIString(boolString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(boolString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParameterSet::handleBoolParam", "bool value: %d", boolVal)

	// add a new BoolParam to the ParameterSet
	BoolParam newBoolParam(boolVal, paramName);
	setParam(paramName, newBoolParam);
	ACS_DEBUG("ParameterSet::handleBoolParam", "inserted a bool param into object model")
}

/**
 * Private method to handle an IntParam
 */
void ParameterSet::handleIntParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
	}
	else {
		string errorMsg = "int parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	DOMNode *valueNode = valueNodes->item(0);
	DOMNode *valueTextNode = valueNode->getFirstChild();
	int intValue = 0;
	if(NULL != valueTextNode) {
		intValue = XMLString::parseInt(valueTextNode->getTextContent());
	}
	else {
		string errorMsg = "int parameter \"" + paramName + "\" must have non-null value value";
		throw domain_error(errorMsg);
	}

	// get the units
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr<string> paramUnits;
	if(NULL != unitsNodes) {
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) {
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				paramUnits.reset(new string(tmpString));
				XMLString::release(&tmpString);
			}
			else {
				string errorMsg = "int parameter \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new IntParam to the ParameterSet
	IntParam newIntParam(intValue, paramName, paramUnits);
	setParam(paramName, newIntParam);
	ACS_DEBUG("ParameterSet::handleIntParam", "inserted an int param into object model")
}

/**
 * Private method to handle an IntArrayParam
 */
void ParameterSet::handleIntArrayParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	char *tmpString = NULL;
	string paramName;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleIntArrayParam", "param name:", paramName.c_str())
	}
	else {
		string errorMsg = "int array parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value(s)
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	vector<int> valueVector;
	for (unsigned int j = 0; j < valueNodes->getLength(); j++) {
		DOMNode *valueNode = valueNodes->item(j);
		DOMNode *valueTextNode = valueNode->getFirstChild();
		if(NULL != valueTextNode) {
			int intValue = XMLString::parseInt(valueTextNode->getTextContent());
			valueVector.push_back(intValue);
		}
		else {
			string errorMsg = "int array parameter \"" + paramName + "\" must have non-null value value";
			throw domain_error(errorMsg);
		}
	}

	// get the units
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr<string> paramUnits;
	if(NULL != unitsNodes) {
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) {
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				paramUnits.reset(new string(tmpString));
				XMLString::release(&tmpString);
			}
			else {
				string errorMsg = "int array parameter \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new IntArrayParam to the ParameterSet
	IntArrayParam newIntArrayParam(valueVector, paramName, paramUnits);
	setParam(paramName, newIntArrayParam);
	ACS_DEBUG("ParameterSet::handleIntArrayParam", "inserted an int array param into object model")
}

/**
 * Private method to handle a DoubleParam
 */
void ParameterSet::handleDoubleParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	char *tmpString = NULL;
	string paramName;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleDoubleParam", "param name:", paramName.c_str())
	}
	else {
		string errorMsg = "double parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	DOMNode *valueNode = valueNodes->item(0);
	DOMNode *valueTextNode = valueNode->getFirstChild();
	double doubleVal = 0.0;
	if(NULL != valueTextNode) {
		XMLDouble xmlDouble(valueTextNode->getTextContent());
		doubleVal = xmlDouble.getValue();
		ACS_DEBUG_PARAM("ParameterSet::handleDoubleParam", "param value: %f", doubleVal)
	}
	else {
		string errorMsg = "double parameter \"" + paramName + "\" must have non-null value value";
		throw domain_error(errorMsg);
	}

	// get the units
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr<string> paramUnits;
	if(NULL != unitsNodes) {
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) {
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				paramUnits.reset(new string(tmpString));
				XMLString::release(&tmpString);
			}
			else {
				string errorMsg = "double parameter \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new DoubleParam to the ParameterSet
	DoubleParam newDoubleParam(doubleVal, paramName, paramUnits);
	setParam(paramName, newDoubleParam);
	ACS_DEBUG("ParameterSet::handleDoubleParam", "inserted a double param into object model")
}

/**
 * Private method to handle a DoubleArrayParam
 */
void ParameterSet::handleDoubleArrayParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleDoubleArrayParam", "param name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "double array parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value(s)
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	vector<double> valueVector;
	for (unsigned int j = 0; j < valueNodes->getLength(); j++) {
		DOMNode *valueNode = valueNodes->item(j);
		DOMNode *valueTextNode = valueNode->getFirstChild();
		if(NULL != valueTextNode) {
			XMLDouble xmlDouble(valueTextNode->getTextContent());
			double doubleVal = xmlDouble.getValue();
			valueVector.push_back(doubleVal);
		}
		else {
			string errorMsg = "double array parameter \"" + paramName + "\" must have non-null value value";
			throw domain_error(errorMsg);
		}
	}

	// get the units
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr<string> paramUnits;
	if(NULL != unitsNodes) {
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) {
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				paramUnits.reset(new string(tmpString));
				XMLString::release(&tmpString);
			}
			else {
				string errorMsg = "double array parameter \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new DoubleArrayParam to the ParameterSet
	DoubleArrayParam newDoubleArrayParam(valueVector, paramName, paramUnits);
	setParam(paramName, newDoubleArrayParam);
	ACS_DEBUG("ParameterSet::handleDoubleArrayParam", "inserted a double array param into object model")
}

/**
 * Private method to handle a StringParam
 */
void ParameterSet::handleStringParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	char *tmpString = NULL;
	string paramName;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleStringParam", "param name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "string parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	DOMNode *valueNode = valueNodes->item(0);
	DOMNode *valueTextNode = valueNode->getFirstChild();
	string paramValue;
	if(NULL != valueTextNode) {
		tmpString = XMLString::transcode(valueTextNode->getTextContent());
		paramValue = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleStringParam", "param value: %s", paramValue.c_str())
	}
	else {
		string errorMsg = "string parameter \"" + paramName + "\" must have non-null value value";
		throw domain_error(errorMsg);
	}

	// add a new StringParam to the ParameterSet
	StringParam newStringParam(paramName, paramValue);
	setParam(paramName, newStringParam);
	ACS_DEBUG("ParameterSet::handleStringParam", "inserted a string param into object model")
}

/**
 * Private method to handle a StringArrayParam
 */
void ParameterSet::handleStringArrayParam(DOMElement *paramElem) 
{
	// get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	char *tmpString = NULL;
	string paramName;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParameterSet::handleStringArrayParam", "param name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "string array parameter must have non-null name";
		throw domain_error(errorMsg);
	}

	// get the value(s)
	DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
	vector<string> valueVector;
	for (unsigned int j = 0; j < valueNodes->getLength(); j++) {
		DOMNode *valueNode = valueNodes->item(j);
		DOMNode *valueTextNode = valueNode->getFirstChild();
		string stringValue;
		if(NULL != valueTextNode) {
			tmpString = XMLString::transcode(valueTextNode->getTextContent());
			stringValue = string(tmpString);
			XMLString::release(&tmpString);
			valueVector.push_back(stringValue);
			ACS_DEBUG_PARAM("ParameterSet::handleStringArrayParam", 
				"pushing string: %s into value vector", stringValue.c_str())
		}
		else {
			string errorMsg = "string array parameter \"" + paramName + "\" must have non-null value value(s)";
			throw domain_error(errorMsg);
		}
	}

	// add a new StringArrayParam to the ParameterSet
	StringArrayParam newStringArrayParam(valueVector, paramName);
	setParam(paramName, newStringArrayParam);
	ACS_DEBUG("ParameterSet::handleStringArrayParam", "inserted a string array into object model")
}

/**
 * Private method to process all the parameter nodes in the XML file
 */
void ParameterSet::processParamNodes(DOMNodeList *paramNodes) 
{
	for (unsigned int i = 0; i < paramNodes->getLength(); i++) {
		DOMNode * paramItem = paramNodes->item(i);
		if(paramItem->getNodeType() == DOMNode::ELEMENT_NODE) {
			DOMElement * paramElem = (DOMElement*)paramItem;

			const DOMTypeInfo * typeInfo = paramElem->getTypeInfo();
			const XMLCh* typeName = typeInfo->getName();

			// check the type of parameter and instantiate the proper class 
			if(XMLString::equals(typeName, INT_PARAM_TYPE))
			{
				handleIntParam(paramElem);
			}
			if(XMLString::equals(typeName, DOUBLE_PARAM_TYPE))
			{
				handleDoubleParam(paramElem);
			}
			if(XMLString::equals(typeName, STRING_PARAM_TYPE))
			{
				handleStringParam(paramElem);
			}
			if(XMLString::equals(typeName, BOOL_PARAM_TYPE))
			{
				handleBoolParam(paramElem);
			}
			if(XMLString::equals(typeName, INT_ARRAY_PARAM_TYPE))
			{
				handleIntArrayParam(paramElem);
			}
			if(XMLString::equals(typeName, DOUBLE_ARRAY_PARAM_TYPE))
			{
				handleDoubleArrayParam(paramElem);
			}
			if(XMLString::equals(typeName, STRING_ARRAY_PARAM_TYPE))
			{
				handleStringArrayParam(paramElem);
			}
		}
	}
}

/*
 * Returns the ParamSetDef, i.e. the parameter set definition for the ParameterSet. From
 * the ParamSetDef, you can get things like the "help" and "prompt" text, etc.
 */
ParamSetDef* ParameterSet::getParamSetDef()
{
	return ParameterSetDef.get();
}

/**
 * Sets the name for the parameter set.
 */
void ParameterSet::setName(string psetName)
{
	name = psetName;
}

/**
 * Validates the parameter set against the parameter set definition
 */
void ParameterSet::validate() 
{
	bool exceptionCaught = false;
	string errorMsg;
	try {
		validateBoolParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	try {
		validateIntParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	try {
		validateDoubleParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	try {
		validateStringParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	try {
		validateIntArrayParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	try {
		validateDoubleArrayParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	try {
		validateStringArrayParams();
	}
	catch(invalid_argument toCatch) {
		exceptionCaught = true;
		errorMsg.append(toCatch.what());	
	}
	// throw a single exception containing all validation errors
	if(true == exceptionCaught) {
		throw invalid_argument(errorMsg);
	}
}

/**
 * Validates the bool parameters 
 */
void ParameterSet::validateBoolParams() 
{
	// for each bool parameter defined in psetdef
	auto_ptr< vector<BoolParamDef> > boolParamDefs = ParameterSetDef->getBoolParamDefs();
	for(unsigned int i = 0; i < boolParamDefs->size(); ++i)
	{
		BoolParamDef pDef = boolParamDefs->begin()[i];
		validateBoolParam(pDef);
	}
}

/**
 * Validates the int parameters 
 */
void ParameterSet::validateIntParams() 
{
	// for each int parameter defined in psetdef
	auto_ptr< vector<IntParamDef> > paramDefs = ParameterSetDef->getIntParamDefs();
	for(unsigned int i = 0; i < paramDefs->size(); ++i)
	{
		IntParamDef pDef = paramDefs->begin()[i];
		validateIntParam(pDef);
	}
}

/**
 * Validates the double parameters 
 */
void ParameterSet::validateDoubleParams() 
{
	// for each double parameter defined in psetdef
	auto_ptr< vector<DoubleParamDef> > paramDefs = ParameterSetDef->getDoubleParamDefs();
	for(unsigned int i = 0; i < paramDefs->size(); ++i)
	{
		DoubleParamDef pDef = paramDefs->begin()[i];
		validateDoubleParam(pDef);
	}
}

/**
 * Validates the string parameters 
 */
void ParameterSet::validateStringParams() 
{
	// for each string parameter defined in psetdef
	auto_ptr< vector<StringParamDef> > stringParamDefs = ParameterSetDef->getStringParamDefs();
	for(unsigned int i = 0; i < stringParamDefs->size(); ++i)
	{
		StringParamDef pDef = stringParamDefs->begin()[i];
		validateStringParam(pDef);
	}
}

/**
 * Validates the int array parameters 
 */
void ParameterSet::validateIntArrayParams() 
{
	// for each int parameter defined in psetdef
	auto_ptr< vector<IntArrayParamDef> > paramDefs = ParameterSetDef->getIntArrayParamDefs();
	for(unsigned int i = 0; i < paramDefs->size(); ++i)
	{
		IntArrayParamDef pDef = paramDefs->begin()[i];
		validateIntArrayParam(pDef);
	}
}

/**
 * Validates the string array parameters 
 */
void ParameterSet::validateStringArrayParams() 
{
	// for each string parameter defined in psetdef
	auto_ptr< vector<StringArrayParamDef> > paramDefs = ParameterSetDef->getStringArrayParamDefs();
	for(unsigned int i = 0; i < paramDefs->size(); ++i)
	{
		StringArrayParamDef pDef = paramDefs->begin()[i];
		validateStringArrayParam(pDef);
	}
}

/**
 * Validates the double array parameters 
 */
void ParameterSet::validateDoubleArrayParams() 
{
	// for each double parameter defined in psetdef
	auto_ptr< vector<DoubleArrayParamDef> > paramDefs = ParameterSetDef->getDoubleArrayParamDefs();
	for(unsigned int i = 0; i < paramDefs->size(); ++i)
	{
		DoubleArrayParamDef pDef = paramDefs->begin()[i];
		validateDoubleArrayParam(pDef);
	}
}

void ParameterSet::validateStringParam(StringParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			StringParam param = getStringParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required string parameter \"" + pDef.getName() + "\"\n";
			throw invalid_argument(errorMsg);
		}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			StringParam param = getStringParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateStringParam", 
				"optional string param: %s was overridden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			auto_ptr<string> defaultVal = pDef.getDefault();
			if(NULL != defaultVal.get()) {
				// if parameter not specified in pset, use default from psetdef 
				StringParam param(*pDef.getDefault(), pDef.getName());
				setParam(pDef.getName(), param);
				ACS_DEBUG_PARAM("ParameterSet::validateStringParam", 
					"adding optional, unspecified string param: %s", pDef.getName().c_str())
			}
		}
	}
 	// verify against valid values, if defined in psetdef
	auto_ptr<vector<string> > validVals = pDef.getValidValues();
	if(NULL != validVals.get() && pDef.getHasValidValues()) {
		StringParam param = getStringParam(pDef.getName());
		string paramVal = param.getValue();
		bool found = false;
		for(unsigned int j = 0; j < validVals->size(); ++j) {
			ACS_LOG(LM_DEBUG, "ParameterSet::validateStringParam", 
				(LM_DEBUG, "valid value[%d] is: %s and val is: %s", j, validVals->begin()[j].c_str(), paramVal.c_str()))
			if(false == found) {
				if(0 == paramVal.compare(validVals->begin()[j])) {
					found = true;
					continue;
				}
			}
		}
		if(false == found) { 
			ostringstream os;
			os << "value of \"" << paramVal << "\" is not a valid value for string param \"" << param.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}
}

void ParameterSet::validateBoolParam(BoolParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			BoolParam param = getBoolParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required bool parameter \"" + pDef.getName() + "\"\n";
			throw invalid_argument(errorMsg);
		}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			BoolParam param = getBoolParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateBoolParam", 
				"optional bool param: %s was overriden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			if(true == pDef.getHasDefault()) {
				// if parameter not specified in pset, then use default from psetdef 
				BoolParam param(pDef.getDefault(), pDef.getName());
				setParam(pDef.getName(), param);
				ACS_DEBUG("ParameterSet::validateBoolParam", "adding optional, unspecified bool param")
			}
		}
	}
}

void ParameterSet::validateIntParam(IntParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			IntParam param = getIntParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required int parameter \"" + pDef.getName() + "\"\n";
			throw invalid_argument(errorMsg);
		}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			IntParam param = getIntParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateIntParam", 
				"optional int param: %s was overridden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			auto_ptr<int> defaultVal = pDef.getDefault();
			auto_ptr<string> defaultStringVal = pDef.getStringDefault();
			if(NULL != defaultVal.get()) {
				// optional parameter not specified in pset, but psetdef has a default which we will use 
				IntParam param(*pDef.getDefault(), pDef.getName(), pDef.getUnits());
				setParam(pDef.getName(), param);
				ACS_DEBUG_PARAM("ParameterSet::validateIntParam", 
					"adding optional, unspecified int param: %s", pDef.getName().c_str())
			}
		}
	}

	// verify that value in pset is below max and above min, if defined in psetdef
	auto_ptr<int> max = pDef.getMax();
	if(NULL != max.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateIntParam", "psetdef max is: %d", *max)
		// max was defined in psetdef, verify value is below max
		IntParam param = getIntParam(pDef.getName());
		int paramVal = param.getValue();
		ACS_DEBUG_PARAM("ParameterSet::validateIntParam", "pset max is: %d", paramVal)
		if(paramVal > *max) {
			ostringstream os;
			os << "value of \"" << paramVal << "\" exceeds max of \"" << *max 
			<< "\" defined in psetdef for int param \"" << pDef.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}

	auto_ptr<int> min = pDef.getMin();
	if(NULL != min.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateIntParam", "psetdef min is: %d", *min)
		// min was defined in psetdef, verify value is above min
		IntParam param = getIntParam(pDef.getName());
		int paramVal = param.getValue();
		ACS_DEBUG_PARAM("ParameterSet::validateIntParam", "pset min is: %d", paramVal)
		if(paramVal < *min) {
			ostringstream os;
			os << "value of \"" << paramVal << "\" is below min of \"" << *min 
			<< "\" defined in psetdef for int param \""<< pDef.getName()<< "\"\n";
			throw invalid_argument(os.str());
		}
	}
	// verify against valid values, if defined in psetdef
	auto_ptr<vector<int> > validVals = pDef.getValidValues();
	if(NULL != validVals.get() && pDef.getHasValidValues()) {
		IntParam param = getIntParam(pDef.getName());
		int paramVal = param.getValue();
		bool found = false;
		for(unsigned int j = 0; j < validVals->size(); ++j) {
			if(false == found) {
				if(paramVal == validVals->begin()[j]) {
					found = true;
					continue;
				}
			}
		}
		if(false == found) { 
			ostringstream os;
			os << "value of \"" << paramVal << "\" is not a valid value for int param \"" << param.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}
}

void ParameterSet::validateDoubleParam(DoubleParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			DoubleParam param = getDoubleParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required double parameter \"" + pDef.getName() + "\"\n";
				throw invalid_argument(errorMsg);
			}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			DoubleParam param = getDoubleParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateDoubleParam", 
				"optional double param: %s was overridden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			auto_ptr<double> defaultVal = pDef.getDefault();
			auto_ptr<string> defaultStringVal = pDef.getStringDefault();
			if(NULL != defaultVal.get()) {
				// if parameter not specified in pset, use default from psetdef 
				DoubleParam param(*pDef.getDefault(), pDef.getName(), pDef.getUnits());
				setParam(pDef.getName(), param);
				ACS_DEBUG_PARAM("ParameterSet::validateDoubleParam", 
					"adding optional, unspecified double param: %s", pDef.getName().c_str())
			}
		}
	}

	// verify that value in pset is below max and above min, if defined in psetdef
	auto_ptr<double> max = pDef.getMax();
	if(NULL != max.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateDoubleParam", "psetdef max is: %f", *max)
		// max was defined in psetdef, verify value is below max
		DoubleParam param = getDoubleParam(pDef.getName());
		double paramVal = param.getValue();
		ACS_DEBUG_PARAM("ParameterSet::validateDoubleParam", "pset max is: %f", paramVal)
		if(paramVal > *max) {
			ostringstream os;
			os << "value of \"" << paramVal << "\" exceeds max of \"" << *max 
			<< "\" defined in psetdef for double param \"" << pDef.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}

	auto_ptr<double> min = pDef.getMin();
	if(NULL != min.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateDoubleParam", "psetdef min is: %f", *min)
		// min was defined in psetdef, verify value is above min
		DoubleParam param = getDoubleParam(pDef.getName());
		double paramVal = param.getValue();
		ACS_DEBUG_PARAM("ParameterSet::validateDoubleParam", "pset min is: %f", paramVal)
		if(paramVal < *min) {
			ostringstream os;
			os << "value of \"" << paramVal << "\" is below min of \"" << *min 
			<< "\" defined in psetdef for double param \""<< pDef.getName()<< "\"\n";
			throw invalid_argument(os.str());
		}
	}
	// verify against valid values, if defined in psetdef
	auto_ptr<vector<double> > validVals = pDef.getValidValues();
	if(NULL != validVals.get() && pDef.getHasValidValues()) {
		DoubleParam param = getDoubleParam(pDef.getName());
		double paramVal = param.getValue();
		bool found = false;
		for(unsigned int j = 0; j < validVals->size(); ++j) {
			ACS_LOG(LM_DEBUG, "ParameterSet::validateDoubleParam", 
				(LM_DEBUG, "valid value[%d] is: %f and val is: %s", j, validVals->begin()[j], paramVal))
			if(false == found) {
				if(validVals->begin()[j] == paramVal) {
					found = true;
					continue;
				}
			}
		}
		if(false == found) { 
			ostringstream os;
			os << "value of \"" << paramVal << "\" is not a valid value for double param \"" << param.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}
}

void ParameterSet::validateIntArrayParam(IntArrayParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			IntArrayParam param = getIntArrayParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required int array parameter \"" + pDef.getName() + "\"\n";
			throw invalid_argument(errorMsg);
		}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			IntArrayParam param = getIntArrayParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateIntArrayParam",
				"optional int array param: %s was overridden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			auto_ptr< vector<int> > defaultVals = pDef.getDefaultValues();
			if(NULL != defaultVals.get() && 0 != defaultVals->size()) {
				// for parameter not specified in pset, use default from psetdef
				IntArrayParam param(*pDef.getDefaultValues(), pDef.getName(), pDef.getUnits());
				setParam(pDef.getName(), param);
				ACS_DEBUG_PARAM("ParameterSet::validateIntArrayParam", 
					"adding optional, unspecified int array param: %s", pDef.getName().c_str())
			}
		}
	}

	// verify that size of int array param is within maxLen, if defined in psetdef
	auto_ptr<int> maxLen = pDef.getMaxLen();
	if(NULL != maxLen.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateIntArrayParam", "psetdef maxLen is: %d", *maxLen)
		// maxLen was defined in psetdef, verify array length is below maxLen
		IntArrayParam param = getIntArrayParam(pDef.getName());
		int paramVal = param.getValues().size();
		ACS_DEBUG_PARAM("ParameterSet::validateIntArrayParam", "pset array length is: %d", paramVal)
		if(paramVal > *maxLen) {
			ostringstream os;
			os << "array length of \"" << paramVal << "\" exceeds maxLen of \"" << *maxLen
			<< "\" defined in psetdef for int array param \"" << pDef.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}
}

void ParameterSet::validateStringArrayParam(StringArrayParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			StringArrayParam param = getStringArrayParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required string array parameter \"" + pDef.getName() + "\"\n";
			throw invalid_argument(errorMsg);
		}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			StringArrayParam param = getStringArrayParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateStringArrayParam", 
				"optional string array param: %s overrriden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			auto_ptr< vector<string> > defaultVals = pDef.getDefaultValues();
			if(NULL != defaultVals.get() && 0 != defaultVals->size()) {
				// parameter not specified in pset, use default from psetdef 
				StringArrayParam param(*pDef.getDefaultValues(), pDef.getName());
				setParam(pDef.getName(), param);
				ACS_DEBUG_PARAM("ParameterSet::validateStringArrayParam", 
					"adding optional, unspecified string array param: %s", pDef.getName().c_str())
			}
		}
	}

	// verify that size of string array param is within maxLen, if defined in psetdef
	auto_ptr<int> maxLen = pDef.getMaxLen();
	if(NULL != maxLen.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateStringArrayParam", "psetdef maxLen is: %d", *maxLen)
		// maxLen was defined in psetdef, verify array length is below maxLen
		StringArrayParam param = getStringArrayParam(pDef.getName());
		int paramVal = param.getValues().size();
		ACS_DEBUG_PARAM("ParameterSet::validateStringArrayParam", "pset array length is: %d", paramVal)
		if(paramVal > *maxLen) {
			ostringstream os;
			os << "array length of \"" << paramVal << "\" exceeds maxLen of \"" << *maxLen
			<< "\" defined in psetdef for string array param \"" << pDef.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}
}

void ParameterSet::validateDoubleArrayParam(DoubleArrayParamDef pDef) 
{
	// required parameter
	if(true == pDef.isRequired()) {
		try {
			// verify that it is present in pset if required
			DoubleArrayParam param = getDoubleArrayParam(pDef.getName());
		}
		catch(domain_error dmErr) {
			string errorMsg = "ParameterSet is missing required double array parameter \"" + pDef.getName() + "\"\n";
			throw invalid_argument(errorMsg);
		}
	}
	// optional parameter
	else {
		// fill in default values in pset, if not present, using default value from psetdef
		try {
			// check to see if the param was specified (i.e. default value was overridden) 
			// in pset if so, don't need to use default
			DoubleArrayParam param = getDoubleArrayParam(pDef.getName());
			ACS_DEBUG_PARAM("ParameterSet::validateDoubleArrayParam", 
				"optional double array: %s was overridden in pset", pDef.getName().c_str())
		}
		catch(domain_error dmErr) {
			// exception caught means the value wasn't defined in the pset; 
			// default it if there is a default in psetdef
			auto_ptr< vector<double> > defaultVals = pDef.getDefaultValues();
			// for parameter not specified in pset, use default from psetdef (if present)
			if(NULL != defaultVals.get() && 0 != defaultVals->size()) {
				DoubleArrayParam param(*pDef.getDefaultValues(), pDef.getName(), pDef.getUnits());
				setParam(pDef.getName(), param);
				ACS_DEBUG_PARAM("ParameterSet::validateDoubleArrayParam", 
					"adding optional double array param: %s", pDef.getName().c_str())
			}
		}
	}

	// verify that size of double array param is within maxLen, if defined in psetdef
	auto_ptr<int> maxLen = pDef.getMaxLen();
	if(NULL != maxLen.get())
	{
		ACS_DEBUG_PARAM("ParameterSet::validateDoubleArrayParam", "psetdef maxLen: %d", *maxLen)
		// maxLen was defined in psetdef, verify array length is below maxLen
		DoubleArrayParam param = getDoubleArrayParam(pDef.getName());
		int paramVal = param.getValues().size();
		ACS_DEBUG_PARAM("ParameterSet::validateDoubleArrayParam", "pset array length: %d", paramVal)
		if(paramVal > *maxLen) {
			ostringstream os;
			os << "array length of \"" << paramVal << "\" exceeds maxLen of \"" << *maxLen
			<< "\" defined in psetdef for double array param \"" << pDef.getName() << "\"\n";
			throw invalid_argument(os.str());
		}
	}
}

void ParameterSet::initialize() 
{
	XMLPlatformUtils::Initialize();
	ACS_DEBUG("ParameterSet::initialize", "initialized XML platform")

	PSETDEF_TAG_NAME = XMLString::transcode(PSETDEF_STRING);
	PARAMETER_TAG_NAME = XMLString::transcode(PARAMETER_STRING);
	NAME_TAG_NAME = XMLString::transcode(NAME_STRING);
	VALUE_TAG_NAME = XMLString::transcode(VALUE_STRING);
	UNITS_TAG_NAME = XMLString::transcode(UNITS_STRING);

	INT_PARAM_TYPE = XMLString::transcode(INT_PARAM_STRING);
	DOUBLE_PARAM_TYPE = XMLString::transcode(DOUBLE_PARAM_STRING);
	STRING_PARAM_TYPE = XMLString::transcode(STRING_PARAM_STRING);
	BOOL_PARAM_TYPE = XMLString::transcode(BOOL_PARAM_STRING);
	INT_ARRAY_PARAM_TYPE = XMLString::transcode(INT_ARRAY_PARAM_STRING);
	DOUBLE_ARRAY_PARAM_TYPE = XMLString::transcode(DOUBLE_ARRAY_PARAM_STRING);
	STRING_ARRAY_PARAM_TYPE = XMLString::transcode(STRING_ARRAY_PARAM_STRING);
}

/**
 * Used to tell the parser where to find the schema (xsd) file.
 * Normally, we will check 1. TASK_DIR environment variable, then 2. ACSROOT/config,
 * then 3. INTROOT/config; if these aren't defined, the parser will just use whatever is in the 
 * schemaLocation hint in the xml file (if present).
 */
void ParameterSet::setSchemaLocation(DOMBuilder * parser)
{
	// set the schema location if the environment variable is defined
	char *xsdDir = getenv(TASK_DIR_ENV_VAR_NAME);
	if(NULL != xsdDir) {
		string schemaDirStr(xsdDir);
		schemaDirStr.append(SLASH_STRING).append(SCHEMA_SUBDIR_NAME);
		string testSchemaLocation(PSET_NAMESPACE_URI);
		testSchemaLocation.append(SPACE_STRING).append(schemaDirStr).append(SLASH_STRING).append(PSET_SCHEMA_FILE_NAME);
		StrX schemaLocationStrX(testSchemaLocation.c_str());
		parser->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, (void*)schemaLocationStrX.unicodeForm());
	}
	// else use ACSROOT/config if ACSROOT is defined
	xsdDir = getenv(ACSROOT_ENV_VAR_NAME);
	if(NULL != xsdDir) {
		string schemaDirStr(xsdDir);
		schemaDirStr.append(SLASH_STRING).append(CONFIG_SUBDIR_NAME);
		string testSchemaLocation(PSET_NAMESPACE_URI);
		testSchemaLocation.append(SPACE_STRING).append(schemaDirStr).append(SLASH_STRING).append(PSET_SCHEMA_FILE_NAME);
		StrX schemaLocationStrX(testSchemaLocation.c_str());
		parser->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, (void*)schemaLocationStrX.unicodeForm());
	}
	// else use INTROOT/config if INTROOT is defined
	xsdDir = getenv(INTROOT_ENV_VAR_NAME);
	if(NULL != xsdDir) {
		string schemaDirStr(xsdDir);
		schemaDirStr.append(SLASH_STRING).append(CONFIG_SUBDIR_NAME);
		string testSchemaLocation(PSET_NAMESPACE_URI);
		testSchemaLocation.append(SPACE_STRING).append(schemaDirStr).append(SLASH_STRING).append(PSET_SCHEMA_FILE_NAME);
		StrX schemaLocationStrX(testSchemaLocation.c_str());
		parser->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, (void*)schemaLocationStrX.unicodeForm());
	}
	// else if none of the above environment variables was defined, 
	// we will rely on the XML instance document's schemalocation hint
}

