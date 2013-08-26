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
* "@(#) $Id: ParamSetDef.cpp,v 1.17 2010/04/27 12:20:58 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <fstream>
#include <iterator>
#include <memory>
#include <vector>

#include <ParamSetDef.h>
#include <acsDOMErrorHandler.h>
#include <ParamStrX.h>

#include <xercesc/dom/DOM.hpp>
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

typedef map< string, IntParamDef >::value_type intParamDefValType;
typedef map< string, DoubleParamDef >::value_type doubleParamDefValType;
typedef map< string, StringParamDef >::value_type stringParamDefValType;
typedef map< string, IntArrayParamDef >::value_type intArrayParamDefValType;
typedef map< string, DoubleArrayParamDef >::value_type doubleArrayParamDefValType;
typedef map< string, StringArrayParamDef >::value_type stringArrayParamDefValType;
typedef map< string, BoolParamDef >::value_type boolParamDefValType;

/**
 * Constructor.
 * 
 * @param xmlFile the string containing a filename 
 * which contains the XML representation of the parameter set for a task.
 */
ParamSetDef::ParamSetDef(const string & xmlFile)
{
	XMLPlatformUtils::Initialize();
	PARAMETER_TAG_NAME = XMLString::transcode(PARAMETER_TAG_NAME_STR);
	NAME_TAG_NAME = XMLString::transcode(NAME_TAG_NAME_STR);
	REQUIRED_TAG_NAME = XMLString::transcode(REQUIRED_TAG_NAME_STR);
	PROMPT_TAG_NAME = XMLString::transcode(PROMPT_TAG_NAME_STR);
	HELP_TAG_NAME = XMLString::transcode(HELP_TAG_NAME_STR);
	DEFAULT_TAG_NAME = XMLString::transcode(DEFAULT_TAG_NAME_STR);
	STRING_DEFAULT_TAG_NAME = XMLString::transcode(STRING_DEFAULT_TAG_NAME_STR);
	LENGTH_TAG_NAME = XMLString::transcode(LENGTH_TAG_NAME_STR);
	VALID_VALUES_TAG_NAME = XMLString::transcode(VALID_VALUES_TAG_NAME_STR);
	VALUE_TAG_NAME = XMLString::transcode(VALUE_TAG_NAME_STR);
	MAX_TAG_NAME = XMLString::transcode(MAX_TAG_NAME_STR);
	MIN_TAG_NAME = XMLString::transcode(MIN_TAG_NAME_STR);
	UNITS_TAG_NAME = XMLString::transcode(UNITS_TAG_NAME_STR);
	MAXLEN_TAG_NAME = XMLString::transcode(MAXLEN_TAG_NAME_STR);

	INT_PARAM_TYPE = XMLString::transcode(INT_PARAM_TYPE_STR);
	DOUBLE_PARAM_TYPE = XMLString::transcode(DOUBLE_PARAM_TYPE_STR);
	STRING_PARAM_TYPE = XMLString::transcode(STRING_PARAM_TYPE_STR);
	BOOL_PARAM_TYPE = XMLString::transcode(BOOL_PARAM_TYPE_STR);
	INT_ARRAY_PARAM_TYPE = XMLString::transcode(INT_ARRAY_PARAM_TYPE_STR);
	DOUBLE_ARRAY_PARAM_TYPE = XMLString::transcode(DOUBLE_ARRAY_PARAM_TYPE_STR);
	STRING_ARRAY_PARAM_TYPE = XMLString::transcode(STRING_ARRAY_PARAM_TYPE_STR);

	parseFile(xmlFile);
}

/**
 * Destructor.
 */
ParamSetDef::~ParamSetDef()
{
	XMLString::release(&PARAMETER_TAG_NAME);
	XMLString::release(&NAME_TAG_NAME);
	XMLString::release(&REQUIRED_TAG_NAME);
	XMLString::release(&PROMPT_TAG_NAME);
	XMLString::release(&HELP_TAG_NAME);
	XMLString::release(&DEFAULT_TAG_NAME);
	XMLString::release(&STRING_DEFAULT_TAG_NAME);
	XMLString::release(&LENGTH_TAG_NAME);
	XMLString::release(&VALID_VALUES_TAG_NAME);
	XMLString::release(&VALUE_TAG_NAME);
	XMLString::release(&MAX_TAG_NAME);
	XMLString::release(&MIN_TAG_NAME);
	XMLString::release(&UNITS_TAG_NAME);
	XMLString::release(&MAXLEN_TAG_NAME);

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
 * Gets the type of a parameter by name.
 * @param paramName the name of the parameter for which the type is desired.
 * @return the type as a ParamSetDef::paramType enum.
 */
ParamSetDef::paramTypesEnum ParamSetDef::getParamTypeForName(string paramName) 
{
	paramTypesEnum retValue = BOOL;
	bool found = false; 
	try {
		getBoolParamDef(paramName);
		found = true;
		retValue = BOOL;
	}
	catch(domain_error domainErr) {
		// not yet found
		found = false;
	}
	if(false == found) {
		try {
			getIntParamDef(paramName);
			found = true;
			retValue = INT;
		}
		catch(domain_error domainErr) {
			// not yet found
			found = false;
		}
	}
	if(false == found) {
		try {
			getIntArrayParamDef(paramName);
			found = true;
			retValue = INT_ARRAY;
		}
		catch(domain_error domainErr) {
			// not yet found
			found = false;
		}
	}
	if(false == found) {
		try {
			getDoubleParamDef(paramName);
			found = true;
			retValue = DOUBLE;
		}
		catch(domain_error domainErr) {
			// not yet found
			found = false;
		}
	}
	if(false == found) {
		try {
			getDoubleArrayParamDef(paramName);
			found = true;
			retValue = DOUBLE_ARRAY;
		}
		catch(domain_error domainErr) {
			// not yet found
			found = false;
		}
	}
	if(false == found) {
		try {
			getStringParamDef(paramName);
			found = true;
			retValue = STRING;
		}
		catch(domain_error domainErr) {
			// not yet found
			found = false;
		}
	}
	if(false == found) {
		try {
			getStringArrayParamDef(paramName);
			found = true;
			retValue = STRING_ARRAY;
		}
		catch(domain_error domainErr) {
			// not yet found
			found = false;
		}
	}

	if(false == found) {
		string errMsg = "No entry for parameter: " + paramName + "\n";
		ACS_DEBUG_PARAM("ParamSetDef::getParamTypeForName", "No entry for parameter: %s", paramName.c_str())
		throw domain_error(errMsg);
	}
	return retValue;
}

/**
 * get an int param by name.
 * @param paramName the name of the parameter desired.
 */
IntParamDef ParamSetDef::getIntParamDef(string paramName) 
{
   IntParamDef retVal;
   map<string, IntParamDef>::iterator mapEntry = intParamDefMap.find(paramName);
   if(mapEntry != intParamDefMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get a double param by name.
 * @param paramName the name of the parameter desired.
 */
DoubleParamDef ParamSetDef::getDoubleParamDef(string paramName) 
{
   DoubleParamDef retVal;
   map<string, DoubleParamDef >::iterator mapEntry = doubleParamDefMap.find(paramName);
   if(mapEntry != doubleParamDefMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get an string param by name.
 * @param paramName the name of the parameter desired.
 */
StringParamDef ParamSetDef::getStringParamDef(string paramName) 
{
   StringParamDef retVal;
   map<string, StringParamDef >::iterator mapEntry = stringParamDefMap.find(paramName);
   if(mapEntry != stringParamDefMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get an bool param by name.
 * @param paramName the name of the parameter desired.
 */
BoolParamDef ParamSetDef::getBoolParamDef(string paramName) 
{
   BoolParamDef retVal;
   map<string, BoolParamDef >::iterator mapEntry = boolParamDefMap.find(paramName);
   if(mapEntry != boolParamDefMap.end()) 
   {
      retVal = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retVal;
}

/**
 * get an array of int params by name
 * @param paramName the name of the parameter desired.
 */
IntArrayParamDef ParamSetDef::getIntArrayParamDef(string paramName) 
{
   IntArrayParamDef retArray;
   map<string, IntArrayParamDef >::iterator mapEntry = intArrayParamDefMap.find(paramName);
   if(mapEntry != intArrayParamDefMap.end()) 
   {
      retArray = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retArray;
}

/**
 * get an array of double params by name
 * @param paramName the name of the parameter desired.
 */
DoubleArrayParamDef ParamSetDef::getDoubleArrayParamDef(string paramName) 
{
   DoubleArrayParamDef retArray;
   map<string, DoubleArrayParamDef >::iterator mapEntry = doubleArrayParamDefMap.find(paramName);
   if(mapEntry != doubleArrayParamDefMap.end()) 
   {
      retArray = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retArray;
}

/**
 * get an array of string param by name
 * @param paramName the name of the parameter desired.
 */
StringArrayParamDef ParamSetDef::getStringArrayParamDef(string paramName) 
{
   StringArrayParamDef retArray;
   map<string, StringArrayParamDef >::iterator mapEntry = stringArrayParamDefMap.find(paramName);
   if(mapEntry != stringArrayParamDefMap.end()) 
   {
      retArray = (*mapEntry).second;
   }
   else 
   {
     string errorMsg = "No entry for paramdef \"" + paramName + "\" was found";
     throw domain_error(errorMsg);
   }
   return retArray;
}

/**
 * Private method to orchestrate the XML parsing.
 */
int ParamSetDef::parseFile(const string & xmlFile)
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
			ACS_DEBUG_PARAM("ParamSetDef::parseFile", "Appended TASK_DIR to file name: %s", fileToParse.c_str())
		}
		else {
			string errorMsg = "ParamSetDef::parseFile - cannot locate file of name: " + xmlFile;
			throw invalid_argument(errorMsg);
		}
	}
	return parseDOM(fileToParse);
}

/**
 * Private method to orchestrate the XML parsing using DOM.
 */
int ParamSetDef::parseDOM(const string & xmlFile)
{
	bool doNamespaces = true;
	bool doSchema = true;
	bool schemaFullChecking = true;

	// Instantiate the DOM parser.
	static const XMLCh gLS[] = { chLatin_L, chLatin_S, chNull };
	DOMImplementation *impl = DOMImplementationRegistry::getDOMImplementation(gLS);
	DOMBuilder        *parser = ((DOMImplementationLS*)impl)->createDOMBuilder(DOMImplementationLS::MODE_SYNCHRONOUS, 0);

	parser->setFeature(XMLUni::fgDOMNamespaces, doNamespaces);
	parser->setFeature(XMLUni::fgXercesSchema, doSchema);
	parser->setFeature(XMLUni::fgXercesSchemaFullChecking, schemaFullChecking);
	parser->setFeature(XMLUni::fgDOMValidation, true);
	parser->setFeature(XMLUni::fgXercesDOMHasPSVIInfo, true);

	setSchemaLocation(parser);

	// enable datatype normalization - default is off
	parser->setFeature(XMLUni::fgDOMDatatypeNormalization, true);

	/*******************
	// optionally you can implement your DOMErrorHandler (e.g. acsDOMErrorHandler)
	// and set it to the builder
	*******************/
	acsDOMErrorHandler* errHandler = new acsDOMErrorHandler();
	parser->setErrorHandler(errHandler);

	DOMDocument *doc = 0;
	
	try {
		doc = parser->parseURI(xmlFile.c_str());
		ACS_DEBUG_PARAM("ParamSetDef::parseDOM", "Parsed file: %s", xmlFile.c_str())

		// Get all parameters
		DOMNodeList * paramNodes = doc->getElementsByTagName(PARAMETER_TAG_NAME);
		if(NULL != paramNodes) {
			processParamDefNodes(paramNodes);
		}
	}
	catch (const XMLException& toCatch) {
		char* message = XMLString::transcode(toCatch.getMessage());
		ACS_LOG(LM_ERROR, "ParamSetDef::parseDOM",  
			(LM_ERROR, "***** XMLException ***** \n\n Message: \n\n %s *****\n", message))
		XMLString::release(&message);
		return -1;
	}
	catch (const DOMException& toCatch) {
		char* message = XMLString::transcode(toCatch.msg);
		ACS_LOG(LM_ERROR, "ParamSetDef::parseDOM",  
			(LM_ERROR, "***** DOMException ***** \n\n Message: \n\n %s *****\n", message))
		XMLString::release(&message);
		return -1;
	}
	catch (const std::domain_error& toCatch) {
		ACS_LOG(LM_ERROR, "ParamSetDef::parseDOM",  
			(LM_ERROR, "***** Error in parameter set definition: ***** \n\n %s ****\n", 
			toCatch.what()))
		return -1;
	}
	catch (...) {
		ACS_LOG(LM_ERROR, "ParamSetDef::parseDOM",  (LM_ERROR, "***** Unexpected exception in parameter set definition: ***** \n"))
		return -1;
	}

	parser->release();
	delete errHandler;
	return 0;
}


/**
 * Private method to orchestrate the XML parsing using SAX.
 */
int ParamSetDef::parseSAX(const string & xmlFile)
{
	// TODO - implement this, if desired to parse with SAX
	return 0;
}

/**
 * Private method to handle a BoolParamDef
 */
void ParamSetDef::handleBoolParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleBoolParamDef", "bool param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "bool paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleBoolParamDef", "required value: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "bool parameter: \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText;
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleBoolParamDef", "help value: %s", helpText.c_str())
	}
	else {
		string errorMsg = "bool parameter: \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleBoolParamDef", "prompt value: %s", promptText.c_str())
	}
	else {
		string errorMsg = "bool parameter: \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the default value
	DOMNodeList *defaultNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	auto_ptr< bool > defaultValue;
	if(NULL != defaultNodes) 
	{
		DOMNode *defaultNode = defaultNodes->item(0);
		if(NULL != defaultNode)
		{
			DOMNode *defaultTextNode = defaultNode->getFirstChild();
			if(NULL != defaultTextNode) {
				tmpString = XMLString::transcode(defaultTextNode->getTextContent());
			}
			else {
				string errorMsg = "bool paramdef \"" + paramName + "\" must have non-null default";
				throw domain_error(errorMsg);
			}
			bool defaultBoolVal = ((0 == XMLString::compareIString(tmpString, TRUE_STRING))
				|| (0 == XMLString::compareIString(tmpString, ONE_STRING)) ) ? true : false;
			XMLString::release(&tmpString);
			defaultValue.reset(new bool(defaultBoolVal));
			ACS_DEBUG_PARAM("ParamSetDef::handleBoolParamDef", "default value: %d", defaultBoolVal)
		}
	}

	// add a new BoolParamDef to the ParamSetDef
	BoolParamDef newBoolParamDef(paramName, helpText, promptText, requiredBoolVal, defaultValue);
	boolParamDefMap.insert(boolParamDefValType(paramName, newBoolParamDef));
	ACS_DEBUG_PARAM("ParamSetDef::handleBoolParamDef", "inserted boolparam: %s into object model", paramName.c_str())
}

/**
 * Private method to handle an IntParamDef
 */
void ParamSetDef::handleIntParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "int param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "int paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "required string: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "int paramdef \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "required value: %d", requiredBoolVal)

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText;
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "help value: %s", helpText.c_str())
	}
	else {
		string errorMsg = "int paramdef \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "prompt value: %s", promptText.c_str())
	}
	else {
		string errorMsg = "int parameter: \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the default value
	DOMNodeList *defaultNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	auto_ptr< int > defaultValue;
	if(NULL != defaultNodes) {
		DOMNode *defaultNode = defaultNodes->item(0);
		if(NULL != defaultNode) {
			DOMNode *defaultTextNode = defaultNode->getFirstChild();
			if(NULL != defaultTextNode) {
				defaultValue.reset(new int(XMLString::parseInt(defaultTextNode->getTextContent())));
			}
			else {
				string errorMsg = "int paramdef \"" + paramName + "\" must have non-null default";
				throw domain_error(errorMsg);
			}
		}
	}

	// 6. get the stringdefault text
	DOMNodeList *strDefaultNodes = paramElem->getElementsByTagName(STRING_DEFAULT_TAG_NAME);
	auto_ptr< string > strDefaultText;
	if(NULL != strDefaultNodes) {
		DOMNode *strDefaultNode = strDefaultNodes->item(0);
		if(NULL != strDefaultNode) {
			DOMNode *strDefaultTextNode = strDefaultNode->getFirstChild();
			if(NULL != strDefaultTextNode) {
				tmpString = XMLString::transcode(strDefaultTextNode->getTextContent());
				strDefaultText.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "string default: %s", strDefaultText->c_str())
			}
			else {
				string errorMsg = "int paramdef \"" + paramName + "\" must have non-null string default";
				throw domain_error(errorMsg);
			}
		}
	}

	// 7. get the units text
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr< string > unitsText;
	if(NULL != unitsNodes) {
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) {
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				unitsText.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "units: %s", unitsText->c_str())
			}
			else {
				string errorMsg = "int paramdef \"" + paramName + "\" must have non-null units";
				throw domain_error(errorMsg);
			}
		}
	}

	// 8. get the max value
	DOMNodeList *maxNodes = paramElem->getElementsByTagName(MAX_TAG_NAME);
	auto_ptr< int > maxValue;
	if(NULL != maxNodes) {
		DOMNode *maxNode = maxNodes->item(0);
		if(NULL != maxNode) {
			DOMNode *maxTextNode = maxNode->getFirstChild();
			if(NULL != maxTextNode) {
				maxValue.reset(new int(XMLString::parseInt(maxTextNode->getTextContent())));
				ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "max value: %d", *maxValue)
			}
			else {
				string errorMsg = "int paramdef \"" + paramName + "\" must have non-null max";
				throw domain_error(errorMsg);
			}
		}
	}

	// 9. get the min value
	DOMNodeList *minNodes = paramElem->getElementsByTagName(MIN_TAG_NAME);
	auto_ptr< int > minValue;
	if(NULL != minNodes) {
		DOMNode *minNode = minNodes->item(0);
		if(NULL != minNode) {
			DOMNode *minTextNode = minNode->getFirstChild();
			if(NULL != minTextNode) {
				minValue.reset(new int(XMLString::parseInt(minTextNode->getTextContent())));
				ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "min value: %d", *minValue)
			}
			else {
				string errorMsg = "int paramdef \"" + paramName + "\" must have non-null min";
				throw domain_error(errorMsg);
			}
		}
	}

	// 10. get the valid values
	auto_ptr< vector<int> > validVals;
	DOMNodeList *validValNodes = paramElem->getElementsByTagName(VALID_VALUES_TAG_NAME);
	if(0 != validValNodes->getLength()) {
		validVals.reset(new(vector<int>)); 
		DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
		for (unsigned int i = 0; i < valueNodes->getLength(); i++) {
			DOMNode *valNode = valueNodes->item(i);
			DOMNode *valTextNode = valNode->getFirstChild();
			if(NULL != valTextNode) {
				int value = XMLString::parseInt(valTextNode->getTextContent());
				ACS_DEBUG_PARAM("ParamSetDef::handleIntParamDef", "valid value: %d", value)
				validVals->push_back(value);
			}
			else {
				string errorMsg = "int paramdef \"" + paramName + "\" must have non-null valid value(s)";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new IntParamDef to the ParamSetDef
	IntParamDef newIntParamDef(paramName, helpText, promptText, requiredBoolVal,
		defaultValue, strDefaultText, unitsText, maxValue, minValue, validVals);

	intParamDefMap.insert( intParamDefValType(paramName, newIntParamDef) );
}

/**
 * Private method to handle an IntArrayParamDef
 */
void ParamSetDef::handleIntArrayParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "int array param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "int array paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "required string: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "int array parameter: \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "required value: %d", requiredBoolVal)

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText;
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "help text: %s", helpText.c_str())
	}
	else {
		string errorMsg = "int array parameter: \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "prompt text: %s", promptText.c_str())
	}
	else {
		string errorMsg = "int array parameter: \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the maxlen
	DOMNodeList *maxLenNodes = paramElem->getElementsByTagName(MAXLEN_TAG_NAME);
	auto_ptr< int > maxLen;
	if(NULL != maxLenNodes) 
	{
		DOMNode *maxLenNode = maxLenNodes->item(0);
		if(NULL != maxLenNode) 
		{
			DOMNode *maxLenTextNode = maxLenNode->getFirstChild();
			if(NULL != maxLenTextNode) {
				int maxLenValue = XMLString::parseInt(maxLenTextNode->getTextContent());
				maxLen.reset(new int(maxLenValue));
				ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "maxLen: %d", *maxLen)
			}
			else {
				string errorMsg = "int array parameter: \"" + paramName + "\" must have non-null maxLen value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 6. get the units text
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr< string > unitsText;
	if(NULL != unitsNodes) 
	{
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) 
		{
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				unitsText.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "units: %s", unitsText->c_str())
			}
			else {
				string errorMsg = "int array parameter: \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 7. get the default value(s)
	auto_ptr< vector<int> > defaultVals(new vector<int>);
	DOMNodeList *defaultValNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	for (unsigned int i = 0; i < defaultValNodes->getLength(); i++) {
		DOMNode *defaultValNode = defaultValNodes->item(i);
		DOMNode *defaultValTextNode = defaultValNode->getFirstChild();
		if(NULL != defaultValTextNode) {
			int defaultValValue = XMLString::parseInt(defaultValTextNode->getTextContent());
			ACS_DEBUG_PARAM("ParamSetDef::handleIntArrayParamDef", "default value: %d", defaultValValue)
			defaultVals->push_back(defaultValValue);
		}
		else {
			string errorMsg = "int array parameter: \"" + paramName + "\" must have non-null default value(s)";
			throw domain_error(errorMsg);
		}
	}

	// add a new IntArrayParamDef to the ParamSetDef
	IntArrayParamDef newIntArrayParamDef(paramName, helpText, promptText, requiredBoolVal,
		unitsText, maxLen, defaultVals);
	intArrayParamDefMap.insert( intArrayParamDefValType(paramName, newIntArrayParamDef) );
}

/**
 * Private method to handle a DoubleParamDef
 */
void ParamSetDef::handleDoubleParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "double param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "double paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "required string: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "double paramdef \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "required value: %d", requiredBoolVal)

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText;
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "help text: %s", helpText.c_str())
	}
	else {
		string errorMsg = "double paramdef \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "prompt text: %s", promptText.c_str())
	}
	else {
		string errorMsg = "double paramdef \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the default value
	DOMNodeList *defaultNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	auto_ptr< double > defaultValue;
	if(NULL != defaultNodes) {
		DOMNode *defaultNode = defaultNodes->item(0);
		if(NULL != defaultNode) {
			DOMNode *defaultTextNode = defaultNode->getFirstChild();
			if(NULL != defaultTextNode) {
				XMLDouble defaultXmlDouble(defaultTextNode->getTextContent());
				double defaultDoubleVal = defaultXmlDouble.getValue();
				defaultValue.reset(new double(defaultDoubleVal));
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "default value: %f", *defaultValue)
			}
			else {
				string errorMsg = "double paramdef \"" + paramName + "\" must have non-null default value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 6. get the stringdefault text
	DOMNodeList *strDefaultNodes = paramElem->getElementsByTagName(STRING_DEFAULT_TAG_NAME);
	auto_ptr< string > strDefaultText;
	if(NULL != strDefaultNodes) {
		DOMNode *strDefaultNode = strDefaultNodes->item(0);
		if(NULL != strDefaultNode) {
			DOMNode *strDefaultTextNode = strDefaultNode->getFirstChild();
			if(NULL != strDefaultTextNode) {
				tmpString = XMLString::transcode(strDefaultTextNode->getTextContent());
				strDefaultText.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "string default: %s", strDefaultText->c_str())
			}
			else {
				string errorMsg = "double paramdef \"" + paramName + "\" must have non-null string default value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 7. get the units text
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr< string > unitsText;
	if(NULL != unitsNodes) {
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) {
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				unitsText.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "units: %s", unitsText->c_str())
			}
			else {
				string errorMsg = "double paramdef \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 8. get the max value
	DOMNodeList *maxNodes = paramElem->getElementsByTagName(MAX_TAG_NAME);
	auto_ptr< double > maxValue;
	if(NULL != maxNodes) {
		DOMNode *maxNode = maxNodes->item(0);
		if(NULL != maxNode) {
			DOMNode *maxTextNode = maxNode->getFirstChild();
			if(NULL != maxTextNode) {
				XMLDouble maxXmlDouble(maxTextNode->getTextContent());
				double maxDoubleVal = maxXmlDouble.getValue();
				maxValue.reset(new double(maxDoubleVal));
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "max value: %f", *maxValue)
			}
			else {
				string errorMsg = "double paramdef \"" + paramName + "\" must have non-null max value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 9. get the min value
	DOMNodeList *minNodes = paramElem->getElementsByTagName(MIN_TAG_NAME);
	auto_ptr< double > minValue;
	if(NULL != minNodes) {
		DOMNode *minNode = minNodes->item(0);
		if(NULL != minNode) {
			DOMNode *minTextNode = minNode->getFirstChild();
			if(NULL != minTextNode) {
				XMLDouble minXmlDouble(minTextNode->getTextContent());
				double minDoubleVal = minXmlDouble.getValue();
				minValue.reset(new double(minDoubleVal));
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "min value: %f", *minValue)
			}
			else {
				string errorMsg = "double paramdef \"" + paramName + "\" must have non-null min value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 10. get the valid values
	auto_ptr< vector<double> > validVals;
	DOMNodeList *validValNodes = paramElem->getElementsByTagName(VALID_VALUES_TAG_NAME);
	if(0 != validValNodes->getLength()) {
		validVals.reset(new(vector<double>)); 
		DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
		for (unsigned int i = 0; i < valueNodes->getLength(); i++) {
			DOMNode *valueNode = valueNodes->item(i);
			DOMNode *valueTextNode = valueNode->getFirstChild();
			if(NULL != valueTextNode) {
				XMLDouble valXmlDouble(valueTextNode->getTextContent());
				double validValue = valXmlDouble.getValue();
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleParamDef", "valid value: %f", validValue)
				validVals->push_back(validValue);
			}
			else {
				string errorMsg = "double paramdef \"" + paramName + "\" must have non-null valid value(s)";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new DoubleParamDef to the ParamSetDef
	DoubleParamDef newDoubleParamDef(paramName, helpText, promptText, requiredBoolVal,
		defaultValue, strDefaultText, unitsText, maxValue, minValue, validVals);

	doubleParamDefMap.insert( doubleParamDefValType(paramName, newDoubleParamDef) );
}

/**
 * Private method to handle a DoubleArrayParamDef
 */
void ParamSetDef::handleDoubleArrayParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "double array param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "double array paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "required string: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "double array paramdef \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "required value: %d", requiredBoolVal)

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText;
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "help text: %s", helpText.c_str())
	}
	else {
		string errorMsg = "double array paramdef \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "prompt text: %s", promptText.c_str())
	}
	else {
		string errorMsg = "double array paramdef \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the maxlen
	DOMNodeList *maxLenNodes = paramElem->getElementsByTagName(MAXLEN_TAG_NAME);
	auto_ptr< int > maxLen;
	if(NULL != maxLenNodes) 
	{
		DOMNode *maxLenNode = maxLenNodes->item(0);
		if(NULL != maxLenNode) 
		{
			DOMNode *maxLenTextNode = maxLenNode->getFirstChild();
			if(NULL != maxLenTextNode) {
				int maxLenValue = XMLString::parseInt(maxLenTextNode->getTextContent());
				maxLen.reset(new int(maxLenValue));
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "maxLen: %d", *maxLen)
			}
			else {
				string errorMsg = "double array paramdef \"" + paramName + "\" must have non-null maxLen value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 6. get the units text
	DOMNodeList *unitsNodes = paramElem->getElementsByTagName(UNITS_TAG_NAME);
	auto_ptr< string > unitsText;
	if(NULL != unitsNodes) 
	{
		DOMNode *unitsNode = unitsNodes->item(0);
		if(NULL != unitsNode) 
		{
			DOMNode *unitsTextNode = unitsNode->getFirstChild();
			if(NULL != unitsTextNode) {
				tmpString = XMLString::transcode(unitsTextNode->getTextContent());
				unitsText.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "units: %s", unitsText->c_str())
			}
			else {
				string errorMsg = "double array paramdef \"" + paramName + "\" must have non-null units value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 7. get the default value(s)
	auto_ptr< vector<double> > defaultVals(new vector<double>);
	DOMNodeList *defaultValNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	for (unsigned int i = 0; i < defaultValNodes->getLength(); i++) {
		DOMNode *defaultValNode = defaultValNodes->item(i);
		DOMNode *defaultValTextNode = defaultValNode->getFirstChild();
		if(NULL != defaultValTextNode) {
			XMLDouble defaultXmlDouble(defaultValTextNode->getTextContent());
			double defaultValValue = defaultXmlDouble.getValue();
			ACS_DEBUG_PARAM("ParamSetDef::handleDoubleArrayParamDef", "default value: %f", defaultValValue)
			defaultVals->push_back(defaultValValue);
		}
		else {
			string errorMsg = "double array paramdef \"" + paramName + "\" must have non-null default value(s)";
			throw domain_error(errorMsg);
		}
	}

	// add a new DoubleArrayParamDef to the ParamSetDef
	DoubleArrayParamDef newDoubleArrayParamDef(paramName, helpText, promptText, requiredBoolVal,
		unitsText, maxLen, defaultVals);
	doubleArrayParamDefMap.insert( doubleArrayParamDefValType(paramName, newDoubleArrayParamDef) );
}

/**
 * Private method to handle a StringParamDef
 */
void ParamSetDef::handleStringParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "string param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "string paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "required string: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "string paramdef \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "required value: %d", requiredBoolVal)

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText;
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "help text: %s", helpText.c_str())
	}
	else {
		string errorMsg = "string paramdef \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "prompt text: %s", promptText.c_str())
	}
	else {
		string errorMsg = "string paramdef \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the default value
	DOMNodeList *defaultNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	auto_ptr < string > defaultString;
	if(NULL != defaultNodes) {
		DOMNode *defaultNode = defaultNodes->item(0);
		if(NULL != defaultNode) {
			DOMNode *defaultTextNode = defaultNode->getFirstChild();
			if(NULL != defaultTextNode) {
				tmpString = XMLString::transcode(defaultTextNode->getTextContent());
				defaultString.reset(new string(tmpString));
				XMLString::release(&tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "default: %s", defaultString->c_str())
			}
			else {
				string errorMsg = "string paramdef \"" + paramName + "\" must have non-null default value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 6. get the valid values
	auto_ptr< vector<string> > validVals;
	DOMNodeList *validValNodes = paramElem->getElementsByTagName(VALID_VALUES_TAG_NAME);
	if(0 != validValNodes->getLength()) {
		validVals.reset(new(vector<string>)); 
		DOMNodeList *valueNodes = paramElem->getElementsByTagName(VALUE_TAG_NAME);
		for (unsigned int i = 0; i < valueNodes->getLength(); i++) {
			DOMNode *valueNode = valueNodes->item(i);
			DOMNode *valueTextNode = valueNode->getFirstChild();
			if(NULL != valueTextNode) {
				tmpString = XMLString::transcode(valueTextNode->getTextContent());
				string validValue(tmpString);
				ACS_DEBUG_PARAM("ParamSetDef::handleStringParamDef", "valid value: %s", validValue.c_str())
				XMLString::release(&tmpString);
				validVals->push_back(validValue);
			}
			else {
				string errorMsg = "string paramdef \"" + paramName + "\" must have non-null valid value(s)";
				throw domain_error(errorMsg);
			}
		}
	}

	// add a new StringParamDef to the ParamSetDef
	StringParamDef newStringParamDef(paramName, helpText, promptText, 
		requiredBoolVal, defaultString, validVals);
	stringParamDefMap.insert( stringParamDefValType(paramName, newStringParamDef) );
}

/**
 * Private method to handle a StringArrayParamDef
 */
void ParamSetDef::handleStringArrayParamDef(DOMElement *paramElem) 
{
	// 1. get the name
	DOMNodeList *nameNodes = paramElem->getElementsByTagName(NAME_TAG_NAME);
	DOMNode *nameNode = nameNodes->item(0);
	DOMNode *nameTextNode = nameNode->getFirstChild();
	string paramName;
	char *tmpString = NULL;
	if(NULL != nameTextNode) {
		tmpString = XMLString::transcode(nameTextNode->getTextContent());
		paramName = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "string array param def name: %s", paramName.c_str())
	}
	else {
		string errorMsg = "string array paramdef must have non-null name";
		throw domain_error(errorMsg);
	}

	// 2. get the required flag
	DOMNodeList *requiredNodes = paramElem->getElementsByTagName(REQUIRED_TAG_NAME);
	DOMNode *requiredNode = requiredNodes->item(0);
	DOMNode *requiredTextNode = requiredNode->getFirstChild();
	string requiredString;
	if(NULL != requiredTextNode) {
		tmpString = XMLString::transcode(requiredTextNode->getTextContent());
		requiredString = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "required string: %s", requiredString.c_str())
	}
	else {
		string errorMsg = "string array paramdef \"" + paramName + "\" must have non-null required value";
		throw domain_error(errorMsg);
	}
	bool requiredBoolVal = ((0 == XMLString::compareIString(requiredString.c_str(), TRUE_STRING))
		|| (0 == XMLString::compareIString(requiredString.c_str(), ONE_STRING)) ) ? true : false;
	ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "required value: %d", requiredBoolVal)

	// 3. get the help text
	DOMNodeList *helpNodes = paramElem->getElementsByTagName(HELP_TAG_NAME);
	DOMNode *helpNode = helpNodes->item(0);
	DOMNode *helpTextNode = helpNode->getFirstChild();
	string helpText; 
	if(NULL != helpTextNode) {
		tmpString = XMLString::transcode(helpTextNode->getTextContent());
		helpText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "help text: %s", helpText.c_str())
	}
	else {
		string errorMsg = "string array paramdef \"" + paramName + "\" must have non-null help value";
		throw domain_error(errorMsg);
	}

	// 4. get the prompt text
	DOMNodeList *promptNodes = paramElem->getElementsByTagName(PROMPT_TAG_NAME);
	DOMNode *promptNode = promptNodes->item(0);
	DOMNode *promptTextNode = promptNode->getFirstChild();
	string promptText;
	if(NULL != promptTextNode) {
		tmpString = XMLString::transcode(promptTextNode->getTextContent());
		promptText = string(tmpString);
		XMLString::release(&tmpString);
		ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "prompt text: %s", promptText.c_str())
	}
	else {
		string errorMsg = "string array paramdef \"" + paramName + "\" must have non-null prompt value";
		throw domain_error(errorMsg);
	}

	// 5. get the maxlen
	DOMNodeList *maxLenNodes = paramElem->getElementsByTagName(MAXLEN_TAG_NAME);
	auto_ptr< int > maxLen;
	if(NULL != maxLenNodes) 
	{
		DOMNode *maxLenNode = maxLenNodes->item(0);
		if(NULL != maxLenNode) 
		{
			DOMNode *maxLenTextNode = maxLenNode->getFirstChild();
			if(NULL != maxLenTextNode) {
				int maxLenValue = XMLString::parseInt(maxLenTextNode->getTextContent());
				maxLen.reset(new int(maxLenValue));
				ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "maxLen: %d", *maxLen) 
			}
			else {
				string errorMsg = "string array paramdef \"" + paramName + "\" must have non-null maxLen value";
				throw domain_error(errorMsg);
			}
		}
	}

	// 6. get the default value(s)
	auto_ptr< vector<string> > defaultVals(new vector<string>);
	DOMNodeList *defaultValNodes = paramElem->getElementsByTagName(DEFAULT_TAG_NAME);
	for (unsigned int i = 0; i < defaultValNodes->getLength(); i++) {
		DOMNode *defaultValNode = defaultValNodes->item(i);
		DOMNode *defaultValTextNode = defaultValNode->getFirstChild();
		string defaultStr;
		if(NULL != defaultValTextNode) {
			tmpString = XMLString::transcode(defaultValTextNode->getTextContent());
			defaultStr = string(tmpString);
			XMLString::release(&tmpString);
			ACS_DEBUG_PARAM("ParamSetDef::handleStringArrayParamDef", "default value: %s", defaultStr.c_str()) 
			defaultVals->push_back(defaultStr);
		}
		else {
			string errorMsg = "string array paramdef \"" + paramName + "\" must have non-null default value(s)";
			throw domain_error(errorMsg);
		}
	}

	// add a new StringArrayParamDef to the ParamSetDef
	StringArrayParamDef newStringArrayParamDef(paramName, helpText, promptText, requiredBoolVal,
		maxLen, defaultVals);
	stringArrayParamDefMap.insert( stringArrayParamDefValType(paramName, newStringArrayParamDef) );
}

/**
 * Private method to process all the parameter nodes in the XML file
 */
void ParamSetDef::processParamDefNodes(DOMNodeList *paramNodes) 
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
				handleIntParamDef(paramElem);
			}
			if(XMLString::equals(typeName, DOUBLE_PARAM_TYPE))
			{
				handleDoubleParamDef(paramElem);
			}
			if(XMLString::equals(typeName, STRING_PARAM_TYPE))
			{
				handleStringParamDef(paramElem);
			}
			if(XMLString::equals(typeName, BOOL_PARAM_TYPE))
			{
				handleBoolParamDef(paramElem);
			}
			if(XMLString::equals(typeName, INT_ARRAY_PARAM_TYPE))
			{
				handleIntArrayParamDef(paramElem);
			}
			if(XMLString::equals(typeName, DOUBLE_ARRAY_PARAM_TYPE))
			{
				handleDoubleArrayParamDef(paramElem);
			}
			if(XMLString::equals(typeName, STRING_ARRAY_PARAM_TYPE))
			{
				handleStringArrayParamDef(paramElem);
			}
		}
	}
}

/**
 * gets all the bool param defs for this psetdef
 */
auto_ptr< vector<BoolParamDef> > ParamSetDef::getBoolParamDefs()
{
	auto_ptr< vector<BoolParamDef> > retVal(new vector<BoolParamDef>);
	map<string, BoolParamDef>::iterator pos;
	for(pos = boolParamDefMap.begin(); pos != boolParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * gets all the int param defs for this psetdef
 */
auto_ptr< vector<IntParamDef> > ParamSetDef::getIntParamDefs()
{
	auto_ptr< vector<IntParamDef> > retVal(new vector<IntParamDef>);
	map<string, IntParamDef>::iterator pos;
	for(pos = intParamDefMap.begin(); pos != intParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * gets all the string param defs for this psetdef
 */
auto_ptr< vector<StringParamDef> > ParamSetDef::getStringParamDefs()
{
	auto_ptr< vector<StringParamDef> > retVal(new vector<StringParamDef>);
	map<string, StringParamDef>::iterator pos;
	for(pos = stringParamDefMap.begin(); pos != stringParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * gets all the double param defs for this psetdef
 */
auto_ptr< vector<DoubleParamDef> > ParamSetDef::getDoubleParamDefs()
{
	auto_ptr< vector<DoubleParamDef> > retVal(new vector<DoubleParamDef>);
	map<string, DoubleParamDef>::iterator pos;
	for(pos = doubleParamDefMap.begin(); pos != doubleParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * gets all the int array param defs for this psetdef
 */
auto_ptr< vector<IntArrayParamDef> > ParamSetDef::getIntArrayParamDefs()
{
	auto_ptr< vector<IntArrayParamDef> > retVal(new vector<IntArrayParamDef>);
	map<string, IntArrayParamDef>::iterator pos;
	for(pos = intArrayParamDefMap.begin(); pos != intArrayParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * gets all the double array param defs for this psetdef
 */
auto_ptr< vector<DoubleArrayParamDef> > ParamSetDef::getDoubleArrayParamDefs()
{
	auto_ptr< vector<DoubleArrayParamDef> > retVal(new vector<DoubleArrayParamDef>);
	map<string, DoubleArrayParamDef>::iterator pos;
	for(pos = doubleArrayParamDefMap.begin(); pos != doubleArrayParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * gets all the string array param defs for this psetdef
 */
auto_ptr< vector<StringArrayParamDef> > ParamSetDef::getStringArrayParamDefs()
{
	auto_ptr< vector<StringArrayParamDef> > retVal(new vector<StringArrayParamDef>);
	map<string, StringArrayParamDef>::iterator pos;
	for(pos = stringArrayParamDefMap.begin(); pos != stringArrayParamDefMap.end(); ++pos)
	{
		retVal->push_back(pos->second);	
	}
	return retVal;
}

/**
 * Used to tell the parser where to find the schema (xsd) file.
 * Normally, we will check 1. TASK_DIR environment variable, then 2. ACSROOT/config,
 * then 3. INTROOT/config; if these aren't defined, the parser will just use whatever is in the 
 * schemaLocation hint in the xml file (if present).
 */
void ParamSetDef::setSchemaLocation(DOMBuilder * parser)
{
	// set the schema location if the environment variable is defined
	char *xsdDir = getenv(TASK_DIR_ENV_VAR_NAME);
	if(NULL != xsdDir) {
		string schemaDirStr(xsdDir);
		schemaDirStr.append(SLASH_STRING).append(SCHEMA_SUBDIR_NAME);
		string testSchemaLocation(PSETDEF_NAMESPACE_URI);
		testSchemaLocation.append(SPACE_STRING).append(schemaDirStr).append(SLASH_STRING).append(PSETDEF_SCHEMA_FILE_NAME);
		StrX schemaLocationStrX(testSchemaLocation.c_str());
		parser->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, (void*)schemaLocationStrX.unicodeForm());
	}
	// else use ACSROOT/config if ACSROOT is defined
	xsdDir = getenv(ACSROOT_ENV_VAR_NAME);
	if(NULL != xsdDir) {
		string schemaDirStr(xsdDir);
		schemaDirStr.append(SLASH_STRING).append(CONFIG_SUBDIR_NAME);
		string testSchemaLocation(PSETDEF_NAMESPACE_URI);
		testSchemaLocation.append(SPACE_STRING).append(schemaDirStr).append(SLASH_STRING).append(PSETDEF_SCHEMA_FILE_NAME);
		StrX schemaLocationStrX(testSchemaLocation.c_str());
		parser->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, (void*)schemaLocationStrX.unicodeForm());
	}
	// else use INTROOT/config if INTROOT is defined
	xsdDir = getenv(INTROOT_ENV_VAR_NAME);
	if(NULL != xsdDir) {
		string schemaDirStr(xsdDir);
		schemaDirStr.append(SLASH_STRING).append(CONFIG_SUBDIR_NAME);
		string testSchemaLocation(PSETDEF_NAMESPACE_URI);
		testSchemaLocation.append(SPACE_STRING).append(schemaDirStr).append(SLASH_STRING).append(PSETDEF_SCHEMA_FILE_NAME);
		StrX schemaLocationStrX(testSchemaLocation.c_str());
		parser->setProperty(XMLUni::fgXercesSchemaExternalSchemaLocation, (void*)schemaLocationStrX.unicodeForm());
	}
	// else if none of the above environment variables was defined, 
	// we will rely on the XML instance document's schemalocation hint
}



