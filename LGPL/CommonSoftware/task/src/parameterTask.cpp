/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2005 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: parameterTask.cpp,v 1.27 2010/04/27 12:24:57 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram    2005-01-19  created 
* sharring  2005-03-22  refactored to handle command-line parameters
*/

#include "vltPort.h"
#include "parameterTask.h"
#include <TaskServices.h>
#include <ParamStrX.h>
#include <ParamSetDef.h>
#include <xercesc/dom/DOMElement.hpp>
#include <xercesc/dom/DOMImplementation.hpp>
#include <xercesc/dom/DOMImplementationRegistry.hpp>
#include <xercesc/dom/DOMText.hpp>
#include <xercesc/dom/DOMWriter.hpp>
#include <xercesc/util/OutOfMemoryException.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/framework/XMLFormatter.hpp>
#include <xercesc/framework/MemBufFormatTarget.hpp>

static char *rcsId="@(#) $Id: parameterTask.cpp,v 1.27 2010/04/27 12:24:57 htischer Exp $"; 
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

XERCES_CPP_NAMESPACE_USE
using namespace ACS;
using namespace Parameters;
using std::domain_error;
using std::endl;
using std::string;
using std::invalid_argument;

typedef map< string, vector<string> >::value_type nameValType;

parameterTask::parameterTask(const ACE_CString &name, maci::ContainerServices* containerServices) :
    acscomponent::ACSComponentImpl(name, containerServices)
{
	ACS_TRACE("parameterTask::parameterTask");
}

/**
 * This is the ACS-provided 'run' method which takes the command-line parameters
 * and processes them by creating a map (as an intermediate step) and then, from 
 * the map, generating XML. The XML is then passed to the ParameterSet constructor 
 * to instantiate a ParameterSet object, which is an in-memory represention of 
 * the command-line parameters. The ParameterSet object is then passed to the 'go' 
 * method (which is to be implemented by the task author).
 * @throw taskErrType::TaskRunFailureEx
 */
void parameterTask::run(const ACS::StringSequence & parameters, const char* filePrefix)
{
	try
	{
		string fileName(filePrefix);
		buildParameterMap(parameters, fileName);

		string xmlContents = buildParameterSetXML(fileName);

		LOG(Logging::BaseLog::LM_INFO, 
		    "parameterTask::run", 
		    "The XML built from command line params is:\n" + xmlContents);

		string identifier("CommandLine");
		InMemoryXmlData xmlData(identifier, xmlContents);
		ParameterSet pset(&xmlData);

		TaskServices taskServices;
		go(pset, taskServices);
	}
	catch(invalid_argument &ex) 
	{
		ACS_LOG(LM_ERROR, "parameterTask::run", 
			(LM_ERROR, "**** Error ************** \n\n invalid_argument exception\n %s \n***************", ex.what()))
		taskErrType::TaskRunFailureExImpl exceptionToThrow = 
			taskErrType::TaskRunFailureExImpl(__FILE__, __LINE__, "parameterTask::run").getTaskRunFailureEx();
		throw exceptionToThrow;
	}
	catch(domain_error &ex) 
	{
		ACS_LOG(LM_ERROR, "parameterTask::run", 
			(LM_ERROR, "**** Error ************** \n\n domain_error exception\n %s \n***************", ex.what()))
		taskErrType::TaskRunFailureExImpl exceptionToThrow = 
			taskErrType::TaskRunFailureExImpl(__FILE__, __LINE__, "parameterTask::run").getTaskRunFailureEx();
		throw exceptionToThrow;
	}
	catch(...) 
	{
		throw taskErrType::TaskRunFailureExImpl(__FILE__, __LINE__, "parameterTask::run").getTaskRunFailureEx();
	}
}

/**
 * This method converts the entries in the map (created from the command-line arguments)
 * into XML which will then be fed as input into the parameter handling code (i.e. ParameterSet class).
 */
string parameterTask::buildParameterSetXML(const string & xmlFileNamePrefix) 
{
	string retVal;
	try {
		XMLPlatformUtils::Initialize();
	}
	catch (const XMLException& toCatch)
	{
		ACS_LOG(LM_ERROR, "parameterTask::buildParameterSetXML", 
			(LM_ERROR, "Error - XMLException - info: %s\n", StrX(toCatch.getMessage()).localForm()))
	}
	DOMImplementation* impl =  DOMImplementationRegistry::getDOMImplementation(StrX("XML 2.0").unicodeForm());

	if (impl != NULL)
	{
		try
		{
			// create a new DOMDocument which we will populate with 
			// entries from the command-line parameters, in order to 
			// make an xml version of the parameter set for use internally
			string qualifiedName(PARAMETERSET_NAMESPACE_PREFIX);
			qualifiedName.append(":").append(PARAMETERSET_STRING);

			DOMDocument* doc = impl->createDocument(
				StrX(PSET_NAMESPACE_URI).unicodeForm(), // root element namespace URI.
				StrX(qualifiedName.c_str()).unicodeForm(), // root element name
				0); // document type object (DTD).

			doc->setStandalone(true);

			// set our internal auto_ptr to point to the new document
			this->domDocument.reset(doc);

			string schemaHint(PSET_NAMESPACE_URI);
			schemaHint.append(" ").append(PARAMETERSET_SCHEMA_NAME);
			DOMElement* rootElem = doc->getDocumentElement();
			rootElem->setAttribute(StrX("xmlns:xsi").unicodeForm(), StrX("http://www.w3.org/2001/XMLSchema-instance").unicodeForm());
			rootElem->setAttribute(StrX("xsi:schemaLocation").unicodeForm(), 
				StrX(schemaHint.c_str()).unicodeForm());

			DOMElement*  psetdefElem = doc->createElement(StrX(PSETDEF_STRING).unicodeForm());
			rootElem->appendChild(psetdefElem);

			string xmlFileName = xmlFileNamePrefix + ".xml";
			DOMText* psetdefValTextNode = doc->createTextNode(StrX(xmlFileName.c_str()).unicodeForm());
			psetdefElem->appendChild(psetdefValTextNode);
			DOMElement*  nameElem = doc->createElement(StrX(NAME_STRING).unicodeForm());
			rootElem->appendChild(nameElem);
			DOMText* nameValTextNode = doc->createTextNode(StrX("command-line values").unicodeForm());
			nameElem->appendChild(nameValTextNode);

			map<string, vector<string> >::iterator position;
			// for each parameter in the parameterMap
			for(position = parameterMap.begin(); position != parameterMap.end(); ++position) {
				// determine the type by looking it up in our parameter set definition, i.e. psetdef 
				// (which we have obtained by parsing the task's psetdef xml file containing the task's metadata)
				// and add an element of the proper type to the XML document being constructed, with name equal to the
				// key portion of the current map entry, value equal to the value portion of the current map entry.
				ParamSetDef::paramTypesEnum paramType = paramSetDef->getParamTypeForName(position->first);
				switch(paramType) {
					case ParamSetDef::BOOL: {
						DOMElement *boolElem = createBoolElement(position->first, position->second, doc);
						rootElem->appendChild(boolElem);
						break;
					}
					case ParamSetDef::INT: {
						DOMElement *intElem = createIntElement(position->first, position->second, doc);
						rootElem->appendChild(intElem);
						break;
					}
					case ParamSetDef::INT_ARRAY: {
						DOMElement *intArrayElem = createIntArrayElement(position->first, position->second, doc);
						rootElem->appendChild(intArrayElem);
						break;
					}
					case ParamSetDef::DOUBLE: {
						DOMElement *doubleElem = createDoubleElement(position->first, position->second, doc);
						rootElem->appendChild(doubleElem);
						break;
					}
					case ParamSetDef::DOUBLE_ARRAY: {
						DOMElement * doubleArrayElem = 
							createDoubleArrayElement(position->first, position->second, doc);
						rootElem->appendChild(doubleArrayElem);
						break;
					}
					case ParamSetDef::STRING: {
						DOMElement *stringElem = createStringElement(position->first, position->second, doc);
						rootElem->appendChild(stringElem);
						break;
					}
					case ParamSetDef::STRING_ARRAY: {
						DOMElement * stringArrayElem = createStringArrayElement(position->first, position->second, doc);
						rootElem->appendChild(stringArrayElem);
						break;
					}
				}
			}

			// construct the DOM writer
			DOMWriter *domWriter = impl->createDOMWriter();
			if (domWriter->canSetFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true)) {
				domWriter->setFeature(XMLUni::fgDOMWRTFormatPrettyPrint, true);
			}

			// construct the MemBufFormatTarget
			XMLFormatTarget *myFormatTarget = new MemBufFormatTarget();

			// set the encoding to be ISO-8859-1
			XMLCh tempStr[100];
			XMLString::transcode("ISO-8859-1", tempStr, 99);
			domWriter->setEncoding(tempStr);

			// serialize the document to an internal memory buffer
			domWriter->writeNode(myFormatTarget, *doc);

			// get the string which is encoded in ISO-8859-1 from the MemBufFormatTarget
			char* theXMLString_Encoded = (char*) 
  					((MemBufFormatTarget*)myFormatTarget)->getRawBuffer();

			retVal = string(StrX(theXMLString_Encoded).localForm());

			// release the memory
			delete myFormatTarget;
			delete domWriter;

			//doc->release();
		}
		catch (const OutOfMemoryException& e)
		{
			ACS_LOG(LM_ERROR, "parameterTask::buildParameterSetXML", 
				(LM_ERROR, "Error - OutOfMemoryException - info: %s\n", StrX(e.getMessage()).localForm()))
		}
		catch (const DOMException& e)
		{
			ACS_LOG(LM_ERROR, "parameterTask::buildParameterSetXML", 
				(LM_ERROR, "Error - DOMException - info: %s\n", StrX(e.getMessage()).localForm()))
		}
	}
	else
	{
		ACS_LOG(LM_ERROR, "parameterTask::buildParameterSetXML", 
			(LM_ERROR, "Error - requested DOM implementation not supported!"))
	}
	return retVal;
}

/**
 * Private method to do the common work for creating a DOMElement for an array parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values with which to populate the DOMElement's
 *        array values.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 * @param paramType a string which represents the parameter's type, e.g. intArray, doubleArray, etc.
 */
DOMElement* parameterTask::createArrayElement(const string & paramName, const vector<string> & values, 
	DOMDocument *doc, const string & paramType) 
{
	DOMElement* paramElem = doc->createElement(StrX(PARAMETER_STRING).unicodeForm());
	paramElem->setAttribute(StrX("xsi:type").unicodeForm(), StrX(paramType.c_str()).unicodeForm());

	DOMElement*  nameElem = doc->createElement(StrX(NAME_STRING).unicodeForm());
	paramElem->appendChild(nameElem);

	DOMText* nameValTextNode = doc->createTextNode(StrX(paramName.c_str()).unicodeForm());
	nameElem->appendChild(nameValTextNode);

	vector<string>::const_iterator position;
	for(position = values.begin(); position < values.end(); ++position) {
		DOMElement*  valueElem = doc->createElement(StrX(VALUE_STRING).unicodeForm());
		paramElem->appendChild(valueElem);

		DOMText* valueValTextNode = doc->createTextNode(StrX((*position).c_str()).unicodeForm());
		valueElem->appendChild(valueValTextNode);
	}

	return paramElem;
}

/**
 * Private method to do the common work for creating a DOMElement for a non-array parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values (actually, for simple non-array
 *        elements, only the first element in the vector is of interest/used) with which to 
 *        populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 * @param paramType a string which represents the parameter's type, e.g. intArray, doubleArray, etc.
 *
 */
DOMElement* parameterTask::createSimpleElement(const string & paramName, const vector<string> & values, 
	DOMDocument *doc, const string & paramType)
{
	DOMElement* paramElem = doc->createElement(StrX(PARAMETER_STRING).unicodeForm());
	paramElem->setAttribute(StrX("xsi:type").unicodeForm(), StrX(paramType.c_str()).unicodeForm());

	DOMElement*  nameElem = doc->createElement(StrX(NAME_STRING).unicodeForm());
	paramElem->appendChild(nameElem);

	DOMText* nameValTextNode = doc->createTextNode(StrX(paramName.c_str()).unicodeForm());
	nameElem->appendChild(nameValTextNode);

	if(!values.empty()) {
		DOMElement*  valueElem = doc->createElement(StrX(VALUE_STRING).unicodeForm());
		paramElem->appendChild(valueElem);

		// the first element in the values vector is the value to use; this is a simple
		// element and more than one value would not make sense. could throw an exception 
		// if there are more than 1 element in the vector, but it shouldn't be necessary 
		// as by the time this method is called validation has already occurred.
		DOMText* valueValTextNode = doc->createTextNode(StrX(values.at(0).c_str()).unicodeForm());
		valueElem->appendChild(valueValTextNode);
	}

	return paramElem;
}

/**
 * Private method to create a DOMElement for a bool parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values (actually, for simple non-array
 *        elements such as bool, only the first element in the vector is of interest/used) 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createBoolElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(BOOL_PARAM_STRING);
	DOMElement * newElem = createSimpleElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method to create a DOMElement for an int parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values (actually, for simple non-array
 *        elements such as int, only the first element in the vector is of interest/used) 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createIntElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(INT_PARAM_STRING);
	DOMElement*  newElem = createSimpleElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method to create a DOMElement for a double parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values (actually, for simple non-array
 *        elements such as double, only the first element in the vector is of interest/used) 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createDoubleElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(DOUBLE_PARAM_STRING);
	DOMElement*  newElem = createSimpleElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method to create a DOMElement for a string parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values (actually, for simple non-array
 *        elements such as string, only the first element in the vector is of interest/used) 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createStringElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(STRING_PARAM_STRING);
	DOMElement*  newElem = createSimpleElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method to create a DOMElement for an intArray parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createIntArrayElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(INT_ARRAY_PARAM_STRING);
	DOMElement*  newElem = createArrayElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method to create a DOMElement for an intArray parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createDoubleArrayElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(DOUBLE_ARRAY_PARAM_STRING);
	DOMElement*  newElem = createArrayElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method to create a DOMElement for an stringArray parameter
 * @param paramName the name of the parameter for which to create the DOMElement
 * @param values a vector of strings containing the values 
 *        with which to populate the DOMElement's value.
 * @param doc the DOMDocument to which the new DOMElement will belong.
 */
DOMElement* parameterTask::createStringArrayElement(const string & name, const vector<string> & values, DOMDocument* doc) 
{
	string paramString(PARAMETERSET_NAMESPACE_PREFIX);
	paramString.append(":").append(STRING_ARRAY_PARAM_STRING);
	DOMElement*  newElem = createArrayElement(name, values, doc, paramString);
	return newElem;
}

/**
 * Private method in checking to see if a command-line name=value pair is well formed;
 * specifically, this utility method simply checks to see if the currentPosition in a
 * string is at the end of the string. If the current position is at the end of the
 * string, this will throw an exception with the error message designated by the user 
 * and passed in the msg parameter.
 * 
 * @param currPosition
 * @param length
 * @param msg The error message to use in the exception, in the event that the current position 
 *        is out of bounds and an exception is thrown.
 */
void parameterTask::checkPosition(string::size_type currPosition, unsigned int length, const string & msg) 
{
	if(currPosition >= length) {
		throw invalid_argument(msg);
	}
}

/**
 * This method takes all the command line parameters and builds a map which maps the 
 * parameter name (string) to a vector of parameter values (vector of strings). For simple
 * (non-array) parameters, the vector will contain a single string while for array parameters,
 * the vector may contain more than one entry. The map will be used later to generate XML which is
 * then fed into the parameter handling logic (which requires XML as its input) - this conversion
 * is perfomed in buildParameterSetXML() method.
 */
void parameterTask::buildParameterMap(const ACS::StringSequence & params, const string & baseFileName) 
{
	ACS_TRACE("parameterTask::buildParameterMap enter");
	ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "sequence length is: %d \n", params.length())

	// instantiate the ParamSetDef from the XML file containing the task's meta-data
	string xmlFileName = baseFileName + ".xml";
	ParamSetDef * psetDef = new ParamSetDef(xmlFileName);
	this->paramSetDef.reset(psetDef);

	const string nameValuePairDelim("=");
	string::size_type nameStartPosition = 0;
	vector<string> parsedValues;
	for(CORBA::ULong i = 0; i < params.length(); i++) {
		ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "entering loop, value of i is: %d \n", i)
		if(NULL != params[i]) {
			string currStr(params[i]);
			ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "name/value pair: %s \n", currStr.c_str())

			// search for the first equals sign in the string, this will 
			// distinguish the name portion from the value portion (of the name/value pair)
			string::size_type nameEndPosition = currStr.find_first_of(nameValuePairDelim);
			checkPosition(nameEndPosition, currStr.length(), 
				"Error - malformed parameter - missing value portion of name=value pair");
			ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "The equals sign position is: %d", nameEndPosition)
			int nameLength = (nameEndPosition - nameStartPosition);
			string name = currStr.substr(nameStartPosition, nameLength);
			ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "The parameter name was parsed as: %s", name.c_str())

			string valueStr = currStr.substr(nameEndPosition+1);
			ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "The value string is: %s", valueStr.c_str())
			ParamSetDef::paramTypesEnum paramType;
			try {
				paramType = paramSetDef->getParamTypeForName(name);
			}
			catch(domain_error domainEx) {
				string msg = "parameter \"" + name + "\" does not exist in parameter set definition";
				throw invalid_argument(msg);
			}
			switch(paramType) {
				case ParamSetDef::BOOL: {
					parsedValues = parseBoolElement(valueStr);
					break;
				}
				case ParamSetDef::INT: {
					parsedValues = parseIntElement(valueStr);
					break;
				}
				case ParamSetDef::INT_ARRAY: {
					parsedValues = parseIntArrayElement(valueStr);
					break;
				}
				case ParamSetDef::DOUBLE: {
					parsedValues = parseDoubleElement(valueStr);
					break;
				}
				case ParamSetDef::DOUBLE_ARRAY: {
					parsedValues = parseDoubleArrayElement(valueStr);
					break;
				}
				case ParamSetDef::STRING: {
					parsedValues = parseStringElement(valueStr);
					break;
				}
				case ParamSetDef::STRING_ARRAY: {
					parsedValues = parseStringArrayElement(valueStr);
					break;
				}
			}
			parameterMap.insert(nameValType(name, parsedValues));
			ACS_DEBUG_PARAM("parameterTask::buildParameterMap", "exiting loop, value of i is: %d \n", i)
		}
	}

	ACS_TRACE("parameterTask::buildParameterMap exit");
}

/**
 * NOTE: the parsing at this level is really simple because all we need to do here
 * is to ensure that the string is in the proper format such that xerces can deal with it after
 * we convert to XML and feed it into the xerces parser. Consequently, all we do here is remove/replace
 * the scientific notation using "D" or "d" which xerces does not recognize as valid scientific notation,
 * and use, instead, "e" which xerces recognizes.
 */
vector<string> parameterTask::parseDoubleElement(const string & valueString) 
{
	vector<string> retVal;
	string localValueString(valueString);
	// replace any 'd' or 'D' in the string with 'e' to allow xerces to properly
	// interpret the d/D as scientific notation
	string::size_type dPosition = localValueString.find_first_of("eE");
	if(string::npos != dPosition) 
	{ 
		ACS_DEBUG_PARAM("parameterTask::parseDoubleElement", "The string before correction of sci-notation is: %s", localValueString.c_str())
		localValueString.replace(dPosition, 1, "e");
		ACS_DEBUG_PARAM("parameterTask::parseDoubleElement", "The string after correction of sci-notation is: %s", localValueString.c_str())
	}
	retVal.push_back(localValueString);
	return retVal;
}

/**
 * Parsing is trivial, as we only need to make sure it gets added properly to the 
 * XML that will be generated in buildParameterSetXML() method; we simply use the 
 * entire string (value portion of name=value pair).
 */
vector<string> parameterTask::parseStringElement(const string & valueString) 
{
	vector<string> retVal;
	retVal.push_back(valueString);
	return retVal;
}

/**
 * Parsing is trivial, as we only need to make sure it gets added properly to the 
 * XML that will be generated in buildParameterSetXML() method; we simply use the 
 * entire string (value portion of name=value pair) - if the string is not a valid boolean
 * value, it will be caught as a problem by xerces and by the parameter handling logic at a 
 * later point.
 */
vector<string> parameterTask::parseBoolElement(const string & valueString) 
{
	vector<string> retVal;
	retVal.push_back(valueString);
	return retVal;
}

/**
 * Parsing is trivial, as we only need to make sure it gets added properly to the 
 * XML that will be generated in buildParameterSetXML() method; we simply use the 
 * entire string (value portion of name=value pair) - if the string is not a valid int
 * value, it will be caught as a problem by xerces and by the parameter handling logic at a 
 * later point.
 */
vector<string> parameterTask::parseIntElement(const string & valueString) 
{
	vector<string> retVal;
	retVal.push_back(valueString);
	return retVal;
}

/**
 * Parsing is trivial, as we only need to make sure the ints get added properly to the 
 * XML that will be generated in buildParameterSetXML() method; we simply use the 
 * value(s) verbatim from the incoming string that is being parsed (values are
 * the text between commas of name=value,value,value string). If the string(s) found
 * are not valid int values, they will be caught as a problem by xerces and by the parameter 
 * handling logic at a later point (as it parses the XML that is generated in buildParameterSetXML).
 */
vector<string> parameterTask::parseIntArrayElement(const string & valueString) 
{
	ACS_DEBUG_PARAM("parameterTask::parseIntArrayElement", "The value string incoming: %s", valueString.c_str())
	vector<string> retVal;
	const string delimiter(",");
	string::size_type valueStartPosition = 0;
	string::size_type valueEndPosition = 0;
	
	while(valueString.length() > valueStartPosition) {
		// using commas as delimiters, create a separate string
		// for each int in the array and add it to the vector 
		valueEndPosition = valueString.find_first_of(delimiter, valueStartPosition);
		if(string::npos == valueEndPosition)
		{
			valueEndPosition = valueString.length();
		}
		int valueLength = (valueEndPosition - valueStartPosition);
		string value = valueString.substr(valueStartPosition, valueLength);
		ACS_DEBUG_PARAM("parameterTask::parseIntArrayElement", "value pushed into vector is: %s", value.c_str())
		retVal.push_back(value);
		valueStartPosition = valueEndPosition+1;
	}

	return retVal;
}

/**
 * NOTE: the parsing at this level is really simple because all we need to do here
 * is to ensure that the string(s) are in the proper format such that xerces can deal with them after
 * we convert to XML and feed it into the xerces parser. Consequently, all we do here is remove/replace
 * the scientific notation using "D" or "d" which xerces does not recognize as valid scientific notation,
 * and use, instead, "e" which xerces recognizes.
 */
vector<string> parameterTask::parseDoubleArrayElement(const string & valueString) 
{
	ACS_DEBUG_PARAM("parameterTask::parseDoubleArrayElement", "The value string incoming: %s", valueString.c_str())
	vector<string> retVal;
	const string delimiter(",");
	string::size_type valueStartPosition = 0;
	string::size_type valueEndPosition = 0;
	
	while(valueString.length() > valueStartPosition) {
		// using commas as delimiters, create a separate string
		// for each double in the array and add it to the vector 
		valueEndPosition = valueString.find_first_of(delimiter, valueStartPosition);
		if(string::npos == valueEndPosition)
		{
			valueEndPosition = valueString.length();
		}
		int valueLength = (valueEndPosition - valueStartPosition);
		string value = valueString.substr(valueStartPosition, valueLength);

		// replace any 'd' or 'D' in the string with 'e' to allow xerces to properly
		// interpret the d/D as scientific notation
		string::size_type dPosition = value.find_first_of("dD");
		if(string::npos != dPosition) 
		{ 
			ACS_DEBUG_PARAM("parameterTask::parseDoubleArrayElement", "The string before correction of sci-notation is: %s", value.c_str())
			value.replace(dPosition, 1, "e");
			ACS_DEBUG_PARAM("parameterTask::parseDoubleArrayElement", "The string after correction of sci-notation is: %s", value.c_str())
		}
		ACS_DEBUG_PARAM("parameterTask::parseDoubleArrayElement", "value pushed into vector is: %s", value.c_str())
		retVal.push_back(value);
		valueStartPosition = valueEndPosition+1;
	}
	return retVal;
}

/**
 * Parsing is trivial, as we only need to make sure the strings get added properly to the 
 * XML that will be generated in buildParameterSetXML() method; we simply use the 
 * value(s) from the incoming string verbatim. That is, each item/value of the string array 
 * being parsed is in the incoming string and is delimited by commas as in name=value,value,value). 
 */
vector<string> parameterTask::parseStringArrayElement(const string & valueString) 
{
	ACS_DEBUG_PARAM("parameterTask::parseStringArrayElement", "The value string incoming: %s", valueString.c_str())
	vector<string> retVal;
	const string delimiter(",");
	const string quotes("\'\"");
	string::size_type valueStartPosition = 0;
	string::size_type valueEndPosition = 0;
	
	while(valueString.length() > valueStartPosition) {
		if(valueString.at(valueStartPosition) == '\"' || valueString.at(valueStartPosition) == '\'')
		{
			// quoted strings can be used to embed commas inside a string in which 
			// case the quotes delimit the value and we ignore commas inside the quotes

			// NOTE: embedded quotes within strings may not work, but JMcMullin says
			// this shouldn't occur, so it is not considered in this algorithm.

			// first, skip the quote as it shouldn't be included in the string
			valueStartPosition++;
			valueEndPosition = valueString.find_first_of(quotes, valueStartPosition);
		}
		else 
		{
			// otherwise, commas delimit the values
			valueEndPosition = valueString.find_first_of(delimiter, valueStartPosition);
		}
		if(string::npos == valueEndPosition)
		{
			valueEndPosition = valueString.length();
		}
		int valueLength = (valueEndPosition - valueStartPosition);
		string value = valueString.substr(valueStartPosition, valueLength);
		if(value.length() > 0)
		{
			ACS_DEBUG_PARAM("parameterTask::parseStringArrayElement", "value pushed into vector is: %s", value.c_str())
			retVal.push_back(value);
		}
		valueStartPosition = valueEndPosition+1;
	}
	return retVal;
}
/*___oOo___*/


