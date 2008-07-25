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
* "@(#) $Id: ParameterSet.cpp,v 1.2 2008/07/25 07:52:19 cparedes Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <iostream>
#include <iterator>
#include <sstream>
#include <vector>

#include <ParameterSet.h>
#include <MyDOMErrorHandler.h>
#include <StrX.h>

#include <xercesc/dom/DOM.hpp>
#include <xercesc/parsers/XercesDOMParser.hpp>
#include <xercesc/util/PlatformUtils.hpp>
#include <xercesc/util/XMLDouble.hpp>

#ifndef MEMPARSE_ENCODING
   #if defined(OS390)
      #define MEMPARSE_ENCODING "ibm-1047-s390"
   #elif defined(OS400)
      #define MEMPARSE_ENCODING "ibm037"
   #else
      #define MEMPARSE_ENCODING "ascii"
   #endif
#endif 

/**
 * Constructor.
 * 
 * @param xmlFile the character string containing XML representation of the parameter set data.
 */
ParameterSet::ParameterSet(const char* xmlFile)
{
	// Initialize the XML4C2 system
	try
	{
		XMLPlatformUtils::Initialize();
		std::cout << "initialized XML platform\n";

		PARAMETER_TAG_NAME = XMLString::transcode("parameter");
		NAME_TAG_NAME = XMLString::transcode("name");
		VALUE_TAG_NAME = XMLString::transcode("value");
		UNITS_TAG_NAME = XMLString::transcode("units");

		INT_PARAM_TYPE = XMLString::transcode("IntParamType");
		DOUBLE_PARAM_TYPE = XMLString::transcode("DoubleParamType");
		STRING_PARAM_TYPE = XMLString::transcode("StringParamType");
		BOOL_PARAM_TYPE = XMLString::transcode("BoolParamType");
		INT_ARRAY_PARAM_TYPE = XMLString::transcode("IntArrayParamType");
		DOUBLE_ARRAY_PARAM_TYPE = XMLString::transcode("DoubleArrayParamType");
		STRING_ARRAY_PARAM_TYPE = XMLString::transcode("StringArrayParamType");

		parseInputString(xmlFile);
	}
	catch (const XMLException& toCatch)
	{
		std::cerr << "Error during initialization! Message:\n"
		<< StrX(toCatch.getMessage()) << std::endl;
	}
	catch (const std::invalid_argument argError)
	{
		std::cerr <<          "****** Error validating against the parameter set definition! ******* \n\nMessage: \n\n"
		<< argError.what() << "*********************************************************************" 
		<< std::endl << std::endl;
	}
}

/**
 * Destructor.
 */
ParameterSet::~ParameterSet()
{
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
}

/**
 * Private method to orchestrate the XML parsing.
 */
int ParameterSet::parseInputString(const char* xmlFile)
{
    return parseDOM(xmlFile);
}

/**
 * Private method to orchestrate the XML parsing using DOM.
 */
int ParameterSet::parseDOM(const char* xmlFile)
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

	// enable datatype normalization - default is off
	parser->setFeature(XMLUni::fgDOMDatatypeNormalization, true);

	/*******************
	// optionally you can implement your DOMErrorHandler (e.g. MyDOMErrorHandler)
	// and set it to the builder
	*******************/
	MyDOMErrorHandler* errHandler = new MyDOMErrorHandler();
	parser->setErrorHandler(errHandler);

	DOMDocument *doc = 0;
	
	try {
		doc = parser->parseURI(xmlFile);
		std::cout << "parsed file: " << xmlFile <<"\n";

		// Get all parameters
		DOMNodeList * paramNodes = doc->getElementsByTagName(PARAMETER_TAG_NAME);
		if(NULL != paramNodes) {
			processParamNodes(paramNodes);
		}
	}
	catch (const XMLException& toCatch) {
		char* message = XMLString::transcode(toCatch.getMessage());
		std::cout << "Exception message is: \n" << message << "\n";
		XMLString::release(&message);
		return -1;
	}
	catch (const DOMException& toCatch) {
		char* message = XMLString::transcode(toCatch.msg);
		std::cout << "Exception message is: \n" << message << "\n";
		XMLString::release(&message);
		return -1;
	}
	catch (...) {
		std::cout << "Unexpected Exception in ParameterSet\n" ;
		return -1;
	}

	parser->release();
	delete errHandler;
	return 0;
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
			XMLCh* typeAttrString = XMLString::transcode("xsi:type");
			const XMLCh* typeName = paramElem->getAttribute(typeAttrString);

			// check the type of parameter and instantiate the proper class 
			if(XMLString::equals(typeName, INT_PARAM_TYPE))
			{
				std::cout << "int param encountered" << std::endl;
			}
			if(XMLString::equals(typeName, DOUBLE_PARAM_TYPE))
			{
				std::cout << "double param encountered" << std::endl;
			}
			if(XMLString::equals(typeName, STRING_PARAM_TYPE))
			{
				std::cout << "string param encountered" << std::endl;
			}
			if(XMLString::equals(typeName, BOOL_PARAM_TYPE))
			{
				std::cout << "bool param encountered" << std::endl;
			}
			if(XMLString::equals(typeName, INT_ARRAY_PARAM_TYPE))
			{
				std::cout << "int array param encountered" << std::endl;
			}
			if(XMLString::equals(typeName, DOUBLE_ARRAY_PARAM_TYPE))
			{
				std::cout << "double array param encountered" << std::endl;
			}
			if(XMLString::equals(typeName, STRING_ARRAY_PARAM_TYPE))
			{
				std::cout << "string array param encountered" << std::endl;
			}
		}
	}
}
