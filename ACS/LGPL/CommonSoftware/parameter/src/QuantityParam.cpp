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
* "@(#) $Id: QuantityParam.cpp,v 1.3 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  08/14/05  created 
*/

#include <QuantityParam.h>
#include <parameterConstants.h>
#include <string>

using std::string;
using namespace Parameters;

/**
 * Constructor.
 */
QuantityParam::QuantityParam()
{
}

/**
 * Constructor.
 */
QuantityParam::QuantityParam(const string & nameVal, auto_ptr<string> unitsVal): Param(nameVal)
{
	if(NULL != unitsVal.get()) {
		units_m = *unitsVal;
		hasUnits_m = true;
	}
	else {
		hasUnits_m = false;
	}
}

/**
 * Destructor.
 */
QuantityParam::~QuantityParam()
{
}

/*
 * Accessor for the flag indicating if there are units.
 * @return the flag indicating if there are units
 */
bool QuantityParam::getHasUnits()
{
   return hasUnits_m;
}

/*
 * Accessor for the units
 * @return the units value as a int ptr.
 */
auto_ptr< string > QuantityParam::getUnits()
{
	auto_ptr<string> retVal;
	if(true == getHasUnits()) {
		retVal.reset(new string(units_m));
	}
	return retVal;
}

/**
 * Returns an XML string representation of the param.
 * @return the param as an XML string
 * NOTE: the XML is not a complete document, just a fragment.
 */
string QuantityParam::toString() 
{
	string retVal;

	// opening stanza
	//e.g. <param xsi:type="pset:bool">
	retVal.append(TAB);
	retVal.append(LESS_THAN_SIGN);
	retVal.append(PARAMETER_STRING);
	retVal.append(SPACE);
	retVal.append(XML_SCHEMA_INSTANCE_PREFIX);
	retVal.append(COLON);
	retVal.append(TYPE_STRING);
	retVal.append(EQUALS);
	retVal.append(QUOTE);
	retVal.append(PARAMETERSET_NAMESPACE_PREFIX);
	retVal.append(COLON);
	retVal.append(getType());
	retVal.append(QUOTE);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(NEWLINE);

	// name stanza
	//e.g. <name>overwrite</name>
	retVal.append(TAB);
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

	// units stanza, if applicable
	// e.g. <units>MHz</units>
	if(true == getHasUnits())
	{
		retVal.append(TAB);
		retVal.append(TAB);
		retVal.append(LESS_THAN_SIGN);
		retVal.append(UNITS_STRING );
		retVal.append(GREATER_THAN_SIGN);
		retVal.append(*(getUnits()));
		retVal.append(LESS_THAN_SIGN);
		retVal.append(SLASH_STRING);
		retVal.append(UNITS_STRING);
		retVal.append(GREATER_THAN_SIGN);
		retVal.append(NEWLINE);
	}

	// value(s) stanza
	// e.g. <value>10.01</value>
	retVal.append(valueToString());

	// closing param stanza
	// e.g. </param>
	retVal.append(TAB);
	retVal.append(LESS_THAN_SIGN);
	retVal.append(SLASH_STRING);
	retVal.append(PARAMETER_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(NEWLINE);

	return retVal;
}


