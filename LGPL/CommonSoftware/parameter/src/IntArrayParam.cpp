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
* "@(#) $Id: IntArrayParam.cpp,v 1.3 2005/08/15 23:26:53 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <IntArrayParam.h>
#include <parameterConstants.h>
#include <sstream>

using namespace parameterSet;
using std::stringstream;

/**
 * Constructor.
 */
IntArrayParam::IntArrayParam()
{
}

/**
 * Constructor.
 */
IntArrayParam::IntArrayParam(vector<int> vals, string nameVal, auto_ptr<string> unitsVal)
{
	values = vals;
	name = nameVal;
	if(NULL != unitsVal.get()) {
		units = *unitsVal;
		hasUnits = true;
	}
	else {
		hasUnits = false;
	}
}

/**
 * Destructor.
 */
IntArrayParam::~IntArrayParam()
{
}

/*
 * Accessor for the values.
 * @return the value as a vector of int.
 */
vector<int> IntArrayParam::getValues()
{
   return values;
}

/**
 * Accessor for the type, e.g. "bool" or "int" etc.
 * @return the type as a string.
 */
string IntArrayParam::getType()
{
	return INT_ARRAY_PARAM_STRING;
}

/**
 * Used to create the value portion of the toString (XML) string.
 */
string IntArrayParam::valueToString()
{
	string retVal;

	// iterate over the values, creating
	// a value XML entry for each one.
	vector<int>::iterator iter;

	for(iter = values.begin(); iter != values.end(); iter++)
	{
		// generate the value stanza
		//	e.g. <value>10</true>
		retVal.append(TAB);
		retVal.append(TAB);
		retVal.append(LESS_THAN_SIGN);
		retVal.append(VALUE_STRING);
		retVal.append(GREATER_THAN_SIGN);

		stringstream strStream;
		strStream << *(iter);
		retVal.append(strStream.str());

		retVal.append(LESS_THAN_SIGN);
		retVal.append(SLASH_STRING);
		retVal.append(VALUE_STRING);
		retVal.append(GREATER_THAN_SIGN);
		retVal.append(NEWLINE);
	}

	return retVal;
}
