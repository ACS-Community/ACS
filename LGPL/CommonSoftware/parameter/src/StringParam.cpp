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
* "@(#) $Id: StringParam.cpp,v 1.4 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <StringParam.h>
#include <parameterConstants.h>

using namespace std;
using namespace Parameters;

/**
 * Constructor.
 */
StringParam::StringParam()
{
}

/**
 * Constructor.
 */
StringParam::StringParam(const string & nameVal, const string & stringVal): Param(nameVal), value_m(stringVal)
{
}

/**
 * Destructor.
 */
StringParam::~StringParam()
{
}

/*
 * Accessor for the value.
 * @return the value as a string.
 */
string StringParam::getValue()
{
   return value_m;
}

/**
 * Accessor for the type, e.g. "bool" or "int" etc.
 * @return the type as a string.
 */
string StringParam::getType()
{
	string retVal(STRING_PARAM_STRING);
	return retVal;
}

/**
 * Used to create the value portion of the toString (XML) string.
 */
string StringParam::valueToString()
{
	string retVal;

	// value stanza
	//	e.g. <value>true</true>
	retVal.append(TAB);
	retVal.append(TAB);
	retVal.append(LESS_THAN_SIGN);
	retVal.append(VALUE_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(getValue());
	retVal.append(LESS_THAN_SIGN);
	retVal.append(SLASH_STRING);
	retVal.append(VALUE_STRING);
	retVal.append(GREATER_THAN_SIGN);
	retVal.append(NEWLINE);

	return retVal;
}


