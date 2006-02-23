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
* "@(#) $Id: StringParamDef.cpp,v 1.2 2005/01/24 23:03:09 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <iostream>
#include <StringParamDef.h>

using namespace std;
using namespace parameterSet;

/**
 * Constructor.
 */
StringParamDef::StringParamDef()
{
}

/**
 * Constructor.
 */
StringParamDef::StringParamDef(string nameVal, string helpVal, string promptVal, 
	bool isRequired, auto_ptr< string > defaultVal, auto_ptr< vector < string > > validVals)
{
	name = nameVal;	
	help = helpVal;
	prompt = promptVal;
	required = isRequired;

	if(NULL != defaultVal.get()) {
		defaultValue = *defaultVal;
		hasDefault = true;
	}
	else {
		hasDefault = false;
	}
	if(NULL != validVals.get()) {
		validValues = *validVals;
		hasValidValues = true;
	}
	else {
		hasValidValues = false;
	}
}

/**
 * Destructor.
 */
StringParamDef::~StringParamDef()
{
}

/*
 * Accessor for the default value.
 * @return the default value as a string.
 */
auto_ptr<string> StringParamDef::getDefault()
{
	auto_ptr<string> retVal;
	if(true == getHasDefault()) {
		retVal.reset(new string(defaultValue));
	}
	return retVal;
}

/*
 * Accessor for the valid values.
 * @return the valid values as a ptr to a vector of strings.
 */
auto_ptr< vector< string > > StringParamDef::getValidValues() 
{
	auto_ptr< vector <string> > retVal;
	if(true == getHasValidValues()) {
		retVal.reset(new vector< string >(validValues));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value as an string.
 */
bool StringParamDef::getHasDefault()
{
	return hasDefault;
}
    
/*
 * Accessor for the flag indicating if there is a valid values.
 * @return the flag indicating if there is a valid values as a vector of strings.
 */
bool StringParamDef::getHasValidValues()
{
	return hasValidValues;
}
