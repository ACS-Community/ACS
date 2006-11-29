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
* "@(#) $Id: StringParamDef.cpp,v 1.3 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <iostream>
#include <StringParamDef.h>

using namespace std;
using namespace Parameters;

/**
 * Constructor.
 */
StringParamDef::StringParamDef()
{
}

/**
 * Constructor.
 */
StringParamDef::StringParamDef(const string & nameVal, const string & helpVal, const string & promptVal, 
	bool isRequired, auto_ptr< string > defaultVal, auto_ptr< vector < string > > validVals): 
		ParamDef(nameVal, helpVal, promptVal, isRequired)
{
	if(NULL != defaultVal.get()) {
		defaultValue_m = *defaultVal;
		hasDefault_m = true;
	}
	else {
		hasDefault_m = false;
	}
	if(NULL != validVals.get()) {
		validValues_m = *validVals;
		hasValidValues_m = true;
	}
	else {
		hasValidValues_m = false;
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
		retVal.reset(new string(defaultValue_m));
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
		retVal.reset(new vector< string >(validValues_m));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value as an string.
 */
bool StringParamDef::getHasDefault()
{
	return hasDefault_m;
}
    
/*
 * Accessor for the flag indicating if there is a valid values.
 * @return the flag indicating if there is a valid values as a vector of strings.
 */
bool StringParamDef::getHasValidValues()
{
	return hasValidValues_m;
}
