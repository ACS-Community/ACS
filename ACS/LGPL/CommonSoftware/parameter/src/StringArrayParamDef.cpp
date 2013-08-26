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
* "@(#) $Id: StringArrayParamDef.cpp,v 1.3 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <StringArrayParamDef.h>

using namespace std;
using namespace Parameters;

/**
 * Constructor.
 */
StringArrayParamDef::StringArrayParamDef()
{
}


/**
 * Constructor.
 */
StringArrayParamDef::StringArrayParamDef(const string & paramName, const string & helpText, const string & promptText, 
	bool requiredBoolVal, auto_ptr< int > maxLenVal, auto_ptr< vector < string > > defaultVals): 
		ParamDef(paramName, helpText, promptText, requiredBoolVal)
{
	if(NULL != maxLenVal.get()) {
		maxLen_m = *maxLenVal;
		hasMaxLen_m = true;
	}
	else {
		hasMaxLen_m = false;
	}
	if(NULL != defaultVals.get()) {
		defaultValues_m = *defaultVals;
		hasDefaultValues_m = true;
	}
	else {
		hasDefaultValues_m = false;
	}
}

/**
 * Destructor.
 */
StringArrayParamDef::~StringArrayParamDef()
{
}

/*
 * Accessor for the maxLen
 * @return the maxlen value as a ptr to int
 */
auto_ptr<int> StringArrayParamDef::getMaxLen()
{
	auto_ptr<int> retVal;
	if(true == getHasMaxLen()) {
		retVal.reset(new int(maxLen_m));
	}
	return retVal;
}

/*
 * Accessor for the default value.
 * @return the default value as a vector of string.
 */
auto_ptr< vector<string> > StringArrayParamDef::getDefaultValues()
{
	auto_ptr<vector <string> > retVal;
	if(true == getHasDefaultValues()) {
		retVal.reset(new vector<string>(defaultValues_m));
	}
	return retVal;
}

/*
 * Accessor for flag indicating if there is maxLen
 * @return the flag indicating if there is maxlen value 
 */
bool StringArrayParamDef::getHasMaxLen()
{
	return hasMaxLen_m;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value
 */
bool StringArrayParamDef::getHasDefaultValues()
{
	return hasDefaultValues_m;
}
