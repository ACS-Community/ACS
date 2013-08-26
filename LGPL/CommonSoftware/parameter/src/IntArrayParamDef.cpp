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
* "@(#) $Id: IntArrayParamDef.cpp,v 1.3 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <IntArrayParamDef.h>

using namespace Parameters;

/**
 * Constructor.
 */
IntArrayParamDef::IntArrayParamDef()
{
}

/**
 * Constructor.
 */
IntArrayParamDef::IntArrayParamDef(const string & paramName, const string & helpText, const string & promptText, 
	bool requiredBoolVal, auto_ptr< string > unitsText, auto_ptr< int > maxLenVal, auto_ptr< vector < int > > defaultVals):
		ParamDef(paramName, helpText, promptText, requiredBoolVal)
{
	if(NULL != unitsText.get()) {
		units_m = *unitsText;
		hasUnits_m = true;
	}
	else {
		hasUnits_m = false;
	}
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
IntArrayParamDef::~IntArrayParamDef()
{
}

/*
 * Accessor for the units
 * @return the units as a ptr to a string
 */
auto_ptr<string> IntArrayParamDef::getUnits()
{
	auto_ptr<string> retVal;
	if(true == getHasUnits()) {
		retVal.reset(new string(units_m));
	}
	return retVal;
}

/*
 * Accessor for the maxLen
 * @return the maxlen value as a ptr to int
 */
auto_ptr<int> IntArrayParamDef::getMaxLen()
{
	auto_ptr<int> retVal;
	if(true == getHasMaxLen()) {
		retVal.reset(new int(maxLen_m));
	}
	return retVal;
}

/*
 * Accessor for the default value.
 * @return the default value as a vector of int.
 */
auto_ptr< vector<int> > IntArrayParamDef::getDefaultValues()
{
	auto_ptr<vector <int> > retVal;
	if(true == getHasDefaultValues()) {
		retVal.reset(new vector<int>(defaultValues_m));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there are units
 * @return the flag indicating if there are units 
 */
bool IntArrayParamDef::getHasUnits()
{
	return hasUnits_m;
}

/*
 * Accessor for flag indicating if there is maxLen
 * @return the flag indicating if there is maxlen value 
 */
bool IntArrayParamDef::getHasMaxLen()
{
	return hasMaxLen_m;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value
 */
bool IntArrayParamDef::getHasDefaultValues()
{
	return hasDefaultValues_m;
}


