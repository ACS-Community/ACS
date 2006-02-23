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
* "@(#) $Id: DoubleArrayParamDef.cpp,v 1.2 2005/01/24 23:03:09 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <DoubleArrayParamDef.h>

using namespace parameterSet;

/**
 * Constructor.
 */
DoubleArrayParamDef::DoubleArrayParamDef()
{
}


/**
 * Constructor.
 */
DoubleArrayParamDef::DoubleArrayParamDef(string paramName, string helpText, string promptText, 
	bool requiredBoolVal, auto_ptr< string > unitsText, auto_ptr< int > maxLenVal, auto_ptr< vector < double > > defaultVals)
{
	name = paramName;	
	help = helpText;
	prompt = promptText;
	required = requiredBoolVal;

	if(NULL != unitsText.get()) {
		units = *unitsText;
		hasUnits = true;
	}
	else {
		hasUnits = false;
	}
	if(NULL != maxLenVal.get()) {
		maxLen = *maxLenVal;
		hasMaxLen = true;
	}
	else {
		hasMaxLen = false;
	}
	if(NULL != defaultVals.get()) {
		defaultValues = *defaultVals;
		hasDefaultValues = true;
	}
	else {
		hasDefaultValues = false;
	}
}

/**
 * Destructor.
 */
DoubleArrayParamDef::~DoubleArrayParamDef()
{
}

/*
 * Accessor for the units
 * @return the units as a ptr to a string
 */
auto_ptr<string> DoubleArrayParamDef::getUnits()
{
	auto_ptr<string> retVal;
	if(true == getHasUnits()) {
		retVal.reset(new string(units));
	}
	return retVal;
}

/*
 * Accessor for the maxLen
 * @return the maxlen value as a ptr to int
 */
auto_ptr<int> DoubleArrayParamDef::getMaxLen()
{
	auto_ptr<int> retVal;
	if(true == getHasMaxLen()) {
		retVal.reset(new int(maxLen));
	}
	return retVal;
}

/*
 * Accessor for the default value.
 * @return the default value as a vector of double.
 */
auto_ptr< vector<double> > DoubleArrayParamDef::getDefaultValues()
{
	auto_ptr<vector <double> > retVal;
	if(true == getHasDefaultValues()) {
		retVal.reset(new vector<double>(defaultValues));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there are units
 * @return the flag indicating if there are units 
 */
bool DoubleArrayParamDef::getHasUnits()
{
	return hasUnits;
}

/*
 * Accessor for flag indicating if there is maxLen
 * @return the flag indicating if there is maxlen value 
 */
bool DoubleArrayParamDef::getHasMaxLen()
{
	return hasMaxLen;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value
 */
bool DoubleArrayParamDef::getHasDefaultValues()
{
	return hasDefaultValues;
}
