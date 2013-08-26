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
* "@(#) $Id: IntParamDef.cpp,v 1.3 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <IntParamDef.h>
#include <limits.h>
#include <string>
#include <vector>

using namespace std;
using namespace Parameters;

/**
 * Constructor.
 */
IntParamDef::IntParamDef()
{
}

/**
 * Constructor.
 */
IntParamDef::IntParamDef(const string & nameVal, const string & helpVal, const string & promptVal, 
	bool isRequired, auto_ptr< int > defaultVal, auto_ptr< string > strDefault, auto_ptr< string > unitsVal, 
	auto_ptr< int > maxVal, auto_ptr< int > minVal, auto_ptr< vector<int> > validVals):
		ParamDef(nameVal, helpVal, promptVal, isRequired)
{
	if(NULL != defaultVal.get()) {
		defaultValue_m = *defaultVal;
		hasDefault_m = true;
	}
	else {
		hasDefault_m = false;
	}
	if(NULL != strDefault.get()) {
		stringDefault_m = *strDefault;
		hasStringDefault_m = true;
	}
	else {
		hasStringDefault_m = false;
	}
	if(NULL != unitsVal.get()) {
		units_m = *unitsVal;
		hasUnits_m = true;
	}
	else {
		hasUnits_m = false;
	}
	if(NULL != maxVal.get()) {
		max_m = *maxVal;
		hasMax_m = true;
	}
	else {
		hasMax_m = false;
	}
	if(NULL != minVal.get()) {
		min_m = *minVal;
		hasMin_m = true;
	}
	else {
		hasMin_m = false;
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
IntParamDef::~IntParamDef()
{
}

/*
 * Accessor for the default value.
 * @return the default value as a int ptr.
 */
auto_ptr< int > IntParamDef::getDefault()
{
	auto_ptr<int> retVal;
	if(true == getHasDefault()) {
		retVal.reset(new int(defaultValue_m));
	}
	return retVal;
}

/*
 * Accessor for the string default value.
 * @return the default value as a int ptr.
 */
auto_ptr< string > IntParamDef::getStringDefault()
{
	auto_ptr<string> retVal;
	if(true == getHasStringDefault()) {
		retVal.reset(new string(stringDefault_m));
	}
	return retVal;
}

/*
 * Accessor for the units
 * @return the units value as a int ptr.
 */
auto_ptr< string > IntParamDef::getUnits()
{
	auto_ptr<string> retVal;
	if(true == getHasUnits()) {
		retVal.reset(new string(units_m));
	}
	return retVal;
}

/*
 * Accessor for the max value.
 * @return the max value as a int ptr.
 */
auto_ptr< int > IntParamDef::getMax()
{
	auto_ptr<int> retVal;
	if(true == getHasMax()) {
		retVal.reset(new int(max_m));
	}
	return retVal;
}

/*
 * Accessor for the min value.
 * @return the min value as a int ptr.
 */
auto_ptr< int > IntParamDef::getMin()
{
	auto_ptr<int> retVal;
	if(true == getHasMin()) {
		retVal.reset(new int(min_m));
	}
	return retVal;
}

/*
 * Accessor for the valid values
 * @return the valid values as a ptr to a vector of int
 */
auto_ptr< vector<int> > IntParamDef::getValidValues()
{
	auto_ptr<vector <int> > retVal;
	if(true == getHasValidValues()) {
		retVal.reset(new vector<int>(validValues_m));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value as a int ptr.
 */
bool IntParamDef::getHasDefault()
{
   return hasDefault_m;
}

/*
 * Accessor for the flag indicating if there is a string default value.
 * @return the flag indicating if there is a default value as a int ptr.
 */
bool IntParamDef::getHasStringDefault()
{
   return hasStringDefault_m;
}

/*
 * Accessor for the flag indicating if there is a units
 * @return the flag indicating if there is a units value as a int ptr.
 */
bool IntParamDef::getHasUnits()
{
   return hasUnits_m;
}

/*
 * Accessor for the flag indicating if there is a max value.
 * @return the flag indicating if there is a max value as a int ptr.
 */
bool IntParamDef::getHasMax()
{
   return hasMax_m;
}

/*
 * Accessor for the flag indicating if there is a min value.
 * @return the flag indicating if there is a min value as a int ptr.
 */
bool IntParamDef::getHasMin()
{
   return hasMin_m;
}

/*
 * Accessor for the flag indicating if there is a valid values
 * @return the flag indicating if there is a valid values as a ptr to a vector of int
 */
bool IntParamDef::getHasValidValues()
{
   return hasValidValues_m;
}


