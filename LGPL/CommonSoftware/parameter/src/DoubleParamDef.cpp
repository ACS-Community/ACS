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
* "@(#) $Id: DoubleParamDef.cpp,v 1.4 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <DoubleParamDef.h>

using namespace Parameters;

/**
 * Constructor.
 */
DoubleParamDef::DoubleParamDef()
{
}

/**
 * Constructor.
 */
DoubleParamDef::DoubleParamDef(const string & nameVal, const string & helpVal, const string & promptVal, bool isRequired, 
	auto_ptr< double > defaultVal, auto_ptr< string > strDefault, auto_ptr< string > unitsVal, 
	auto_ptr< double > maxVal, auto_ptr< double > minVal, auto_ptr< vector<double> > validVals): 
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
		defaultString_m = *strDefault;
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
DoubleParamDef::~DoubleParamDef()
{
}

/*
 * Accessor for the default value.
 * @return the default value as a double.
 */
auto_ptr<double> DoubleParamDef::getDefault()
{
	auto_ptr<double> retVal;
	if(true == getHasDefault()) {
		retVal.reset(new double(defaultValue_m));
	}
	return retVal;
}

/*
 * Accessor for the default string value.
 * @return the default string  value as a string.
 */
auto_ptr<string> DoubleParamDef::getStringDefault()
{
	auto_ptr<string> retVal;
	if(true == getHasStringDefault()) {
		retVal.reset(new string(defaultString_m));
	}
	return retVal;
}

/*
 * Accessor for the units
 * @return the units value as a int ptr.
 */
auto_ptr< string > DoubleParamDef::getUnits()
{
	auto_ptr<string> retVal;
	if(true == getHasUnits()) {
		retVal.reset(new string(units_m));
	}
	return retVal;
}

/*
 * Accessor for the max value.
 * @return the max value as a double.
 */
auto_ptr<double> DoubleParamDef::getMax()
{
	auto_ptr<double> retVal;
	if(true == getHasMax()) {
		retVal.reset(new double(max_m));
	}
	return retVal;
}

/*
 * Accessor for the min value.
 * @return the min value as a double.
 */
auto_ptr<double> DoubleParamDef::getMin()
{
	auto_ptr<double> retVal;
	if(true == getHasMin()) {
		retVal.reset(new double(min_m));
	}
	return retVal;
}

/*
 * Accessor for the valid values.
 * @return the valid values as a ptr to a vector of doubles.
 */
auto_ptr< vector< double > > DoubleParamDef::getValidValues()
{
	auto_ptr<vector <double> > retVal;
	if(true == getHasValidValues()) {
		retVal.reset(new vector<double>(validValues_m));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value as a ptr to a double.
 */
bool DoubleParamDef::getHasDefault()
{
	return hasDefault_m;
}

/*
 * Accessor for the flag indicating if there is a default string value.
 * @return the flag indicating if there is a default string value as a ptr to a string.
 */
bool DoubleParamDef::getHasStringDefault()
{
	return hasStringDefault_m;
}

/*
 * Accessor for the flag indicating if there is a units.
 * @return the flag indicating if there is a units as a ptr to a string.
 */
bool DoubleParamDef::getHasUnits()
{
	return hasUnits_m;
}

/*
 * Accessor for the flag indicating if there is a max value.
 * @return the flag indicating if there is a max value as a ptr to a double.
 */
bool DoubleParamDef::getHasMax()
{
	return hasMax_m;
}

/*
 * Accessor for the flag indicating if there is a min value.
 * @return the flag indicating if there is a min value as a ptr to a double.
 */
bool DoubleParamDef::getHasMin()
{
	return hasMin_m;
}

/*
 * Accessor for the flag indicating if there is a valid values.
 * @return the flag indicating if there is a valid values as a ptr to a vector of doubles.
 */
bool DoubleParamDef::getHasValidValues()
{
	return hasValidValues_m;
}


