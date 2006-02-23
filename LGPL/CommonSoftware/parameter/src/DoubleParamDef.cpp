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
* "@(#) $Id: DoubleParamDef.cpp,v 1.3 2005/01/26 04:36:41 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <DoubleParamDef.h>

using namespace parameterSet;

/**
 * Constructor.
 */
DoubleParamDef::DoubleParamDef()
{
}

/**
 * Constructor.
 */
DoubleParamDef::DoubleParamDef(string nameVal, string helpVal, string promptVal, bool isRequired, 
	auto_ptr< double > defaultVal, auto_ptr< string > strDefault, auto_ptr< string > unitsVal, 
	auto_ptr< double > maxVal, auto_ptr< double > minVal, auto_ptr< vector<double> > validVals)
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
	if(NULL != strDefault.get()) {
		defaultString = *strDefault;
		hasStringDefault = true;
	}
	else {
		hasStringDefault = false;
	}
	if(NULL != unitsVal.get()) {
		units = *unitsVal;
		hasUnits = true;
	}
	else {
		hasUnits = false;
	}
	if(NULL != maxVal.get()) {
		max = *maxVal;
		hasMax = true;
	}
	else {
		hasMax = false;
	}
	if(NULL != minVal.get()) {
		min = *minVal;
		hasMin = true;
	}
	else {
		hasMin = false;
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
		retVal.reset(new double(defaultValue));
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
		retVal.reset(new string(defaultString));
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
		retVal.reset(new string(units));
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
		retVal.reset(new double(max));
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
		retVal.reset(new double(min));
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
		retVal.reset(new vector<double>(validValues));
	}
	return retVal;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value as a ptr to a double.
 */
bool DoubleParamDef::getHasDefault()
{
	return hasDefault;
}

/*
 * Accessor for the flag indicating if there is a default string value.
 * @return the flag indicating if there is a default string value as a ptr to a string.
 */
bool DoubleParamDef::getHasStringDefault()
{
	return hasStringDefault;
}

/*
 * Accessor for the flag indicating if there is a units.
 * @return the flag indicating if there is a units as a ptr to a string.
 */
bool DoubleParamDef::getHasUnits()
{
	return hasUnits;
}

/*
 * Accessor for the flag indicating if there is a max value.
 * @return the flag indicating if there is a max value as a ptr to a double.
 */
bool DoubleParamDef::getHasMax()
{
	return hasMax;
}

/*
 * Accessor for the flag indicating if there is a min value.
 * @return the flag indicating if there is a min value as a ptr to a double.
 */
bool DoubleParamDef::getHasMin()
{
	return hasMin;
}

/*
 * Accessor for the flag indicating if there is a valid values.
 * @return the flag indicating if there is a valid values as a ptr to a vector of doubles.
 */
bool DoubleParamDef::getHasValidValues()
{
	return hasValidValues;
}
