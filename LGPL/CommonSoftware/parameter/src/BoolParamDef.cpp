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
* "@(#) $Id: BoolParamDef.cpp,v 1.3 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <BoolParamDef.h>
#include <iostream>

using namespace Parameters;
using namespace std;

/**
 * Constructor.
 */
BoolParamDef::BoolParamDef()
{
}

/**
 * Constructor.
 */
BoolParamDef::BoolParamDef(const string & nameVal, const string & helpVal, const string & promptVal, bool isRequired, auto_ptr< bool > defaultVal) : 
	ParamDef(nameVal, helpVal, promptVal, isRequired)
{
	if(NULL != defaultVal.get()) {
		hasDefault_m = true;
		defaultValue_m = *defaultVal;
	}
	else {
		hasDefault_m = false;
		defaultValue_m = false;
	}
}

/**
 * Destructor.
 */
BoolParamDef::~BoolParamDef()
{
}

/*
 * Accessor for the default value.
 * @return the default value 
 */
bool BoolParamDef::getDefault()
{
   return defaultValue_m;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value 
 */
bool BoolParamDef::getHasDefault()
{
   return hasDefault_m;
}



