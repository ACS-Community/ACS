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
* "@(#) $Id: BoolParamDef.cpp,v 1.2 2005/01/24 23:03:09 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <BoolParamDef.h>
#include <iostream>

using namespace parameterSet;
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
BoolParamDef::BoolParamDef(string nameVal, string helpVal, string promptVal, bool isRequired, auto_ptr< bool > defaultVal)
{
	name = nameVal;	
	help = helpVal;
	prompt = promptVal;
	required = isRequired;
	if(NULL != defaultVal.get()) {
		hasDefault = true;
		defaultValue = *defaultVal;
	}
	else {
		hasDefault = false;
		defaultValue = false;
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
   return defaultValue;
}

/*
 * Accessor for the flag indicating if there is a default value.
 * @return the flag indicating if there is a default value 
 */
bool BoolParamDef::getHasDefault()
{
   return hasDefault;
}

