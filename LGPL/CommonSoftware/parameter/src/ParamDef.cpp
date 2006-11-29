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
* "@(#) $Id: ParamDef.cpp,v 1.3 2006/11/29 23:01:27 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  27/09/04  created 
*/

#include <ParamDef.h>

using namespace Parameters;

/**
 * Constructor.
 */
ParamDef::ParamDef()
{
}

/**
 * Constructor.
 */
ParamDef::ParamDef(const string & nameVal, const string & helpVal, const string & promptVal, bool isRequired): 
	required_m(isRequired), name_m(nameVal), help_m(helpVal), prompt_m(promptVal)
{
}

/**
 * Destructor.
 */
ParamDef::~ParamDef()
{
}

/*
 * Accessor for whether this parameter is required (i.e. mandatory).
 * @return boolean indicating whether this param is required (mandatory) (true) or not (false). 
 */
bool ParamDef::isRequired()
{
   return required_m;
}

/*
 * Accessor for the name.
 * @return the name as a string.
 */
string ParamDef::getName()
{
   return name_m;
}

/*
 * Accessor for the help.
 * @return the help as a string.
 */
string ParamDef::getHelp()
{
   return help_m;
}
    
/*
 * Accessor for the prompt.
 * @return the prompt as a string.
 */
string ParamDef::getPrompt()
{
   return prompt_m;
}


