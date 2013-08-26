#ifndef _PARAM_DEF_H
#define _PARAM_DEF_H
/*******************************************************************************
*    ALMA - Atacama Large Millimiter Array
*    (c) Associated Universities Inc., 2002 
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
* "@(#) $Id: ParamDef.h,v 1.3 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>
#include <memory>

using std::string;

/** @file ParamDef.h */

namespace Parameters {

	/**
	 * ParamDef class - abstract base class used to store information about individual params within a ParameterSetDef
	 *                  as defined by the task author of a particular OFFLINE task.
	 */
	class ParamDef
	{    
	  public:
	    /**
	     * Constructor
	     */
	    ParamDef();

	    /**
	     * Constructor
	     */
	    ParamDef(const string & nameVal, const string & helpVal, const string & promptVal, bool isRequired);
	    
	    /**
	     * Destructor
	     */
	    virtual ~ParamDef() = 0;
	    
	    /*
	     * Accessor for whether this parameter is required (i.e. mandatory).
	     * @return boolean indicating whether this param is required (mandatory) (true) or not (false). 
	     */
	    bool isRequired();

	    /*
	     * Accessor for the name.
	     * @return the name as a string.
	     */
	    string getName();

	    /*
	     * Accessor for the help.
	     * @return the help as a string.
	     */
	    string getHelp();
	    
	    /*
	     * Accessor for the prompt.
	     * @return the prompt as a string.
	     */
	    string getPrompt();
	    
	  protected:
	    bool required_m;
	    string name_m;
	    string help_m;
	    string prompt_m;
	};

}
#endif /*!_PARAM_DEF_H*/


