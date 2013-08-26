#ifndef _STRING_PARAM_DEF_H
#define _STRING_PARAM_DEF_H
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
* "@(#) $Id: StringParamDef.h,v 1.3 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <memory>
#include <string>
#include <vector>
#include <ParamDef.h>

/** @file StringParamDef.h */
using std::string;
using std::vector;
using std::auto_ptr;

namespace Parameters {

	/**
	 * StringParamDef class used to support OFFLINE tasks
	 */
	class StringParamDef: public Parameters::ParamDef
	{    
	  public:
	    /**
	     * Constructor
	     */
	    StringParamDef();

	    /**
	     * Constructor.
	     */
	    StringParamDef(const string & nameVal, const string & helpVal, const string & promptVal, 
	       bool isRequired, auto_ptr< string > defaultVal, auto_ptr< vector < string > > validVals);
	    
	    /**
	     * Destructor
	     */
	    virtual ~StringParamDef();
	    
	    /*
	     * Accessor for the default value.
	     * @return the default value as an auto_ptr to a string
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<string> getDefault();
	    
	    /*
	     * Accessor for the valid values.
	     * @return the valid values as an auto_ptr to a vector of strings
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr< vector< string > > getValidValues();
	    
	    /*
	     * Accessor for the flag indicating if there is a default value.
	     * @return the flag indicating if there is a default value
	     */
	    bool getHasDefault();
	    
	    /*
	     * Accessor for the flag indicating if there is a valid values.
	     * @return the flag indicating if there is a valid values
	     */
	    bool getHasValidValues();

	  private:
	    string defaultValue_m;
	    vector< string > validValues_m;
	    bool hasDefault_m;
	    bool hasValidValues_m;
	};

}
#endif /*!_STRING_PARAM_DEF_H*/



