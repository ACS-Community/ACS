#ifndef _STRING_ARRAY_PARAM_DEF_H
#define _STRING_ARRAY_PARAM_DEF_H
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
* "@(#) $Id: StringArrayParamDef.h,v 1.3 2006/11/29 23:01:26 sharring Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ParamDef.h>
#include <memory>
#include <vector>
#include <string>

using std::string;
using std::vector;
using std::auto_ptr;

/** @file StringArrayParamDef.h */

namespace Parameters {

	/**
	 * StringArrayParamDef class used to support OFFLINE tasks
	 */
	class StringArrayParamDef: public Parameters::ParamDef
	{    
	  public:
	    /**
	     * Constructor
	     */
	    StringArrayParamDef();
	    
	    /**
	     * Constructor.
	     */
	    StringArrayParamDef(const string & paramName, const string  & helpText, const string & promptText, 
	       bool requiredBoolVal, auto_ptr< int > maxLen, auto_ptr< vector < string > > defaultVals);

	    /**
	     * Destructor
	     */
	    virtual ~StringArrayParamDef();
	    
	    /*
	     * Accessor for the maxLen
	     * @return the maxlen value as an auto_ptr to int
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<int> getMaxLen();

	    /*
	     * Accessor for the default value.
	     * @return the default value as an auto_ptr to a vector of string.
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr< vector<string> > getDefaultValues();
	    
	    /*
	     * Accessor for the flag indicating if there is a maxLen
	     * @return the flag indicating if there is a maxlen value as a ptr to int
	     */
	    bool getHasMaxLen();

	    /*
	     * Accessor for the flag indicating if there is a default value.
	     * @return the flag indicating if there is a default value as a vector of string.
	     */
	    bool getHasDefaultValues();

	  private:
	    vector<string> defaultValues_m;
	    int maxLen_m;

	    bool hasDefaultValues_m;
	    bool hasMaxLen_m;
	};

}
#endif /*!_STRING_ARRAY_PARAM_DEF_H*/



