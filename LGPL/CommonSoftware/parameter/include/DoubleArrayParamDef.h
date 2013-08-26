#ifndef _DOUBLE_ARRAY_PARAM_DEF_H
#define _DOUBLE_ARRAY_PARAM_DEF_H
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
* "@(#) $Id: DoubleArrayParamDef.h,v 1.3 2006/11/29 23:01:26 sharring Exp $"
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
#include <string>
#include <vector>

using std::auto_ptr;
using std::string;
using std::vector;

/** @file DoubleArrayParamDef.h */

using std::auto_ptr;

namespace Parameters {

	/**
	 * DoubleArrayParamDef class used to support OFFLINE tasks
	 */
	class DoubleArrayParamDef: public Parameters::ParamDef
	{    
	  public:
	    /**
	     * Constructor
	     */
	    DoubleArrayParamDef();
	    
	    /**
	     * Constructor.
	     */
	    DoubleArrayParamDef(const string & paramName, const string & helpText, const string & promptText, 
	       bool requiredBoolVal, auto_ptr< string > unitsText, auto_ptr< int > maxLen, auto_ptr< vector < double > > defaultVals);

	    /**
	     * Destructor
	     */
	    virtual ~DoubleArrayParamDef();
	    
	    /*
	     * Accessor for the units. 
	     * @return the units as an  auto_ptr to a string; 
	     * if units have been defined for this pdef, then the auto_ptr will 
	     * point to a string of the units, else it will have be null. 
	     */
	    auto_ptr<string> getUnits();

	    /*
	     * Accessor for the maxLen
	     * @return the maxlen value as a ptr to int
	     * if maxLen has been defined for this pdef, then the auto_ptr will 
	     * point to an int of the maxLen, else it will pt to a null. 
	     */
	    auto_ptr<int> getMaxLen();

	    /*
	     * Accessor for the default value.
	     * @return the default value as a vector of double
	     * if default values have been defined for this pdef, then the auto_ptr will 
	     * point to a vector of doubles of the defaultvalues, else it will pt to a null. 
	     */
	    auto_ptr< vector<double> > getDefaultValues();
	    
	    /*
	     * Accessor for the flag indicating if there are units
	     * @return the flag indicating if there are units 
	     */
	    bool getHasUnits();

	    /*
	     * Accessor for flag indicating if there is maxLen
	     * @return the flag indicating if there is maxlen value 
	     */
	    bool getHasMaxLen();

	    /*
	     * Accessor for the flag indicating if there is a default value.
	     * @return the flag indicating if there is a default value
	     */
	    bool getHasDefaultValues();

	  private:
	    vector<double> defaultValues_m;
	    int maxLen_m;
	    string units_m;

	    bool hasDefaultValues_m;
	    bool hasMaxLen_m;
	    bool hasUnits_m;
	};

}
#endif /*!_DOUBLE_ARRAY_PARAM_DEF_H*/


