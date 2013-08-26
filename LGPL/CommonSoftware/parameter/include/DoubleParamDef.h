#ifndef _DOUBLE_PARAM_DEF_H
#define _DOUBLE_PARAM_DEF_H
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
* "@(#) $Id: DoubleParamDef.h,v 1.4 2006/11/29 23:01:26 sharring Exp $"
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

/** @file DoubleParamDef.h */

namespace Parameters {

	/**
	 * DoubleParamDef class used to support OFFLINE tasks
	 */
	class DoubleParamDef: public Parameters::ParamDef
	{    
	  public:
	    /**
	     * Constructor
	     */
	    DoubleParamDef();
	    
	    /**
	     * Constructor.
	     */
		DoubleParamDef(const string & nameVal, const string & helpVal,const  string & promptVal, 
			bool isRequired, auto_ptr< double > defaultVal, auto_ptr< string > strDefault, auto_ptr< string > unitsVal, 
			auto_ptr< double > maxVal, auto_ptr< double > minVal, auto_ptr< vector<double> > validVals);

	    /**
	     * Destructor
	     */
	    virtual ~DoubleParamDef();
	    
	    /*
	     * Accessor for the default value.
	     * @return the default value as a auto_ptr to a double
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<double> getDefault();

	    /*
	     * Accessor for the default string value.
	     * @return the default string value as a auto_ptr to a string
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<string> getStringDefault();
	    
	    /*
	     * Accessor for the units.
	     * @return the units as a auto_ptr to a string
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<string> getUnits();
	    
	    /*
	     * Accessor for the max value.
	     * @return the max value as a auto_ptr to a double
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<double> getMax();
	    
	    /*
	     * Accessor for the min value.
	     * @return the min value as a auto_ptr to a double
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr<double> getMin();
	    
	    /*
	     * Accessor for the valid values.
	     * @return the valid values as a auto_ptr to a vector of doubles
	     * if defined for this pdef, then the auto_ptr will 
	     * point to something meaningful, else null.
	     */
	    auto_ptr< vector< double > > getValidValues();
	    
	    /*
	     * Accessor for the flag indicating if there is a default value
	     * @return the flag indicating if there is a default value
	     */
	    bool getHasDefault();

	    /*
	     * Accessor for the flag indicating if there is a default string value
	     * @return the flag indicating if there is a default string value
	     */
	    bool getHasStringDefault();
	    
	    /*
	     * Accessor for the flag indicating if there is a units
	     * @return the flag indicating if there are units
	     */
	    bool getHasUnits();
	    
	    /*
	     * Accessor for the flag indicating if there is a max value
	     * @return the flag indicating if there is a max value
	     */
	    bool getHasMax();
	    
	    /*
	     * Accessor for the flag indicating if there is a min value
	     * @return the flag indicating if there is a min value
	     */
	    bool getHasMin();
	    
	    /*
	     * Accessor for the flag indicating if there is a valid values
	     * @return the flag indicating if there is a valid values
	     */
	    bool getHasValidValues();

	  private:
	    double defaultValue_m;
	    string defaultString_m;
	    string units_m;
	    double max_m;
	    double min_m;
	    vector<double> validValues_m;

	    bool hasDefault_m;
	    bool hasStringDefault_m;
	    bool hasUnits_m;
	    bool hasMax_m;
	    bool hasMin_m;
	    bool hasValidValues_m;
	};

}
#endif /*!_DOUBLE_PARAM_DEF_H*/


