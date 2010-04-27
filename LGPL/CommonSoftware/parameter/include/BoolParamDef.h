#ifndef _BOOL_PARAM_DEF_H
#define _BOOL_PARAM_DEF_H
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
* "@(#) $Id: BoolParamDef.h,v 1.4 2010/04/27 12:20:58 htischer Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* sharring  10/27/04  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ParamDef.h>
#include <string>
#include <memory>
#include <vector>

using std::auto_ptr;
using std::string;
using std::vector;

/** @file BoolParamDef.h */

namespace Parameters {

	/**
	 * BoolParamDef class used to support OFFLINE tasks
	 */
	class BoolParamDef: public Parameters::ParamDef
	{    
	  public:
	    /**
	     * Constructor
	     */
	    BoolParamDef();
	    
	    /**
	     * Constructor
	     */
		BoolParamDef(const string & nameVal, const string & helpVal, const string & promptVal, 
			bool isRequired, auto_ptr< bool > defaultVal);

	    /**
	     * Destructor
	     */
	    virtual ~BoolParamDef();
	    
	    /*
	     * Accessor for the default value.
	     * @return the default value 
	     */
	    bool getDefault();
	    
	    /*
	     * Accessor for the the flag indicating if there is a default value.
	     * @return the flag indicating if there is a default value
	     */
	    bool getHasDefault();
	    
	  private:
	    bool defaultValue_m;
	    bool hasDefault_m;
	};
}
#endif /*!_BOOL_PARAM_DEF_H*/


