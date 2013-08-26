#ifndef _BULK_DATA_NT_FLOW_H_
#define _BULK_DATA_NT_FLOW_H_
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
*
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
*
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
*
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: bulkDataNTFlow.h,v 1.1 2011/11/09 16:20:48 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

namespace AcsBulkdata
{


/**
 *  base class for Bulk data sender and receiver flow
 *
 */
class BulkDataNTFlow
{

public:

	/**
	 * Constructor
	 */
	BulkDataNTFlow(const char* name) : flowName_m(name) {}

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTFlow(){}

	std::string getName() { return flowName_m; }

	//void destroyFlow(const  char* flowName);
protected:

	std::string flowName_m;

};//class BulkDataNTFlow

};

#endif
