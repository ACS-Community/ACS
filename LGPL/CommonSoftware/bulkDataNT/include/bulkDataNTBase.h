#ifndef _BULK_DATA_NT_BASE_H_
#define _BULK_DATA_NT_BASE_H_
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
* "@(#) $Id: bulkDataNTBase.h,v 1.2 2011/07/07 15:05:38 bjeram Exp $"
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


#include <vector>
#include <string>
#include <Message_Block.h>
#include "bulkDataNTDDS.h"


namespace AcsBulkdata
{

typedef unsigned long FlowNumberType;

/**
 * structure for keeping flow related data
 */
struct FlowData
{
	std::string topicName; // = flow name = stream name + flow number
	DDS::Topic *topic;

};


/**
 *  base class for Bulk data sender and receiver
 *  TBD probably we do not need it
 */
class BulkDataNTBase
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTBase();

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTBase();

protected:

	unsigned int numOfFlows_m;

	//BDFlow flows[];
	std::string streamName_m;


};//class BulkDataNTBase

};

#endif
