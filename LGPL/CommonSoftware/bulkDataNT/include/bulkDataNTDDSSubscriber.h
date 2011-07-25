#ifndef _BULK_DATA_NT_DDS_SUBSCRIBER_H_
#define _BULK_DATA_NT_DDS_SUBSCRIBER_H_
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
* "@(#) $Id: bulkDataNTDDSSubscriber.h,v 1.3 2011/07/25 13:50:59 bjeram Exp $"
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

#include "bulkDataNTDDS.h"
#include "bulkDataNTDataReader.hh"

namespace AcsBulkdata
{

/**
 * class responsible for all DDS Subscriber related details
 */

class BulkDataNTDDSSubscriber : public BulkDataNTDDS
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTDDSSubscriber();
	BulkDataNTDDSSubscriber(const DDS::DomainParticipant *p);

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTDDSSubscriber();

		//should return generic writer and have another method in Base class that narrows
	ACSBulkData::BulkDataNTFrameDataReader* createDDSReader(DDS::Topic *topic, DDS::DataReaderListener* listener);
protected:
	DDS::Subscriber* createDDSSubscriber();
	DDS::Subscriber *subscriber_m;
};//class BulkDataNTDDSPublisher

};

#endif
