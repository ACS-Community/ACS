#ifndef _BULK_DATA_NT_DDS_PUBLISHER_H_
#define _BULK_DATA_NT_DDS_PUBLISHER_H_
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
* "@(#) $Id: bulkDataNTDDSPublisher.h,v 1.11 2011/12/15 15:09:20 bjeram Exp $"
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
//CoreDX
//#include "bulkDataNTDataWriter.hh"

namespace AcsBulkdata
{

/**
 * class responsible for all DDS Publisher related details
 */

class BulkDataNTDDSPublisher : public BulkDataNTDDS
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTDDSPublisher(DDS::DomainParticipant *p, const DDSConfiguration &ddsCfg);


	/**
	 * Destructor
	 */
	virtual ~BulkDataNTDDSPublisher();



	//should return generic writer and have another method in Base class that narrows
	ACSBulkData::BulkDataNTFrameDataWriter* createDDSWriter(DDS::Topic *topic, DDS::DataWriterListener *listener);


	void destroyDDSWriter(ACSBulkData::BulkDataNTFrameDataWriter* dw);

	void setWriteBlockingTime(double frameTimeout);

protected:
	DDS::Publisher *publisher_m;

	DDS::Publisher* createDDSPublisher();

	///Destroys publisher_m
	void  destroyDDSPublisher();

	ACSBulkData::BulkDataNTFrameDataWriter* dataWriter_m; /// DDS data writer

	/// disable default - empty constructor
	BulkDataNTDDSPublisher();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTDDSPublisher&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTDDSPublisher(const BulkDataNTDDSPublisher&);
};//class BulkDataNTDDSPublisher

};

#endif
