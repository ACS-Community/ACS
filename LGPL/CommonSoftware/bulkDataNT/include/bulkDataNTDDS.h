#ifndef _BULK_DATA_NT_DDS_H_
#define _BULK_DATA_NT_DDS_H_
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
* "@(#) $Id: bulkDataNTDDS.h,v 1.7 2011/07/27 10:06:11 bjeram Exp $"
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

//COREDX
//#include "bulkDataNTTypeSupport.hh"

//RTI
#ifndef RTI_UNIX
#define RTI_UNIX
#endif
#ifndef RTI_LINUX
#define RTI_LINUX
#endif


//DDS RTI generated
#include "bulkDataNTDDS.h"
#include "bulkDataNTSupport.h"

//DDS RTI library
#include "ndds/ndds_cpp.h"
#include "ndds/ndds_namespace_cpp.h"


#include <logging.h>
#include <ACSErrTypeCommon.h>
#include "ACS_BDError.h"

namespace AcsBulkdata
{

/**
 * class responsible for all DDS related details
 */
//TBD should be renamed to Stream
class BulkDataNTDDS
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTDDS(DDS::DomainParticipant*);

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTDDS();


	DDS::Topic* createDDSTopic(const char* topicName); // should return topic

protected:
/*
	void createDDSFactory();
	void createDDSParticipant();

*/
//	DDS::Publisher* createDDSPublisher(); // should return publisher ?
	//should return generic writer and have another method in Base class that narrows
//	ACSBulkData::BulkDataNTFrameDataWriter* createDDSWriter(DDS::Publisher* pub, DDS::Topic *topic);


//	DDS::DomainParticipantFactory *factory;
	DDS::DomainParticipant* participant_m;

//	DDS::Publisher* pub;
//	DDS::Topic* topic; // should be  an array just temporary
	// + we need array of data writers
	// so for each flow there is at least a topic + writer + publisher(?)
//	ACSBulkData::BulkDataNTFrameDataWriter *dw;

	/// disable default - empty constructor
	BulkDataNTDDS();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTDDS&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTDDS(const BulkDataNTDDS&);
};//class BulkDataNTDDS

};

#endif
