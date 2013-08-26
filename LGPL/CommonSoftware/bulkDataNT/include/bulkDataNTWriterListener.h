#ifndef BULKDATA_NT_WRITER_LISTENER
#define BULKDATA_NT_WRITER_LISTENER
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
* "@(#) $Id: bulkDataNTWriterListener.h,v 1.4 2012/01/27 14:40:37 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-14  created
*/
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataNTSenderFlowCallback.h"
#include "bulkDataNTDDSLoggable.h"
#include <ndds/ndds_namespace_cpp.h>


namespace AcsBulkdata
{

class BulkDataNTWriterListener :
	public virtual DDS::DataWriterListener,
	public BulkDataNTDDSLoggable
{
public:
	//Constructor
	BulkDataNTWriterListener(const char *name, BulkDataNTSenderFlowCallback* cb);

	//Destructor
	virtual ~BulkDataNTWriterListener ();

	virtual void on_offered_deadline_missed (
			::DDS::DataWriter* writer,
			const ::DDS::OfferedDeadlineMissedStatus & status);


	virtual void on_offered_incompatible_qos (
			::DDS::DataWriter* writer,
			const ::DDS::OfferedIncompatibleQosStatus & status);



	virtual void on_liveliness_lost (
			::DDS::DataWriter* writer,
			const ::DDS::LivelinessLostStatus & status);



	virtual void on_publication_matched (
			::DDS::DataWriter* writer,
			const ::DDS::PublicationMatchedStatus & status);

	// RTI specific ?
	virtual void on_reliable_writer_cache_changed(DDSDataWriter* writer,
			const DDS_ReliableWriterCacheChangedStatus& status);

	virtual void on_reliable_reader_activity_changed(DDSDataWriter* writer,
			const DDS_ReliableReaderActivityChangedStatus& status);


	virtual void on_destination_unreachable(DDSDataWriter* writer,
			const DDS_InstanceHandle_t& handle,
			const DDS_Locator_t& destination);

private:
	std::string topicName_m;  /// name of DDS topic

	BulkDataNTSenderFlowCallback* callback_mp; /// pointer to user defined callback

	unsigned long sum_unacknowledged_sample;
	long max_unacknowledged_sample;
	unsigned long iter;
};

};//namespace AcsBulkdata

#endif /*!_H*/
