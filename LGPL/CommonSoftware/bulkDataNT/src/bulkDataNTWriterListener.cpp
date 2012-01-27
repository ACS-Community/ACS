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
* "@(#) $Id: bulkDataNTWriterListener.cpp,v 1.5 2012/01/27 14:40:37 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-14  created 
*/

#include "bulkDataNTWriterListener.h"

#include <ACS_DDS_Errors.h>
#include <iostream>

using namespace std;
using namespace AcsBulkdata;

BulkDataNTWriterListener::BulkDataNTWriterListener(const char *name, BulkDataNTSenderFlowCallback* cb)
		: BulkDataNTDDSLoggable("BulkDataNT:"+string(name)),
		topicName_m(name), callback_mp(cb)
{
	ACS_TRACE(__PRETTY_FUNCTION__);
	sum_unacknowledged_sample = 0;
	max_unacknowledged_sample =0;
	iter=0;
}

// Implementation skeleton destructor
BulkDataNTWriterListener::~BulkDataNTWriterListener ()
{
	ACS_TRACE(__PRETTY_FUNCTION__);
}

void BulkDataNTWriterListener::on_offered_deadline_missed (
		::DDS::DataWriter* writer,
		const ::DDS::OfferedDeadlineMissedStatus & status)
{
	cerr << topicName_m << " BulkDataNTWriterListener::on_offered_deadline_missed" << endl;
}

void BulkDataNTWriterListener::on_offered_incompatible_qos (
		::DDS::DataWriter* writer,
		const ::DDS::OfferedIncompatibleQosStatus & status)
{
	cerr << topicName_m << " BulkDataNTWriterListener::on_offered_incompatible_qos" << endl;
}


void BulkDataNTWriterListener::on_liveliness_lost (
		::DDS::DataWriter* writer,
		const ::DDS::LivelinessLostStatus & status)
{
	cerr << topicName_m << " BulkDataNTWriterListener::on_liveliness_lost" << endl;
}


void BulkDataNTWriterListener::on_publication_matched (DDS::DataWriter* writer,	const DDS::PublicationMatchedStatus & status)
{
	ACS_TRACE(__FUNCTION__);
}//on_publication_matched

void BulkDataNTWriterListener::on_reliable_writer_cache_changed(DDSDataWriter* writer,
		const DDS_ReliableWriterCacheChangedStatus& status)
{
	sum_unacknowledged_sample += status.unacknowledged_sample_count;
	if( status.unacknowledged_sample_count > max_unacknowledged_sample)
		max_unacknowledged_sample = status.unacknowledged_sample_count;
	iter++;
/*
	cerr << "==========> BDDDSWriterListenerImpl::on_reliable_writer_cache_changed" << endl;
	cerr << "==========> unacknowledged_sample_count: " << status.unacknowledged_sample_count;
	cerr << " unacknowledged_sample_count_peak: " << status.unacknowledged_sample_count_peak << endl;
*/
}

void BulkDataNTWriterListener::on_reliable_reader_activity_changed(DDSDataWriter* writer,
		const DDS::ReliableReaderActivityChangedStatus& status)
{
	cerr <<  topicName_m << " ==========> BulkDataNTWriterListener::on_reliable_reader_activity_changed:" << endl;
	cerr <<  topicName_m << "   ==========>	status.active_count:" << status.active_count << endl;
	cerr <<  topicName_m << "   ==========>	status.active_count_change:" << status.active_count_change << endl;
	cerr <<  topicName_m << "   ==========>	status.inactive_count:" << status.inactive_count << endl;
	cerr <<  topicName_m << "   ==========>	status.inactive_count_change:" << status.inactive_count_change << endl;
}

void BulkDataNTWriterListener::on_destination_unreachable(DDSDataWriter* writer,
		const DDS_InstanceHandle_t& handle,
		const DDS_Locator_t& destination)
{
	cerr <<  topicName_m << " BulkDataNTWriterListener::on_destination_unreachable" << endl;
}

/*___oOo___*/
