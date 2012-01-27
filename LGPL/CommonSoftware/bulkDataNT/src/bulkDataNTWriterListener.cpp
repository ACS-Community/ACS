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
* "@(#) $Id: bulkDataNTWriterListener.cpp,v 1.6 2012/01/27 15:15:36 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-14  created 
*/

#include "bulkDataNTWriterListener.h"
#include "ACS_BD_Errors.h"
#include <ACS_DDS_Errors.h>
#include <iostream>

using namespace std;
using namespace AcsBulkdata;
using namespace ACS_BD_Errors;
using namespace ACS_DDS_Errors;

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
	DDSOffeeredDeadlineMissedCompletion iqerr(__FILE__, __LINE__, __FUNCTION__);
	initalizeLogging(); //force initialization of logging sys TBD changed
	callback_mp->onError(iqerr);
}

void BulkDataNTWriterListener::on_offered_incompatible_qos (
		::DDS::DataWriter* writer,
		const ::DDS::OfferedIncompatibleQosStatus & status)
{
	DDSOfferedIncompatibleQoSCompletion iqerr(__FILE__, __LINE__, __FUNCTION__);
	initalizeLogging(); //force initialization of logging sys TBD changed
	callback_mp->onError(iqerr);
}


void BulkDataNTWriterListener::on_liveliness_lost (
		::DDS::DataWriter* writer,
		const ::DDS::LivelinessLostStatus & status)
{
	DDSLivelinesLostCompletion llcomp(__FILE__, __LINE__, __FUNCTION__);
	initalizeLogging(); //force initialization of logging sys TBD changed
	callback_mp->onError(llcomp);
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
	if (status.active_count_change>0)
	    {
	      for(int i=0; i<status.active_count_change; i++)
	        {
	          ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
	              (LM_INFO, "A new receiver has connected to flow: %s of the stream: %s. Total alive connection(s): %d",
	                  callback_mp->getFlowName(), callback_mp->getStreamName(),
	                  status.active_count));
	          //BDNT_READER_LISTENER_USER_ERR( callback_mp->onReceiverConnect() )
	          callback_mp->onReceiverConnect(status.active_count);
	        }//for
	    }else
	      {
	        for(int i=status.active_count_change; i<0; i++)
	          {
	            ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
	                (LM_INFO, "A receiver has disconnected from flow: %s of the stream: %s. Total alive connection(s): %d",
	                    callback_mp->getFlowName(), callback_mp->getStreamName(),
	                    status.active_count));
	            //BDNT_READER_LISTENER_USER_ERR( callback_mp->onReceiverDisconnect() )
	            callback_mp->onReceiverDisconnect(status.active_count);
	          }//for
	      }//if-else
}

void BulkDataNTWriterListener::on_destination_unreachable(DDSDataWriter* writer,
		const DDS_InstanceHandle_t& handle,
		const DDS_Locator_t& destination)
{

	DDSDestinationUnreachableCompletion  du(__FILE__, __LINE__, __FUNCTION__);
	initalizeLogging(); //force initialization of logging sys TBD changed
	callback_mp->onError(du);
}

/*___oOo___*/
