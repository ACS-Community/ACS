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
* "@(#) $Id: bulkDataNTDDSSubscriber.cpp,v 1.19 2012/01/19 15:53:55 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTDDSSubscriber.h"
#include <iostream>


using namespace AcsBulkdata;
using namespace std;
using namespace ACSErrTypeCommon;
using namespace ACS_DDS_Errors;


BulkDataNTDDSSubscriber::BulkDataNTDDSSubscriber(DDS::DomainParticipant *p, const DDSConfiguration &ddsCfg) :
		BulkDataNTDDS(p, ddsCfg)
{
	subscriber_m = createDDSSubscriber();
}

BulkDataNTDDSSubscriber::~BulkDataNTDDSSubscriber()
{
	try
	{
		destroyDDSSubscriber();
	}
	catch(ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}
}//~BulkDataNTDDSSubscriber

DDS::Subscriber* BulkDataNTDDSSubscriber::createDDSSubscriber()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	DDS::SubscriberQos sub_qos;

	if (participant_m==NULL)
	{
		printf("BulkDataNTDDSSubscriber::BulkDataNTDDSSubscriber participant NULL\n");
		return NULL;
	}

	DDS::Subscriber *sub = participant_m->create_subscriber_with_profile(
			ddsCfg_m.libraryQos.c_str(), ddsCfg_m.profileQos.c_str(),
			0, DDS::STATUS_MASK_NONE);
	if(sub==NULL)
	{
		DDSSubscriberCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		throw ex;
	}

	ret = sub->get_qos(sub_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSGetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("sub->get_qos()");
		throw ex;
	}//if

	/// dr has to be created disabled
	sub_qos.entity_factory.autoenable_created_entities=DDS_BOOLEAN_FALSE;

	ret = sub->set_qos(sub_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("sub->set_qos()");
		throw ex;
	}//if

	ret = sub->enable();
	if (ret!=DDS::RETCODE_OK)
	{
		DDSSubscriberEnableProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
	}

	return sub;
}//createDDSSubscriber

void BulkDataNTDDSSubscriber::destroyDDSSubscriber()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	ret = participant_m->delete_subscriber(subscriber_m);
	subscriber_m = 0;
	if (ret!=DDS::RETCODE_OK)
	{
		DDSSubscriberDestroyProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		throw ex;
	}//if
}//destroyDDSSubscriber

ACSBulkData::BulkDataNTFrameDataReader* BulkDataNTDDSSubscriber::createDDSReader(DDS::Topic *topic, DDS::DataReaderListener *listener)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;

	if (subscriber_m==NULL || topic==NULL || listener==NULL)
	{
		NullPointerExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setVariable("publisher_m, topic or listener");
		throw ex;
	}

	DDS::DataReader *dr = subscriber_m->create_datareader_with_profile(topic,
			ddsCfg_m.libraryQos.c_str(), ddsCfg_m.profileQos.c_str(),
			listener,
			DDS::STATUS_MASK_ALL);
	if(dr==NULL)
	{
		DDSDRCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		throw ex;
	}//if
/*
	if (noOfReaders==1)
			{
				//multicast
				struct DDS_TransportMulticastSettings_t* multicast_locator = NULL;
				// DDS_TransportMulticastSettingsSeq_ensure_length(&, 1, 1);
				dr_qos.multicast.value.ensure_length(1,1);
				// DDS_TransportMulticastSettingsSeq_get_reference(&qos.multicast.value,0);
				multicast_locator = &dr_qos.multicast.value[0];

				string mcasta="225.3.2.1";
				DDS_String_replace(&multicast_locator->receive_address,	mcasta.c_str()	);
				cout << "going to listen on multicast address: " << mcasta << endl;


				dr_qos.resource_limits.initial_samples *= 1;//multicast_reader_count;
				if (dr_qos.resource_limits.initial_samples >
					dr_qos.resource_limits.max_samples) {
					dr_qos.resource_limits.max_samples =
							dr_qos.resource_limits.initial_samples;
				}
				dr_qos.reader_resource_limits.max_samples_per_remote_writer =
						dr_qos.resource_limits.initial_samples;
			}
			else
			{
				dr_qos.unicast.value.ensure_length(1,1);
			}

*/
	ret = dr->enable();
	if (ret!=DDS::RETCODE_OK)
	{
		DDSDREnableProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
	}

	return ACSBulkData::BulkDataNTFrameDataReader::narrow(dr);
}


void BulkDataNTDDSSubscriber::destroyDDSReader(ACSBulkData::BulkDataNTFrameDataReader *dr)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;

	ret = subscriber_m->delete_datareader(dr);
	if (ret!=DDS::RETCODE_OK)
	{
		ACS_SHORT_LOG((LM_ERROR, "Problem deleting data reader (%d)", ret));
	}
}
