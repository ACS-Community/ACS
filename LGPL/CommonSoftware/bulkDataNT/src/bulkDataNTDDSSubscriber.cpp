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
* "@(#) $Id: bulkDataNTDDSSubscriber.cpp,v 1.20 2012/01/20 11:56:11 bjeram Exp $"
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


BulkDataNTDDSSubscriber::BulkDataNTDDSSubscriber(DDS::DomainParticipant *p, const ReceiverFlowConfiguration &cfg) :
		BulkDataNTDDS(p, cfg)
{
	subscriber_m = createDDSSubscriber();
	enalbeMulticast_m = cfg.isEnableMulticast();
	mutlicastAddress_m = cfg.getMulticastAddress();
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
	DDS::DataReaderQos dr_qos;

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

	ret =dr->get_qos(dr_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSGetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("dr->get_qos()");
		throw ex;
	}//if

	if (enalbeMulticast_m)
	{
		dr_qos.multicast.value.ensure_length(1,1); // ? should we read the length and increase for one ?
		dr_qos.multicast.value[0].receive_address = DDS_String_dup(mutlicastAddress_m.c_str());

		if (mutlicastAddress_m.compare(ReceiverFlowConfiguration::DEFAULT_MULTICAST_ADDRESS))
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Stream#Flow: %s going to listen on multicast address: %s which is different from default: %s. Please make sure that the same multicast address is used for all receivers!",
					topicName_m.c_str(), mutlicastAddress_m.c_str(), ReceiverFlowConfiguration::DEFAULT_MULTICAST_ADDRESS));
		}
		else
		{
			ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Stream#Flow: %s going to listen on multicast address: %s.", topicName_m.c_str(), mutlicastAddress_m.c_str()));
		}
	}
	else
	{
		//TBD if we add support for setting unicast addreess
		//dr_qos.unicast.value.ensure_length(1,1);
	}

	ret =dr->set_qos(dr_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("dr->set_qos()");
		throw ex;
	}//if

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
