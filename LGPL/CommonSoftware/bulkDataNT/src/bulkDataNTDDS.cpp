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
* "@(#) $Id: bulkDataNTDDS.cpp,v 1.10 2011/07/28 15:11:54 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTDDS.h"
#include <iostream>


using namespace AcsBulkdata;
using namespace std;
using namespace ACSErrTypeCommon;
using namespace ACS_DDS_Errors;


BulkDataNTDDS::BulkDataNTDDS(DDS::DomainParticipant* participant) :
	participant_m(participant)
{
	AUTO_TRACE("BulkDataNTDDS::BulkDataNTDDS");
	if (participant==0)
	{
		NullPointerExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setVariable("participant");
		throw ex;
	}//if
}//BulkDataNTDDS

BulkDataNTDDS::~BulkDataNTDDS()
{
	participant_m = 0;
}//~BulkDataNTDDS

DDS::Topic* BulkDataNTDDS::createDDSTopic(const char* topicName)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;

	//TBD: check if topic already exists find_topic ??
		DDS::TopicQos topic_qos;
		ret = participant_m->get_default_topic_qos(topic_qos);
		if (ret!=DDS::RETCODE_OK)
		{
			DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ex.setDDSTypeCode(ret);
			ex.setQoS("get_default_topic_qos");
			throw ex;
		}//if
//		topic_qos.ownership.kind = DDS_EXCLUSIVE_OWNERSHIP_QOS;
		topic_qos.reliability.kind = ::DDS::RELIABLE_RELIABILITY_QOS; //::DDS::BEST_EFFORT_RELIABILITY_QOS;
//		topic_qos.resource_limits.max_samples_per_instance = 2;
		//TBD: type name could be a parameter of the method or class member

		/* Register the type before creating the topic */
		const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();
		ret = ACSBulkData::BulkDataNTFrameTypeSupport::register_type(participant_m, type_name);

		if (ret != DDS::RETCODE_OK)
		{
			DDSRegisterTypeProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ex.setDDSTypeCode(ret);
			ex.setTypeName(type_name);
			throw ex;
		}

		DDS::Topic *topic =  participant_m->create_topic(topicName,
				type_name,
				topic_qos,
				NULL,
				DDS::STATUS_MASK_NONE
		);
		if (topic==0)
		{
			DDSTopicCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ex.setTopic(topicName);
			throw ex;
		}//if

		ACS_SHORT_LOG((LM_DEBUG, "Created DDS topic: %s", topicName));
		return topic;
}//createDDSTopic



/*___oOo___*/

