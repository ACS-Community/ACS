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
* "@(#) $Id: bulkDataNTDDS.cpp,v 1.19 2012/09/13 14:02:38 bjeram Exp $"
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

const char* AcsBulkdata::dataType2String[]={"BD_PARAM", "BD_DATA", "BD_STOP" };

BulkDataNTDDS::BulkDataNTDDS(DDS::DomainParticipant* participant, const DDSConfiguration &ddsCfg) :
	participant_m(participant),
	ddsCfg_m(ddsCfg)
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
	DDS::Topic *topic;
	DDS::Duration_t to;

	topicName_m = topicName;
	//TBD: type name could be a parameter of the method or class member

	to.sec = 0;
	to.nanosec = 5000000; //5ms
	topic = participant_m->find_topic(topicName, to);

	if (topic==0) // we could not find a topic so we have to create it
	{
		/* Register the type before creating the topic */
		const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();

		topic =  participant_m->create_topic_with_profile(topicName_m.c_str(),
				type_name,
				ddsCfg_m.libraryQos.c_str(), ddsCfg_m.profileQos.c_str(),
				NULL,
				DDS::STATUS_MASK_NONE
		);
		if (topic==0)
		{
			DDSTopicCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
			ex.setTopic(topicName_m.c_str());
			throw ex;
		}//if
	}else
	{
		ACS_SHORT_LOG((LM_DEBUG, "Already creaded DDS topic: %s will be taken", topicName_m.c_str()));
	}

	ret = topic->enable();
	if ( ret != DDS::RETCODE_OK )
	{
		DDSTopicEnableProblemExImpl ex(__FILE__, __LINE__, __FUNCTION__);
		ex.setDDSTypeCode(ret);
		throw ex;
	}

	ACS_SHORT_LOG((LM_DEBUG, "Created DDS topic: %s", topicName_m.c_str()));
	return topic;
}//createDDSTopic

 void BulkDataNTDDS::destroyDDSTopic(DDS::Topic *topic)
 {
	 AUTO_TRACE(__PRETTY_FUNCTION__);
	 DDS::ReturnCode_t ret;

	ret = participant_m->delete_topic(topic);
	if (ret != DDS::RETCODE_OK)
	{
		DDSTopicDeleteProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setTopic(topicName_m.c_str());
		ex.setDDSTypeCode(ret);
		throw ex;
	}//if

}//destroyDDSTopic

/*___oOo___*/

