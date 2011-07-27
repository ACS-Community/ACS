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
* "@(#) $Id: bulkDataNTDDS.cpp,v 1.5 2011/07/27 07:12:10 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#include "bulkDataNTDDS.h"
#include <iostream>

using namespace AcsBulkdata;
using namespace std;


BulkDataNTDDS::BulkDataNTDDS(DDS::DomainParticipant* p) :
	participant_m(p)
{

}

BulkDataNTDDS::~BulkDataNTDDS()
{

	//TBD: do we have to delete something here ?
}






DDS::Topic* BulkDataNTDDS::createDDSTopic(const char* topicName)
{
	DDS::ReturnCode_t ret;
	if (participant_m==0)
	{
		std::cerr << "BulkDataNTDDS::createDDSTopic participant is 0" << std::cerr << endl;
		return 0;
	}
//TOPIC
	//TBD: check if topic already exists find_topic ??
		DDS::TopicQos topic_qos;
		ret = participant_m->get_default_topic_qos(topic_qos);
//		topic_qos.ownership.kind = DDS_EXCLUSIVE_OWNERSHIP_QOS;
		topic_qos.reliability.kind = ::DDS::RELIABLE_RELIABILITY_QOS; //::DDS::BEST_EFFORT_RELIABILITY_QOS;
//		topic_qos.durability.kind = DDS::VOLATILE_DURABILITY_QOS;
//		topic_qos.resource_limits.max_samples_per_instance = 2;
		//TBD: type name could be a parameter of the method or class member

		//REGISTER TYPE
		/* Register the type before creating the topic */
//	ACSBulkData::BulkDataNTFrameTypeSupport* ts =new ACSBulkData::BulkDataNTFrameTypeSupport();
//		const char* type_name = ts->get_type_name();

		const char* type_name = ACSBulkData::BulkDataNTFrameTypeSupport::get_type_name();
		int retcode = ACSBulkData::BulkDataNTFrameTypeSupport::register_type(participant_m, type_name);

		if (retcode != DDS::RETCODE_OK)
				{
			printf("register_type error %d\n", retcode);
				}

		cout << "Going to create DDS topic: " << topicName << " " << type_name << endl;
		DDS::Topic *topic =  participant_m->create_topic(topicName,
				type_name,
				topic_qos,
				NULL,
				0/*DDS::STATUS_MASK_NONE*/
		);
		if (topic==NULL){
			std::cerr << "create_topic failed" << std::endl;
		//TBD: error handling
		}
		return topic;
}//createDDSTopic


void BulkDataNTDDS::operator=(const BulkDataNTDDS&){}

BulkDataNTDDS::BulkDataNTDDS(const BulkDataNTDDS&){}

BulkDataNTDDS::BulkDataNTDDS() : participant_m(0){}



/*___oOo___*/

