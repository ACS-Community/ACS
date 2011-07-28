#include "bulkDataNTDDSSubscriber.h"
#include <iostream>


using namespace AcsBulkdata;
using namespace std;
using namespace ACSErrTypeCommon;
using namespace ACS_BDError;


BulkDataNTDDSSubscriber::BulkDataNTDDSSubscriber(DDS::DomainParticipant *p) :
		BulkDataNTDDS(p)
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
	if (participant_m==NULL)
	{
		printf("BulkDataNTDDSSubscriber::BulkDataNTDDSSubscriber participant NULL\n");
		return NULL;
	}
	//Setup Publisher QoS, add the partition QoS policy
	DDS::SubscriberQos sub_qos;
	ret = participant_m->get_default_subscriber_qos(sub_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("get_default_subscriber_qos");
		throw ex;
	}//if

	DDS::Subscriber *sub = participant_m->create_subscriber(sub_qos, 0, DDS::STATUS_MASK_NONE);
	if(sub==NULL)
	{
		DDSSubscriberCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		throw ex;
	}
	return sub;
}//createDDSSubscriber

void BulkDataNTDDSSubscriber::destroyDDSSubscriber()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	DDS::ReturnCode_t ret;
	ret = participant_m->delete_subscriber(subscriber_m);
	subscriber_m = 0;
	if (ret == 0)
	{
		DDSPublisherDestroyProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
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

	//Apply Qos Policies, in this case the partition
	DDS::DataReaderQos dr_qos;
	ret = subscriber_m ->get_default_datareader_qos (dr_qos);
	if (ret!=DDS::RETCODE_OK)
	{
		DDSQoSSetProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setDDSTypeCode(ret);
		ex.setQoS("get_default_datareader_qos");
		throw ex;
	}//if

	dr_qos.reliability.kind = ::DDS::RELIABLE_RELIABILITY_QOS;
	/*
			dr_qos.resource_limits.max_samples_per_instance = 1;//queue size
			dr_qos.resource_limits.max_samples = 10;
			dr_qos.resource_limits.max_instances = 50;
	 */
	dr_qos.history.kind  = ::DDS::KEEP_ALL_HISTORY_QOS;
	dr_qos.history.depth  = 100;

	/*
			dr_qos.liveliness.lease_duration.sec=5;
			dr_qos.liveliness.lease_duration.nanosec=0;
	 */

	/*
			dr_qos.deadline.period.sec=1;
			dr_qos.deadline.period.nanosec=0;
	 */

	//we can have just one writer & we have no keyed topic
	// RTI specific
	dr_qos.reader_resource_limits.initial_remote_writers=1;
	dr_qos.reader_resource_limits.max_remote_writers=1;
	dr_qos.reader_resource_limits.initial_remote_writers_per_instance=1;
	dr_qos.reader_resource_limits.max_remote_writers_per_instance=1;

	const int UNRESOLVED_SAMPLE_PER_REMOTE_WRITER_MAX = 50; //2
	dr_qos.resource_limits.max_samples = 2  * UNRESOLVED_SAMPLE_PER_REMOTE_WRITER_MAX; //should be 2x writer
	dr_qos.resource_limits.initial_samples = dr_qos.resource_limits.max_samples;

	dr_qos.reader_resource_limits.max_samples_per_remote_writer = 200;//dr_qos.resource_limits.initial_samples;
	dr_qos.resource_limits.max_samples_per_instance = 100; //dr_qos.resource_limits.initial_samples;

	//		dr_qos.rtps_reader.heartbeat_response_delay.sec = 0;
	//		dr_qos.rtps_reader.heartbeat_response_delay.nanosec = 0;
	// RTI the writer probably has more for the reader; ask right away
	dr_qos.protocol.rtps_reliable_reader.min_heartbeat_response_delay.sec = 0;
	dr_qos.protocol.rtps_reliable_reader.min_heartbeat_response_delay.nanosec = 0;
	dr_qos.protocol.rtps_reliable_reader.max_heartbeat_response_delay.sec = 0;
	dr_qos.protocol.rtps_reliable_reader.max_heartbeat_response_delay.nanosec = 100000;

	//multicast
	struct DDS_TransportMulticastSettings_t* multicast_locator = NULL;
	// DDS_TransportMulticastSettingsSeq_ensure_length(&, 1, 1);
	dr_qos.multicast.value.ensure_length(1,1);
	// DDS_TransportMulticastSettingsSeq_get_reference(&qos.multicast.value, 0);
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
	//unicast				dr_qos.unicast.value.ensure_length(1,1);

	// deadline
	/*		dr_qos.deadline.period.sec = 1;
			dr_qos.deadline.period.nanosec = 0;
	 */

	// READERs
	//			if (noOfReaders>1) dr_qos.unicast.value[0].receive_port = 24000+readerIndex; // we get a thread per reader !
	DDS::DataReader *dr = subscriber_m->create_datareader(topic,
			dr_qos,
			listener,
			DDS::STATUS_MASK_ALL/*ALL_STATUS*/);
	if(dr==NULL)
	{
		DDSDRCreateProblemExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		throw ex;
	}//if

	return dr;
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
