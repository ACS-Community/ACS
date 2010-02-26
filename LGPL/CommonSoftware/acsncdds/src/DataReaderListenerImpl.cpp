#include <DataReaderListener.h>
#include <dds/DCPS/Service_Participant.h>
#include <ace/streams.h>

using namespace ddsnc;

// Implementation skeleton constructor
DataReaderListenerImpl::DataReaderListenerImpl()
  : num_reads_(0)
{
}

// Implementation skeleton destructor
DataReaderListenerImpl::~DataReaderListenerImpl ()
{
}

void DataReaderListenerImpl::on_requested_deadline_missed (
    DDS::DataReader_ptr,
    const DDS::RequestedDeadlineMissedStatus &)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_requested_deadline_missed",
                              ""));
}

void DataReaderListenerImpl::on_requested_incompatible_qos (
    DDS::DataReader_ptr,
    const DDS::RequestedIncompatibleQosStatus &)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_requested_incompatible_qos",
                              ""));
}

void DataReaderListenerImpl::on_liveliness_changed (
    DDS::DataReader_ptr,
    const DDS::LivelinessChangedStatus &)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_liveliness_changed",
                              ""));
}

void DataReaderListenerImpl::on_subscription_match (
    DDS::DataReader_ptr,
    const DDS::SubscriptionMatchStatus &)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_subscription_match",
                              ""));
}

void DataReaderListenerImpl::on_sample_rejected(
    DDS::DataReader_ptr,
    const DDS::SampleRejectedStatus&)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_sample_rejected",
                              ""));
}

void DataReaderListenerImpl::on_sample_lost(
  DDS::DataReader_ptr,
  const DDS::SampleLostStatus&)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_sample_lost",
                              ""));
}

void DataReaderListenerImpl::on_data_available(
  DDS::DataReader_ptr reader)
  throw (CORBA::SystemException)
{
	ACS_STATIC_SHORT_LOG((LM_INFO,
        	              "DataReaderListenerImpl::on_data_available",
                              ""));
}
