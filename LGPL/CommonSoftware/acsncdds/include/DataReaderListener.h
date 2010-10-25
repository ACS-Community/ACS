#ifndef DATAREADER_LISTENER_IMPL
#define DATAREADER_LISTENER_IMPL

#include <dds/DdsDcpsSubscriptionS.h>
#include <loggingACEMACROS.h>

#if !defined (ACE_LACKS_PRAGMA_ONCE)
#pragma once
#endif

namespace ddsnc{

//Class DataReaderListenerImpl
/**
 * Base class for DataReaderListener functions callbacks. Only a empty
 * skeleton that print to stdout when a function is called.
 *
 * @see ACSNCDDSDataReaderListener
 *
 * @author Jorge Avarias <javarias[at]inf.utfsm.cl>
 */
class DataReaderListenerImpl
  : public virtual OpenDDS::DCPS::LocalObject<DDS::DataReaderListener>
{
public:
  //Constructor
  DataReaderListenerImpl ();

  //Destructor
  ~DataReaderListenerImpl (void);

  void on_requested_deadline_missed (
    DDS::DataReader_ptr reader,
    const DDS::RequestedDeadlineMissedStatus & status)
    throw (CORBA::SystemException);

  void on_requested_incompatible_qos (
    DDS::DataReader_ptr reader,
    const DDS::RequestedIncompatibleQosStatus & status)
  throw (CORBA::SystemException);

  void on_liveliness_changed (
    DDS::DataReader_ptr reader,
    const DDS::LivelinessChangedStatus & status)
  throw (CORBA::SystemException);

  void on_subscription_matched (
    DDS::DataReader_ptr reader,
    const DDS::SubscriptionMatchedStatus & status
  )
  throw (CORBA::SystemException);

  void on_sample_rejected(
    DDS::DataReader_ptr reader,
    const DDS::SampleRejectedStatus& status
  )
  throw (CORBA::SystemException);

  virtual void on_data_available(
    DDS::DataReader_ptr reader
  )
  throw (CORBA::SystemException);

  void on_sample_lost(
    DDS::DataReader_ptr reader,
    const DDS::SampleLostStatus& status
  )
  throw (CORBA::SystemException);

  long num_reads() const {
    return num_reads_;
  }

protected:
  DDS::DataReader_var reader_;
  long                  num_reads_;
};
}
#endif
