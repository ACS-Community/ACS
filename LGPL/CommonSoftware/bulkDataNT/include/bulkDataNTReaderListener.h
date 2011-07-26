#ifndef BULKDATA_NT_READER_LISTENER
#define BULKDATA_NT_READER_LISTENER


//CoreDX
//#include "bulkDataNTDataReader.hh"
//#include <dds/dds.hh>

//#include <ndds/ndds_cpp.h>
#include "bulkDataNTSupport.h"
#include <ndds/ndds_namespace_cpp.h>


#include "bulkDataNTCallback.h"
#include <string>
#include <ACE.h>

using namespace std;

class BulkDataNTReaderListener
  : public virtual DDS::DataReaderListener
{
public:
  //Constructor
	  BulkDataNTReaderListener (const char*name, BulkDataCallback* cb);

  //Destructor
  virtual ~BulkDataNTReaderListener (void);

  virtual void on_requested_deadline_missed (
    DDS::DataReader* reader,
    DDS::RequestedDeadlineMissedStatus status);
    

  virtual void on_requested_incompatible_qos (
    DDS::DataReader* reader,
    DDS::RequestedIncompatibleQosStatus status);
  

  virtual void on_liveliness_changed (
    DDS::DataReader* reader,
    DDS::LivelinessChangedStatus status);
  

  virtual void on_subscription_matched (
    DDS::DataReader* reader,
    DDS::SubscriptionMatchedStatus status
  );
  

  virtual void on_sample_rejected(
    DDS::DataReader* reader,
    DDS::SampleRejectedStatus status
  );
  

  virtual void on_data_available(
    DDS::DataReader* reader
  );
  

  virtual void on_sample_lost(
    DDS::DataReader* reader,
    DDS::SampleLostStatus status
  );
  

  long num_reads() const {
    return num_reads_;
  }

  unsigned long get_lost_packs() const {
      return lost_packs;
    }

  static int sleep_period;
private:

  DDS::DataReader* reader_;
  long                  num_reads_;
  unsigned long lost_packs;
  std::string listName;

  unsigned long next_sample;
  unsigned int itera;

  // for performance tesat
  ACE_Time_Value start_time;
  unsigned long data_length;

  ACSBulkData::BulkDataNTFrameDataReader *message_dr;

   // pointer to
  BulkDataCallback* callback_m;
};

#endif
