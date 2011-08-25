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
    const DDS::RequestedDeadlineMissedStatus& status);
    

  virtual void on_requested_incompatible_qos (
    DDS::DataReader* reader,
    const DDS::RequestedIncompatibleQosStatus& status);
  

  virtual void on_liveliness_changed (
    DDS::DataReader* reader,
    const DDS::LivelinessChangedStatus& status);
  

  virtual void on_subscription_matched (
    DDS::DataReader* reader,
    const DDS::SubscriptionMatchedStatus& status
  );
  

  virtual void on_sample_rejected(
    DDS::DataReader* reader,
    const DDS::SampleRejectedStatus& status
  );
  

  virtual void on_data_available(
    DDS::DataReader* reader
  );
  

  virtual void on_sample_lost(
    DDS::DataReader* reader,
    const DDS::SampleLostStatus& status
  );
  
  unsigned long get_lost_packs() const {
      return lost_packs;
    }

  static int sleep_period;
private:
  typedef enum {StartState, DataRcvState, StopState} ReaderListenerStates;

  ReaderListenerStates currentState_m; //current state of ReaderListener
  DDS::DataReader* reader_;
  unsigned long lost_packs;
  std::string flowName_m;

  unsigned long next_sample;

  // for performance test
  ACE_Time_Value start_time;
  unsigned long data_length;

  ACSBulkData::BulkDataNTFrameDataReader *message_dr;

   // pointer to
  BulkDataCallback* callback_m;
};

#endif
