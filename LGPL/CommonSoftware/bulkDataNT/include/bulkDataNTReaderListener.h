#ifndef BULKDATA_NT_READER_LISTENER
#define BULKDATA_NT_READER_LISTENER

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
* "@(#) $Id: bulkDataNTReaderListener.h,v 1.25 2012/09/06 10:50:30 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/


#include "bulkDataNTSupport.h"
#include <ndds/ndds_namespace_cpp.h>

#include "bulkDataNTCallback.h"
#include "bulkDataNTDDSLoggable.h"

#include <string>
#include <ACE.h>

namespace AcsBulkdata
{

class BulkDataNTReaderListener
  : public virtual DDS::DataReaderListener,
    public BulkDataNTDDSLoggable
{
public:
  //Constructor
  BulkDataNTReaderListener (const char*name, BulkDataNTCallback* cb);

  //destructor
  virtual ~BulkDataNTReaderListener (void);

  /// Enables calling user's CB (cbStart, cbReceiver, cbStop)
  void enableCallingCB();

  /// Disables calling user's CB (cbStart, cbReceiver, cbStop)
  void disableCallingCB();

  // implementation of methods from #DDS::DataReaderListener

  virtual void on_requested_deadline_missed (DDS::DataReader* reader, const DDS::RequestedDeadlineMissedStatus& status);
    
  virtual void on_requested_incompatible_qos(DDS::DataReader* reader, const DDS::RequestedIncompatibleQosStatus& status);
  
  virtual void on_liveliness_changed(DDS::DataReader* reader, const DDS::LivelinessChangedStatus& status);
  
  virtual void on_subscription_matched(DDS::DataReader* reader, const DDS::SubscriptionMatchedStatus& status);
  
  virtual void on_sample_rejected(DDS::DataReader* reader,  const DDS::SampleRejectedStatus& status);
  
  virtual void on_data_available(DDS::DataReader* reader);
  
  virtual void on_sample_lost(DDS::DataReader* reader, const DDS::SampleLostStatus& status);
  
private:
  /** ReaderListener's state:
   * IgnoreDataState - we go in this state when we get WrongDataError for Data
   * ... and we stay there until next start/stop. In this way we prevent overfloating with error msg.
   */
  typedef enum {StartState, DataRcvState, StopState, IgnoreDataState } ReaderListenerStates;
  ReaderListenerStates currentState_m; /// current state of ReaderListener
  static const char* state2String[]; /// strings name of states

  std::string topicName_m;  /// name of DDS topic

  unsigned int conseqErrorCount_m;  //consequence error count
  unsigned int maxConseqErrorCount_m;  //maximal consequence error count
  void increasConseqErrorCount(); // increases conseqErroCount_m and check if it is more than max and reacts

  ACE_Time_Value start_time;  /// for performance test

  unsigned long dataLength_m; /// length of data arrived so far after startSend/paramter
  unsigned long frameCounter_m; /// number of *data* frame arrived so far for sendData
  unsigned long totalFrames_m; /// total number of frames that we should get for current sendData
  unsigned long nextFrame_m;   /// the count of next frame that should be received

  ACSBulkData::BulkDataNTFrameDataReader *frameDataReader_mp; /// pointer to DDS reader


  BulkDataNTCallback* callback_mp; /// pointer to user defined callback
  bool enableCB_m;  /// should be called user's callback or not ?

  ACE_Time_Value cbReceiveStartTime_m;  /// time just be4 cbReceive is going to be executed
  ACE_Time_Value cbReceiveElapsedTime_m;  /// elapsed time of cbReceive
  double cbReceiveElapsedTimeSec_m; /// elapsed time in sec
  double cbReceiveTimeoutSec_m; /// cbReceiver process timeout read from CDB (or default value)

  // next members need we for calculating average cbReceive process time
  double cbReceiveTotalSec_m;  /// total time all cbReceive calls between cbStart and cbStop
  unsigned int cbReceiveNumCalls_m; /// how many times cbReceive was called  between cbStart and cbStop
  double cbReceiveAvgSec_m; /// average process time for all cbReceive calls between cbStart and cbStop
  double cbReceiveAvgTimeoutSec_m; /// cbReceiver avergae process timeout read from CDB (or default value)

  // we assumed that on_data_available is called just once, so we can have these members in class not in the method.
  DDS::ReturnCode_t retCode;
  DDS::SampleInfo si ;
  ACSBulkData::BulkDataNTFrame message;
  unsigned char tmpArray[ACSBulkData::FRAME_MAX_LEN];
};

};//namespace AcsBulkdata
#endif
