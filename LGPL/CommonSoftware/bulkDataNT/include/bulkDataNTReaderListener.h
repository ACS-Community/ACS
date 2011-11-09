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
* "@(#) $Id: bulkDataNTReaderListener.h,v 1.16 2011/11/09 12:01:36 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/


#include "bulkDataNTSupport.h"
#include <ndds/ndds_namespace_cpp.h>

#include "bulkDataNTCallback.h"
#include <loggingLoggable.h>
#include <string>
#include <ACE.h>

using namespace std;

class BulkDataNTReaderListener
  : public virtual DDS::DataReaderListener,
    public Logging::Loggable
{
public:
  //Constructor
  BulkDataNTReaderListener (const char*name, AcsBulkdata::BulkDataNTCallback* cb);

  //destructor
  virtual ~BulkDataNTReaderListener (void);

  virtual void on_requested_deadline_missed (DDS::DataReader* reader, const DDS::RequestedDeadlineMissedStatus& status);
    
  virtual void on_requested_incompatible_qos(DDS::DataReader* reader, const DDS::RequestedIncompatibleQosStatus& status);
  
  virtual void on_liveliness_changed(DDS::DataReader* reader, const DDS::LivelinessChangedStatus& status);
  
  virtual void on_subscription_matched(DDS::DataReader* reader, const DDS::SubscriptionMatchedStatus& status);
  
  virtual void on_sample_rejected(DDS::DataReader* reader,  const DDS::SampleRejectedStatus& status);
  
  virtual void on_data_available(DDS::DataReader* reader);
  
  virtual void on_sample_lost(DDS::DataReader* reader, const DDS::SampleLostStatus& status);
  
private:
  typedef enum {StartState, DataRcvState, StopState} ReaderListenerStates;
  ReaderListenerStates currentState_m; /// current state of ReaderListener

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

  /// we override getLogger, so that we can initialize logging system if needed
  virtual Logging::Logger::LoggerSmartPtr getLogger ();
  LoggingProxy *logger_mp; //we need separate logger, because we are in separate thread  ...
  // ... other is one logger for all DDS reader thread
  unsigned int loggerInitCount_m; // we need to count how many time we call LoggerProxy::init

   // pointer to user defined callback
  AcsBulkdata::BulkDataNTCallback* callback_mp;
};

#endif
