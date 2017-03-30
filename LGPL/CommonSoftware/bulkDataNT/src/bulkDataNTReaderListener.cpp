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
 * "@(#) $Id: bulkDataNTReaderListener.cpp,v 1.60 2013/02/04 14:24:02 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * bjeram  2011-04-19  created
 */

#include "bulkDataNTReaderListener.h"
#include "bulkDataNTConfiguration.h"
#include "ACS_BD_Errors.h"
#include <ACS_DDS_Errors.h>
#include <ACSErrTypeCommon.h>
#include <RepeatGuard.h>
#include <iostream>
#include <iterator>

using namespace ACS_BD_Errors;
using namespace ACS_DDS_Errors;
using namespace AcsBulkdata;
using namespace std;


const char *BulkDataNTReaderListener::state2String[] = {"StartState", "DataRcvState", "StopState", "IgnoreDataState" };

BulkDataNTReaderListener::BulkDataNTReaderListener(const char* name, BulkDataNTCallback* cb)
: BulkDataNTDDSLoggable("BulkDataNT:"+string(name)),
  currentState_m(StartState),
  topicName_m(name),
  dataLength_m(0),
  frameCounter_m(0),
  totalFrames_m(0),
  callback_mp (cb), enableCB_m(true)
{
  ACS_TRACE(__FUNCTION__);
  nextFrame_m=0;
  frameDataReader_mp=0;
  conseqErrorCount_m=0;
  maxConseqErrorCount_m = 4; //TBD now hard-coded, to be get from somewhere else
  cbReceiveTimeoutSec_m = callback_mp->getCBReceiveProcessTimeout();
  cbReceiveAvgTimeoutSec_m = callback_mp->getCBReceiveAvgProcessTimeout();
  cbReceiveTotalSec_m = 0.0;  cbReceiveNumCalls_m =0;
}//BulkDataNTReaderListener


BulkDataNTReaderListener::~BulkDataNTReaderListener ()
{
  ACS_TRACE(__FUNCTION__);
/*  DDS::DataReaderProtocolStatus drps;
  frameDataReader_mp->get_datareader_protocol_status(drps);
  DDS::DataReaderCacheStatus drcs;
  frameDataReader_mp->get_datareader_cache_status(drcs);

  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
		  (LM_DEBUG, "Protocol Status (TOTAL): sample rcv: %lld (%lld) HB: %lld (%lld) ACKs: %lld (%lld) NACKs: %lld (%lld) Rejected: %lld",
				  drps.received_sample_count, drps.received_sample_bytes,
				  drps.received_heartbeat_count, drps.received_heartbeat_bytes,
				  drps.sent_ack_count, drps.sent_ack_bytes,
				  drps.sent_nack_count, drps.sent_nack_bytes,
				  drps.rejected_sample_count));
*/
}//~BulkDataNTReaderListener

void BulkDataNTReaderListener::on_data_available(DDS::DataReader* reader)
{
  initalizeLogging(); //force initialization of logging sys TBD changed
  if (DDSConfiguration::debugLevel>3)
  {
	  // the message can cause performance penalty for small data sizes
	  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Data arrived (on_data_available) for: %s", topicName_m.c_str()));
  }//if

  frameDataReader_mp = ACSBulkData::BulkDataNTFrameDataReader::narrow(reader);
  if (frameDataReader_mp==NULL)
  {
	  ACS_DDS_Errors::DDSNarrowFailedCompletion nerr(__FILE__, __LINE__, __FUNCTION__);
	  nerr.setVariable("frameDataReader_mp");
	  nerr.setNarrowType("ACSBulkData::BulkDataNTFrameDataReader");
	  callback_mp->onError(nerr);
	  return;
  }//if


  message.data.maximum(ACSBulkData::FRAME_MAX_LEN);
  while (	(retCode = frameDataReader_mp->take_next_sample(message, si)) != DDS::RETCODE_NO_DATA )
    {
      if (retCode == DDS::RETCODE_OK)
        {
          if (si.valid_data == true)
            {
        	  if (DDSConfiguration::debugLevel>3)
        	    {
        	  	  // the message can cause performance penalty for small data sizes
        		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Got BulkDataNTFrame (on_data_available): typeOfdata: %d, restDataLength: %d, size %d for: %s",
        	  			  message.typeOfdata,
        	  			  message.restDataLength,
        	  			  message.data.length(),
        	  			  topicName_m.c_str()));
        	    }//if
              switch(message.typeOfdata)
              {
              case ACSBulkData::BD_PARAM:
              {
            	  if (DDSConfiguration::debugLevel>0)
            	  {
            		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "startSend has been received on: %s with paramter size: %d", topicName_m.c_str(), message.data.length()));
            	  }
            	  if (currentState_m==StartState || currentState_m==StopState || currentState_m==IgnoreDataState)
            	  {
            		  dataLength_m = 0;
            		  frameCounter_m = 0;
            		  currentState_m = DataRcvState;
            		  cbReceiveTotalSec_m = 0.0; cbReceiveNumCalls_m =0;
            		  message.data.to_array(tmpArray, message.data.length());
            		  if (enableCB_m) { BDNT_LISTENER_USER_ERR( callback_mp->cbStart(tmpArray, message.data.length()) ) }
            		  conseqErrorCount_m=0;
            	  }
            	  else //error
            	  {
            		  WrongFrameOrderCompletion wfo(__FILE__, __LINE__, __FUNCTION__);
            		  wfo.setDataType("BD_PARAM"); wfo.setState(state2String[currentState_m]);
            		  wfo.setStreamFlowName(topicName_m.c_str()); wfo.setFrameCount(frameCounter_m);
            		  wfo.setTotalFrameCount(totalFrames_m); wfo.setFrameLength(message.data.length());
            		  callback_mp->onError(wfo);
            		  increasConseqErrorCount();
            	  }//if-else
            	  break;
              }//	case ACSBulkData::BD_PARAM:
              case ACSBulkData::BD_DATA:
                {
                  if (currentState_m==IgnoreDataState) break; // in ignore data state just skip the freame
                  if (currentState_m==DataRcvState )
                    {
                      if (dataLength_m==0) // we get the first data frame
                        {
                    	  if (DDSConfiguration::debugLevel>1)
                    	  {
                    		  // the message can cause perfomance penality for small data sizes
                    		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "New sendData has arrived for: %s", topicName_m.c_str()));
                    		  start_time = ACE_OS::gettimeofday();
                    	  }//if
                          totalFrames_m = message.restDataLength+1;
                          frameCounter_m = 0;
                        }//if

                      dataLength_m += message.data.length();
                      frameCounter_m ++;

                      if ( message.restDataLength>0)
                        {
                          if (nextFrame_m!=0 && nextFrame_m!=message.restDataLength) // do we miss a frame ?
                            {
                              FrameLostCompletion lde(__FILE__, __LINE__, __FUNCTION__);
                              lde.setNextDataFrame(nextFrame_m);
                              lde.setFrameCount(frameCounter_m);
                              lde.setRestFrames(message.restDataLength);
                              lde.setFrameLength(message.data.length());
                              lde.setStreamFlowName(topicName_m.c_str());
                              BDNT_LISTENER_USER_ERR( callback_mp->onDataLost(frameCounter_m, totalFrames_m, lde))
                              increasConseqErrorCount();
                              return; // ??
                            }
                          nextFrame_m = message.restDataLength-1;
                        }
                      else //message.restDataLength==0 what means we got the last frame
                        {
                    	  if (DDSConfiguration::debugLevel>1)
                    	  {
                    		  // the messages can cause performance penalty for small data sizes
                    		  ACE_Time_Value elapsed_time = ACE_OS::gettimeofday() - start_time;
                    		  double deltaTime = (elapsed_time.sec()+( elapsed_time.usec() / 1000000.0 ));
                    		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "All data from sendData has been received: %ld (Bytes) on: %s in %f sec. Receiver Data Rate: %f MBytes/sec",
                    				  dataLength_m, topicName_m.c_str(), deltaTime,
                    				  ((dataLength_m/(1024.0*1024.0))/deltaTime)
                    				  ));

                    		  DDS::SampleLostStatus s;
                    		  reader->get_sample_lost_status(s);
                    		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_DEBUG, "Lost samples at the end of sendData on: %s: total_count: %d total_count_change: %d ",
                    				  topicName_m.c_str(), s.total_count, s.total_count_change
                    				  ))
                    	  }//if (DDSConfiguration::debugLevel>0)
                          dataLength_m = 0;
                        }//else

                      cbReceiveStartTime_m = ACE_OS::gettimeofday();
                      message.data.to_array(tmpArray, message.data.length());
                      if (enableCB_m) { BDNT_LISTENER_USER_ERR( callback_mp->cbReceive(tmpArray, message.data.length()) ) }
                      conseqErrorCount_m=0;
                      cbReceiveElapsedTime_m = ACE_OS::gettimeofday() - cbReceiveStartTime_m;
                      cbReceiveElapsedTimeSec_m = cbReceiveElapsedTime_m.sec() + (cbReceiveElapsedTime_m.usec() / 1000000.0);
                      cbReceiveTotalSec_m += cbReceiveElapsedTimeSec_m; cbReceiveNumCalls_m++;
                      if (cbReceiveElapsedTimeSec_m>cbReceiveTimeoutSec_m)
                      {
                    	  CBReceiveProcessTimeoutCompletion cbReceiveTO(__FILE__, __LINE__, __FUNCTION__);
                    	  cbReceiveTO.setStreamFlowName(topicName_m.c_str());
                    	  cbReceiveTO.setProcessTimeoutSec(cbReceiveTimeoutSec_m);cbReceiveTO.setActualProcessTime(cbReceiveElapsedTimeSec_m);
                    	  cbReceiveTO.setFrameCount(frameCounter_m);cbReceiveTO.setTotalFrameCount(totalFrames_m);
                    	  callback_mp->onError(cbReceiveTO);
                    	  //TBD should we increase error counter here or not ?
                      }//if cbReceiveTimeoutSec_m
                    }
                  else //error
                    {
                      WrongFrameOrderCompletion wfo(__FILE__, __LINE__, __FUNCTION__);
                      wfo.setDataType("BD_DATA"); wfo.setState(state2String[currentState_m]);
                      wfo.setStreamFlowName(topicName_m.c_str()); wfo.setFrameCount(frameCounter_m);
                      wfo.setTotalFrameCount(totalFrames_m); wfo.setFrameLength(message.data.length());
                      wfo.addData("Note", "all BD_DATA frames will be ignored until next BD_START/BD_STOP");
                      callback_mp->onError(wfo);
                      currentState_m = IgnoreDataState; //we go to IgnoreData state, so all data until next start/stop will be ignored
                      //increasConseqErrorCount();
                    }
                  break;
                }//case ACSBulkData::BD_DATA
              case ACSBulkData::BD_STOP:
                {
                  if (currentState_m==DataRcvState )
                    {
                      currentState_m = StopState;
                      if (DDSConfiguration::debugLevel>0)
                      {
                    	  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,(LM_DEBUG, "sendStop has been received for: %s", topicName_m.c_str()));
                      }
                      if (frameCounter_m==0)
                        {
                          ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                              (LM_WARNING, "On %s got stop (BD_STOP) before any data (sendData)!",
                                  topicName_m.c_str()));
                        }else
                          {
                            if (frameCounter_m != totalFrames_m)
                              {
                                ACS_BD_Errors::FrameLostCompletion lde(__FILE__, __LINE__, __FUNCTION__);
                                lde.setNextDataFrame(nextFrame_m);
                                lde.setFrameCount(frameCounter_m);
                                lde.setRestFrames(message.restDataLength); // should be ??
                                lde.setFrameLength(message.data.length()); // should be 0
                                lde.setStreamFlowName(topicName_m.c_str());
                                BDNT_LISTENER_USER_ERR( callback_mp->onDataLost(frameCounter_m, totalFrames_m, lde) )
                                increasConseqErrorCount();
                              }//if
                          }//if-else
                    }else
                      {
                        if (currentState_m==StopState)
                          {
                            ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                                (LM_WARNING, "On %s stop (BD_STOP) arrived in stop state - will be ignored!",
                                    topicName_m.c_str()));
                          }
                        else //StartState || IgnoreDataState
                          {
                            ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                                (LM_WARNING, "On %s stop (BD_STOP) arrived in start state: no parameter data (startSend) has arrived!",
                                    topicName_m.c_str()));
                          }//if-else
                      }
                  // in all above warning/error case we call  user's cbStop()
                  if (enableCB_m) { BDNT_LISTENER_USER_ERR( callback_mp->cbStop() ) }
                  conseqErrorCount_m=0;
                  if (cbReceiveNumCalls_m>0) // if we call cbReceive at least once we calculate the average
                  {
                	  cbReceiveAvgSec_m = cbReceiveTotalSec_m / cbReceiveNumCalls_m;
                	  if (cbReceiveAvgSec_m > cbReceiveAvgTimeoutSec_m)
                	  {
                		  CBReceiveAvgProcessTimeoutCompletion cbReceiveAvgTO(__FILE__, __LINE__, __FUNCTION__);
                		  cbReceiveAvgTO.setStreamFlowName(topicName_m.c_str());
                		  cbReceiveAvgTO.setAvgProcessTimeoutSec(cbReceiveAvgTimeoutSec_m);cbReceiveAvgTO.setActualAvgProcessTime(cbReceiveAvgSec_m);
                		  cbReceiveAvgTO.setCallsCount(cbReceiveNumCalls_m);cbReceiveAvgTO.setThroughput((ACSBulkData::FRAME_MAX_LEN/(1024.0*1024.0)/cbReceiveAvgSec_m));
                		  callback_mp->onError(cbReceiveAvgTO);
                		  //TBD should we increase error counter here or not ?
                	  }//if (cbReceiveAvgSec_m > cbReceiveTimeoutSec_m)
                	  if (DDSConfiguration::debugLevel>1)
                	  {
                		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                				  (LM_DEBUG, "Average processing time for: %s for %d call(s) of cbReceive(): %fs. What corresponds to throughput of: %fMB/sec",
                						  topicName_m.c_str(), cbReceiveNumCalls_m , cbReceiveAvgSec_m, (ACSBulkData::FRAME_MAX_LEN/(1024.0*1024.0)/cbReceiveAvgSec_m)));

                		  DDS::DataReaderProtocolStatus drps;
                		  frameDataReader_mp->get_datareader_protocol_status(drps);
                		  DDS::DataReaderCacheStatus drcs;
                		  frameDataReader_mp->get_datareader_cache_status(drcs);

                		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                				  (LM_DEBUG, "Protocol Status for: %s [sample rcv: %lld (%lld) HB: %lld (%lld) ACKs: %lld (%lld) NACKs: %lld (%lld) Rejected: %lld]",
                						  topicName_m.c_str(),
                						  drps.received_sample_count_change, drps.received_sample_bytes_change,
                						  drps.received_heartbeat_count_change, drps.received_heartbeat_bytes_change,
                						  drps.sent_ack_count_change, drps.sent_ack_bytes_change,
                						  drps.sent_nack_count_change, drps.sent_nack_bytes_change,
                						  drps.rejected_sample_count_change));

                		  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                				  (LM_DEBUG, "Cache Status: sample count (peak): %lld (%lld)", drcs.sample_count, drcs.sample_count_peak));

                	  }
                  }//if (cbReceiveNumCalls_m>0)
                  break;
                }//case ACSBulkData::BD_STOP
              case ACSBulkData::BD_RESET:
                {
                  if (DDSConfiguration::debugLevel>0)
                  {
                     ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,(LM_DEBUG, "sendReset has been received for: %s", topicName_m.c_str()));
                  }
                  if (enableCB_m) { BDNT_LISTENER_USER_ERR( callback_mp->cbReset() ) }
                  //Clean all parameters
                  currentState_m = StopState;
            	  dataLength_m = 0;
            	  frameCounter_m = 0;
                  totalFrames_m = 0;
                  nextFrame_m = 0;
                  conseqErrorCount_m=0;
                  break;
                }//case ACSBulkData::BD_RESET
              default:
                conseqErrorCount_m++;
                UnknownDataTypeCompletion udt(__FILE__, __LINE__, __FUNCTION__);
                udt.setDataType(message.typeOfdata);
                udt.setFrameCount(frameCounter_m);
                udt.setTotalFrameCount(totalFrames_m);
                callback_mp->onError(udt);
              }//switch
            }//if(si.valid_data)
        }
      else
        {
          conseqErrorCount_m++;
          DDSReturnErrorCompletion retErr(__FILE__, __LINE__, __FUNCTION__);
          retErr.setRetCode(retCode);  //would be good if we can give also string value
          callback_mp->onError(retErr);
        }//if(retCode)
    }//while
}//on_data_available

void BulkDataNTReaderListener::on_requested_deadline_missed(DDS::DataReader*, const DDS::RequestedDeadlineMissedStatus& )
{
  ACS_DDS_Errors::DDSRequestedDeadlineMissedCompletion dmerr(__FILE__, __LINE__, __FUNCTION__);
  initalizeLogging(); //force initialization of logging sys TBD changed
  callback_mp->onError(dmerr);
}//on_requested_deadline_missed

void BulkDataNTReaderListener::on_requested_incompatible_qos(DDS::DataReader*, const DDS::RequestedIncompatibleQosStatus&)
{
  ACS_DDS_Errors::DDSRequestedIncompatibleQoSCompletion iqerr(__FILE__, __LINE__, __FUNCTION__);
  initalizeLogging(); //force initialization of logging sys TBD changed
  callback_mp->onError(iqerr);
}//on_requested_incompatible_qos

void BulkDataNTReaderListener::on_liveliness_changed(DDS::DataReader*, const DDS::LivelinessChangedStatus& lcs)
{
  if (lcs.alive_count_change>0)
    {
      for(int i=0; i<lcs.alive_count_change; i++)
        {
          ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
              (LM_INFO, "A new sender has connected to flow: %s of the stream: %s. Total alive connection(s): %d",
                  callback_mp->getFlowName(), callback_mp->getStreamName(),
                  lcs.alive_count));

          // Check how many senders are connected to this flow
          // because with the actual version only one sender can be connected at a given time
          // (only one will receive the ACK, the others will timeout!)
          if (lcs.alive_count>1) {
        	  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
        	                (LM_WARNING, "Too many connected senders to flow: %s of the stream: %s: %d Max allowed 1",
        	                    callback_mp->getFlowName(), callback_mp->getStreamName(),
        	                    lcs.alive_count));
          }
          BDNT_LISTENER_USER_ERR( callback_mp->onSenderConnect(lcs.alive_count) )
        }//for
    }else
      {
        for(int i=lcs.alive_count_change; i<0; i++)
          {
            ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
                (LM_INFO, "A sender has disconnected to flow: %s of the stream: %s. Total alive connection(s): %d",
                    callback_mp->getFlowName(), callback_mp->getStreamName(),
                    lcs.alive_count));
            BDNT_LISTENER_USER_ERR( callback_mp->onSenderDisconnect(lcs.alive_count) )
          }//for
      }//if-else
}//on_liveliness_changed

void BulkDataNTReaderListener::on_subscription_matched(DDS::DataReader*, const DDS::SubscriptionMatchedStatus&)
{
  ACS_TRACE(__FUNCTION__);
}//on_subscription_matched

void BulkDataNTReaderListener::on_sample_rejected( DDS::DataReader*, const DDS::SampleRejectedStatus& srs)
{
	RepeatGuard rg(5000000/*=0.5s*/,0);
	if ( DDSConfiguration::debugLevel > 0 || rg.checkAndIncrement() )
	{
		if (DDSConfiguration::debugLevel<=0)
			LoggingProxy::AddData("repeatCount", "%d", rg.count() );

		ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
				(LM_WARNING, "Sample Rejected, but NOT lost (need to be resent): reason %d, change: %d, total: %d!",
						srs.last_reason, srs.total_count_change, srs.total_count));
	}
}//on_sample_rejected

void BulkDataNTReaderListener::on_sample_lost(DDS::DataReader*, const DDS::SampleLostStatus& s)
{
  ACS_BD_Errors::SampleLostCompletion sle(__FILE__, __LINE__, __FUNCTION__);
  sle.setLostSamples(s.total_count_change);
  sle.setNextDataFrame(nextFrame_m);
  sle.setFrameCount(frameCounter_m);
  sle.setStreamFlowName(topicName_m.c_str());
  sle.setTotalSampleLost(s.total_count);
  sle.setReason(s.last_reason);
  initalizeLogging(); //force initialization of logging sys TBD changed
  BDNT_LISTENER_USER_ERR( callback_mp->onDataLost(frameCounter_m, totalFrames_m, sle) )
}//on_sample_lost

void BulkDataNTReaderListener::increasConseqErrorCount()
{
  conseqErrorCount_m++;
  if (conseqErrorCount_m>=maxConseqErrorCount_m)
    {

      ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
          (LM_ALERT, "Too many consequent errors: %d/%d on %s",
              conseqErrorCount_m, maxConseqErrorCount_m, topicName_m.c_str()));
      //TBD: disconnect
    }
}//increasConseqErroCount

void BulkDataNTReaderListener::enableCallingCB()
{
	enableCB_m=true;
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_INFO, "Calling user's CB for flow: %s of the stream: %s has been ENABLED.",
					callback_mp->getFlowName(), callback_mp->getStreamName()));
}//enableCallingCB

void BulkDataNTReaderListener::disableCallingCB()
{
	enableCB_m=false;
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_INFO, "Calling user's CB for flow: %s of the stream: %s has been DISABLED.",
					callback_mp->getFlowName(), callback_mp->getStreamName()));
}//disableCallingCB
