#ifndef _BULK_DATA_NT_SENDER_FLOW_H_
#define _BULK_DATA_NT_SENDER_FLOW_H_
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
* "@(#) $Id: bulkDataNTSenderFlow.h,v 1.22 2013/01/08 11:39:40 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataNTFlow.h"
#include "bulkDataNTDDSPublisher.h"
#include "bulkDataNTSenderStream.h"
#include "bulkDataNTSenderFlowCallback.h"
#include "bulkDataNTWriterListener.h"
#include <ACE.h>
#include <bulkDataC.h>
#include <acsncSimpleConsumer.h>

namespace AcsBulkdata
{

struct statisticsStruct {
	ACE_UINT64 startSendDuration;
	DDS::DataWriterProtocolStatus startSendDwps;
	DDS::DataWriterCacheStatus startSendDwcs;

	ACE_UINT64 stopSendDuration;
	DDS::DataWriterProtocolStatus stopSendDwps;
	DDS::DataWriterCacheStatus stopSendDwcs;

	std::vector<ACE_UINT64> sendDataDuration;
	std::vector<DDS::DataWriterProtocolStatus> sendDataDwps;
	std::vector<DDS::DataWriterCacheStatus> sendDataDwcs;

	ACE_UINT64 resetSendDuration;
	DDS::DataWriterProtocolStatus resetSendDwps;
	DDS::DataWriterCacheStatus resetSendDwcs;
} ;

class BulkDataNTSenderStream;

class BulkDataNTSenderFlow : public BulkDataNTFlow
{
public:

	/**
	 * Sender Flow constructor.
	 * @param senderStream  sender stream where the flow will be created
	 * @param flowName the name of flow that should be created
	 * @param sndCfg sender flow configuration
	 * @param cb pointer to callback
	 * @param releaseCB should CB be released when flow is destroyed
	 */
	BulkDataNTSenderFlow(BulkDataNTSenderStream *senderStream,
						const char* flowName,
						const SenderFlowConfiguration &sndCfg,
						BulkDataNTSenderFlowCallback *cb,
						bool releaseCB );

	/**
	 * Sender flow destructor
	 */
	virtual ~BulkDataNTSenderFlow();


	/**
	 * Returns number of connected receivers for the flow.
	 * @return number of receivers
	 */
	unsigned int getNumberOfReceivers();

	/**
	 *
	 * @param param  parameter data
	 * @param len parameter data length
	 * @exception #StartSendErrorExImpl
	 */
	/**
	 * Deprecated method to send "START" and parameter using #ACE_Message_Block
	 * @deprecated
	 * @param param parameter in form of #ACE_Message_Block
	 */
	void startSend(ACE_Message_Block *param = 0);

	/**
	 * Method to send "START" and parameter
	 * @param param  parameter data
	 * @param len parameter data length
	 * @exception #StartSendErrorExImpl
	 */
	void startSend(const unsigned char *param, size_t len);

	/**
	 * Method to send data. The data length can be of any size.
	 * @param buffer  data
	 * @param len length of data
	 * @exception #SendDataErrorExImpl
	 */
	void sendData(const unsigned char *buffer, size_t len);

	/**
	 * Method to send "STOP"
	 * @exception #StopSendErrorExImpl
	 */
	void stopSend();

	/**
	 * Method to send "RESET"
	 * @exception #ResetSendErrorExImpl
	 */
	void resetSend();

	void dumpStatistics(ACE_Log_Priority level=LM_DEBUG);
	/**
	 * Get and optionally logs the DataWriter statistics.
	 *
	 * RTI DDS collects global statistics and the changes from the last set of statistics so getting
	 * the statistics has the effect to clean the dfference.
	 * If this method is called before an operation then it is the same as performing a "clean"
	 * and in case of error the stats read from the DataWriter refers to the last operation only.
	 *
	 * @param print: if true log the statistics
	 */
	void getStatistics(bool log);
	void getDelayedStatistics(bool log, int flowMethod);
	void statisticsLogs();
    static void errorPropagationHandler(bulkdata::errorStatusBlock event, void* handlerParam);

protected:

	typedef enum {StartState, DataRcvState, StopState, IgnoreDataState } SenderFlowStates;
	SenderFlowStates currentState_m; /// current state of Sender Flow
	static const char* state2String[]; /// strings name of states
	nc::SimpleConsumer<bulkdata::errorStatusBlock> *errorStatusConsumer_p;

	AcsBulkdata::BulkDataNTSenderStream *senderStream_m; /// pointer to the sender

	SenderFlowConfiguration senderFlowCfg_m; /// flow configuration

	BulkDataNTSenderFlowCallback *callback_m; /// callback
	bool releaseCB_m; /// should the CB be destroyed when the flow is destroyed

	AcsBulkdata::BulkDataNTDDSPublisher *ddsPublisher_m;  /// DDS publisher
	DDS::Topic *ddsTopic_m;  /// DDS topic where data will be write
	BulkDataNTWriterListener *writerReaderListener_m;  /// DDS listener for status etc.
	ACSBulkData::BulkDataNTFrameDataWriter *ddsDataWriter_m;  /// DDS writer

	DDS::Duration_t ackTimeout_m;  /// ACKs timeout
	void setACKsTimeout(double ACKsTimeout);  /// setter for ackTimeout

	double throttling_m;
	double throttlingMinFrameTime_m; /// min time that should elapsed for sending one frame (640000 bytes)
    int sts_m;

	void setThrottling(double throttling); /// setter from throttling parameter (in MB/sec)

	// should it go to upper class Publisher ?
	/**
	 * Common method to send frame(s) to the topic. The method it is used internally by:
	 * #startSend, #sendData, #stopSend and #resetSend
	 * @param dataType data frame type (START/DATA/STOP/RESET)
	 * @param param   - data
	 * @param len length of data
	 * @param restFrameCount how many frames do we have still sent
	 * @param waitForACKs  shall be wait for the ACKs (we force to wait for ACKs)
	 * @exception #SendFrameTimeoutExImpl, #SendFrameGenericErrorExImpl, #FrameAckTimeoutExImpl
	 */
	void writeFrame(ACSBulkData::DataType dataType,  const unsigned char *param=0, size_t len=0, unsigned int restFrameCount=0, bool waitForACKs=false);

	/// frame
	ACSBulkData::BulkDataNTFrame *frame_m;

	/// disable default - empty constructor
	BulkDataNTSenderFlow();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTSenderFlow&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTSenderFlow(const BulkDataNTSenderFlow&);

	statisticsStruct delayedStatistics;

};//class BulkDataSenderFlow

};


#endif /*!_H*/
