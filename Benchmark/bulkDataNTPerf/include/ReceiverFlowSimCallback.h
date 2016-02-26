#ifndef RECEIVERFLOWSIMCALLBACK_H
#define RECEIVERFLOWSIMCALLBACK_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2016-02-26  created
*/

#include <list>
#include <bulkDataNTCallback.h>

/**
 * Callback for the BDNT receiver
*/

class  ReceiverFlowSimCallback :  public AcsBulkdata::BulkDataNTCallback
{
public:
	ReceiverFlowSimCallback();

	virtual ~ReceiverFlowSimCallback();

	int cbStart(unsigned char* userParam_p, unsigned  int size);

	int cbReceive(unsigned char* data, unsigned  int size);

	int cbStop();

	void setStoreData(bool shouldStoreData) {
		storeData = shouldStoreData;
	}

	std::list<unsigned char> getData() {
		return dataToStore;
	}

	uint16_t getUserParamSize() {
		return userParamSize;
	}

	static long cbDealy;
	static bool cbReceivePrint;

	/**
		 * This method is called when an error happens in the flow's callback (cbStart/cbReceive/cbStop),
		 *
		 * @param error - at the moment possible completion errors are:
		 * #WrongFrameOrderCompletion
		 * #UnknownDataTypeCompletion
		 * #DDSReturnErrorCompletion
		 * #CBReceiveProcessTimeoutCompletion
		 * #DDSRequestedDeadlineMissedCompletion Requested
		 * #DDSRequestedIncompatibleQoSCompletion
		 *
		 * @see AcsBulkdata::BulkDataNTCallback
		 */
		virtual void onError(ACSErr::CompletionImpl &error);

		/**
		 * The method is called when a new sender is connected to a flow
		 * @param totalSeners new number os senders after connect
		 *
		 * @see AcsBulkdata::BulkDataNTCallback
		 */
		virtual void onSenderConnect(unsigned short totalSenders);

		/**
		 * The method is called when a sender is disconnected for a flow
		 * @param totalSeners new number of senders, after disconnect
		 *
		 * @see AcsBulkdata::BulkDataNTCallback
		 */
		virtual void onSenderDisconnect(unsigned short totalSenders);

		/**
		 * The method is called when a frame (DDS sample) did not arrive.
		 * The default implementation just log the completion.
		 * @param frmaeCount - missed frame number/count
		 * @param totalFrames - total number of frames that should arrived
		 * @param error completion: #SampleLostCompletion, if detected by DDS or #FrameLostCompletion if detected by BD
		 *
		 * @see AcsBulkdata::BulkDataNTCallback
		 */
		virtual void onDataLost(unsigned long frameCount, unsigned long totalFrames, ACSErr::CompletionImpl &error);

private:
	std::string fn; ///flow Name
	std::string sn; ///stream name
	unsigned int totalRcvData; ///total size of all received data
	unsigned int rcvDataStartStop; ///size of received data between Start/stop
	unsigned int frameCount; ///frame count
	bool storeData;
	uint16_t userParamSize;
	std::list<unsigned char> dataToStore;
};

#endif /*!RECEIVERFLOWSIMCALLBACK_H*/
