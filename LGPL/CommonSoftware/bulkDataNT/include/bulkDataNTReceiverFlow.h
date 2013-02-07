#ifndef _BULK_DATA_NT_RECEIVER_FLOW_H_
#define _BULK_DATA_NT_RECEIVER_FLOW_H_
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
* "@(#) $Id: bulkDataNTReceiverFlow.h,v 1.18 2013/02/07 11:02:29 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataNTFlow.h"
#include "bulkDataNTDDSSubscriber.h"
#include "bulkDataNTStream.h"
#include "bulkDataNTCallback.h"
#include "bulkDataNTReaderListener.h"


namespace AcsBulkdata
{

class BulkDataNTReceiverStreamBase;

class BulkDataNTReceiverFlow : public BulkDataNTFlow
{
public:


	/**
	 * Receiver flow constructor. It is used by REceiverStream class.
	 * @param receiverStream pointer to the stream where the flow should be created
	 * @param flowName the name of the flow that we want to create
	 * @param rcvCfg receiver flow configuration
	 * @param cb  pointer to the receiver flow callback
	 * @param releaseCB should the callback be relased when the flow is destroyed
	 */
	BulkDataNTReceiverFlow(BulkDataNTReceiverStreamBase *receiverStream,
							const char* flowName,
							const ReceiverFlowConfiguration &rcvCfg,
							BulkDataNTCallback *cb,
							bool releaseCB);

	/**
	 *Receiver flow destructor
	 */
	virtual ~BulkDataNTReceiverFlow();

	/**
	 * Sets the name of receiver, so that we can distinguish between different receivers in the system.
	 * @param recvName name of receiver.
	 */
	void setReceiverName(char* recvName);

	/**
	 *If we need to get callback object, for example if CB is created using template parameter.
	 * @return pointer to callback object.
	 */
	BulkDataNTCallback* getCallbackObject() { return callback_m; }

	/**
	 * template/casted version of #getCallbackObject
	 * @return pointer to casted callback object
	 */
	template<class T>
	T* getCallback() { return dynamic_cast<T*>(callback_m); }

	/// Enables calling user's CB (cbStart, cbReceiver, cbStop)
	void enableCallingCB();

	/// Disables calling user's CB (cbStart, cbReceiver, cbStop)
	void disableCallingCB();

	void dumpStatistics();

protected:
	AcsBulkdata::BulkDataNTReceiverStreamBase *receiverStream_m;

	AcsBulkdata::BulkDataNTDDSSubscriber *ddsSubscriber_m;
	ACSBulkData::BulkDataNTFrameDataReader *ddsDataReader_m;
	DDS::Topic *ddsTopic_m; /// DDS topic
	BulkDataNTReaderListener *dataReaderListener_m; /// DDS reader
	BulkDataNTCallback *callback_m; /// callback
	bool releaseCB_m; /// should the CB be destroyed when the flow is destroyed
	ReceiverFlowConfiguration rcvCfg_m; /// local copy of rcv configuration

	/// disable default - empty constructor
	BulkDataNTReceiverFlow();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTReceiverFlow&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTReceiverFlow(const BulkDataNTReceiverFlow&);
};//class BulkDataSenderFlow

};


#endif /*!_H*/
