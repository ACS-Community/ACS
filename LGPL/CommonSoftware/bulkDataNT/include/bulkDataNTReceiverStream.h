#ifndef _BULK_DATA_NT_RECEIVER_STREAM_H_
#define _BULK_DATA_NT_RECEIVER_STREAM_H_
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
 * "@(#) $Id: bulkDataNTReceiverStream.h,v 1.18 2012/03/30 13:49:58 bjeram Exp $"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * bjeram  2011-04-19  created
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataNTStream.h"
#include "bulkDataNTReceiverFlow.h"
#include <map>

namespace AcsBulkdata
{



//TBD: actually we can move lots of stuff from BulkDataNTReceiverStream template class which do not depend on template parameter
class BulkDataNTReceiverStreamBase : public BulkDataNTStream
{
public:
	BulkDataNTReceiverStreamBase(const char* streamName, const ReceiverStreamConfiguration &cfg):
		BulkDataNTStream(streamName, cfg), receiverName_m("DefaultReceiver") {}

	BulkDataNTReceiverStreamBase( const char* receiverName, const char* streamName, const ReceiverStreamConfiguration &cfg):
			BulkDataNTStream(streamName, cfg), receiverName_m(receiverName){}

	/**
	 * Set receiver name (in receiver callback). Nothing to do with stream/flow name!
	 * @param recvName the name of receiver
	 */
	void setReceiverName(char * recvName) { receiverName_m = recvName; }

	/**
	 * Set receiver name (in receiver callback). Nothing to do with stream/flow name!
	 * @param recvName
	 */
	void setReceiverName(const char * recvName) { receiverName_m = recvName; }

	/**
	 * Gives the name (of previously set) receiver name
	 * @return receiver name
	 */
	const char* getReceiverName() { return receiverName_m.c_str(); }

protected:

	std::string receiverName_m;

};//BulkDataNTReceiverStreamBase

template<class TReceiverCallback>
class BulkDataNTReceiverStream : public BulkDataNTReceiverStreamBase

//, public AcsBulkdata::BulkDataNTDDSSubscriber

{
public:

	/**
	 * Receiver stream constructor where we do not specify receiver name.
	 * Receiver name can be specified later using #setReceiverName method
	 * @param streamName name of the stream that will be create
	 * @param cfg configuration (optional)
	 * @exception #StreamCreateProblemExImpl
	 */
	BulkDataNTReceiverStream(const char* streamName, const ReceiverStreamConfiguration &cfg=ReceiverStreamConfiguration());

	/**
	 * Receiver stream constructor where we specify receiver name.
	 * @param receiverName receiver's name that it is easier to distinguish between differnet receivers in the ssytem.
	 * @param streamName name of the stream that will be create
	 * @param cfg configuration (optional)
	 * @exception #StreamCreateProblemExImpl
	 */
	BulkDataNTReceiverStream(
			const char* receiverName,
			const char* streamName,
			const ReceiverStreamConfiguration &cfg=ReceiverStreamConfiguration());

	/**
	 * Receiver Stream destructor. It destroys also all flows belonging to the stream.
	 */
	virtual ~BulkDataNTReceiverStream();


	/**
	 * The method creates a flow an the stream
	 * @param flowName name of flow that willb e created
	 * @param cfg (optional) REceiver's flow configuration
	 * @param cb callback - if no specified it is created from tempalte parameter
	 * @param releaseCB - should be the callback relased when the flow is destroyed
	 * @return
	 */
	BulkDataNTReceiverFlow* createFlow(const char *flowName, const ReceiverFlowConfiguration &cfg=ReceiverFlowConfiguration(),
			BulkDataNTCallback *cb=0, bool releaseCB=false);

	/**
	 * It returns pointer to Receiver Flow.
	 * @param flowName the name of the flow
	 * @return receiver flow if present in the stream, otherwise exception
	 * @exception #FlowNotExistExImpl
	 */
	BulkDataNTReceiverFlow* getFlow(const char* flowName);


	/**
	 *Method just to check if the flow exists in the stream.
	 * @param flowName the name of the flow
	 * @return true if the flow with name flowName exists in the stream otherwise flase
	 */
	bool existFlow(const char* flowName);

	/**
	 *  Create multiple flows (user defined)
	 *  @param fepsConfig
	 *  @throw ACSBulkDataError::AVStreamEndpointErrorExImpl
	 *  @throw ACSBulkDataError::AVInvalidFlowNumberExImpl
	 *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
	 *  @return void
	 */
	void createMultipleFlowsFromConfig(const char *config);

	/** Get the receiver flow and sep configuration
	 *  @throw ACSBulkDataError::AVReceiverConfigErrorExImpl
	 *  @return bulkdata::BulkDataReceiverConfig *
	 */

	/** Get the names of the all flows created by the stream
	 *  @return vector<string>
	 */
	std::vector<std::string> getFlowNames();

	/**
	 * Returns number of flows in the stream.
	 * @return number of flows
	 */
	unsigned int getFlowNumber();

	/// Enables calling user's CB (cbStart, cbReceiver, cbStop) for All flows
	void enableCallingCBforAllFlows();

	/// Disables calling user's CB (cbStart, cbReceiver, cbStop) for All Flows
	void disableCallingCBforAllFlows();



	/** Subscribe to the Notification Mechanism
	 *  @throw ACSBulkDataError::AVNotificationMechanismErrorExImpl
	 *  @param ACS::CBvoid_ptr
	 *  @return void
	 *  @htmlonly
 <br><hr>
 @endhtmlonly
	 */
	/// void subscribeNotification(ACS::CBvoid_ptr notifCb);

	/**
	 *  Enable or disable that data are sent to the user's CB.
	 *  By default this is enable.
	 *  This operation has to be use with caution!!!
	 *  @param enable true -> data will be sent to the user's CB,
	 *                false -> data will *not* be sent to the user's CB,
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	 */
	/// void fwdData2UserCB(CORBA::Boolean enable);

	/*
	 *  @throw ACSBulkDataError::AVNotificationMechanismErrorExImpl
	 */
	/// void notifySender(const ACSErr::Completion& comp);



	/// void setCbTimeout(const char * cbTimeout);


protected:

	virtual void removeFlowFromMap(const char* flow);

	typedef std::map<std::string, BulkDataNTReceiverFlow*> ReceiverFlowMap;
	ReceiverFlowMap receiverFlows_m;
	// we need a flag that prevents elements to be removed from map when we delete flows from dtor
	bool notRemoveFromMap_m;

	/// disable default - empty constructor
	BulkDataNTReceiverStream();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTReceiverStream&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTReceiverStream(const BulkDataNTReceiverStream&);
};//class BulkDataNTReceiverStream

};//namespace AcsBulkdata

#include "bulkDataNTReceiverStream.i"

#endif /*!_H*/
