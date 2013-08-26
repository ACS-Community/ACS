#ifndef _BULKDATA_SENDER_H
#define _BULKDATA_SENDER_H
/*******************************************************************************
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *
 *
 * "@(#)"
 *
 * who       when      what
 * --------  --------  ----------------------------------------------
 * oat       27/01/05  created 
 */

#include <iostream>

#include "orbsvcs/AV/AVStreams_i.h"
#include "orbsvcs/AV/Endpoint_Strategy.h"
#include "orbsvcs/AV/Protocol_Factory.h"
#include "orbsvcs/AV/Flows_T.h"
#include "orbsvcs/AV/Transport.h"
#include "orbsvcs/AV/Policy.h"

#include "ACSBulkDataError.h"
#include "bulkDataSenderDefaultCb.h"
#include "bulkDataFlowProducer.h"

#include "bulkDataC.h"

// #include "ace/High_Res_Timer.h"

// #include <acsQoS.h>

/** @file bulkDataSender.h  
 */

namespace AcsBulkdata
{  
    /** @defgroup BULKDATASENDERDOC Bulk Data Sender
     *  @{
     * @htmlonly
     <hr size="2" width="100%">
     <div align="left">
     <h2>Description</h2>
     bulkDataSender.h implements the Bulk Data Sender
     <br>
     <br>
     <h2>Links</h2>
     <ul>
     <li><a href="classBulkDataSender.html">BulkDataSender Class Reference</a></li>
     </ul>
     </div>
     @endhtmlonly
     * @}
     */


    template<class TSenderCallback>
    class BulkDataSender
    {
      public:
      
	/**
	 * Constructor
	 */
	BulkDataSender();
      
	/**
	 * Destructor
	 */
	virtual ~BulkDataSender();
      
	/** Initialize the A/V
     *  @throw ACSBulkDataError::AVInitErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void initialize();

	/** Create single flow (TCP, A/V default port)
     *  @throw ACSBulkDataError::AVStreamEndpointErrorExImpl
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void createSingleFlow();

	/** Create multiple flows (user defined)
	 * @param fepsConfig
     *  @throw ACSBulkDataError::AVStreamEndpointErrorExImpl
     *  @throw ACSBulkDataError::AVInvalidFlowNumberExImpl
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void createMultipleFlows(const char *fepsConfig);

	/** Bind the Stram End Points
	 * @param recv_config
     *  @throw ACSBulkDataError::AVConnectErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void connectToPeer(bulkdata::BulkDataReceiverConfig *recvConfig_p);

	/** Get the Flow Protocol according to flowname
	 * @param flowname
	 * @param TAO_AV_Protocol_Object *
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
     *  @throw ACSBulkDataError::AVProtocolErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void getFlowProtocol(ACE_CString &flowname, TAO_AV_Protocol_Object *&currentProtocol_p);

	/** 
	 *  Calls the Receiver cbstart() method once the connection is established.
	 * @param ACE_CString
	 * @param ACE_Message_Block 
     *  @throw ACSBulkDataError::AVSendFrameErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void startSend(CORBA::ULong flownumber, ACE_Message_Block *param = 0);

	/** 
	 *  Calls the Receiver cbstart() method once the connection is established.
	 * @param ACE_CString
	 * @param const char * 
     *  @throw ACSBulkDataError::AVSendFrameErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void startSend(CORBA::ULong flownumber, const char *param, size_t len);

	/** 
	 * Calls the Receiver receive_frame() method.
	 * @param CORBA::ULong flowNumber
	 * @param ACE_Message_Block 
     *  @throw ACSBulkDataError::AVSendFrameErrorExImpl
	 * @return void
	 * @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void sendData(CORBA::ULong flownumber, ACE_Message_Block *buffer);
           
	/** 
	 * Calls the Receiver receive_frame() method.
	 * @param CORBA::ULong flowNumber
	 * @param const char *
	 * @param size_t len
	 * @param unsigned long timeout (msec)
     *  @throw ACSBulkDataError::AVSendFrameErrorExImpl
	 * @return void
	 * @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void sendData(CORBA::ULong flownumber, const char *buffer, size_t len);

	/** 
	 *  Calls the Receiver handle_stop() method.
	 * @param ACE_CString
	 * @param ACE_Message_Block 
     *  @throw ACSBulkDataError::AVStopSendErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void stopSend(CORBA::ULong flownumber);

	/** Disconnect peer
     *  @throw ACSBulkDataError::AVDisconnectErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void disconnectPeer();
 
	TAO_StreamCtrl * getStreamCtrl() 
	    {
		return streamctrl_p;
	    }

	const char *getFlowSpec(const ACE_CString & flowName);

	/** Get the names of the connected flows
	 *  @return vector<string>
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	std::vector<std::string> getFlowNames();

        // checks if the handler referenced by handle is registered inside the ACE Reactor
	ACE_HANDLE findHandle(ACE_CString &flowname);

	/* THE FOLLOWING METHODS ARE UNDER TESTING - PLEASE DO NOT USE THEM */
	/********************************************************************/

	void startStream(CORBA::ULong flownumber);
	
	void sendStream(CORBA::ULong flownumber, ACE_Message_Block *buffer);

	void stopStream(CORBA::ULong flownumber);

	/********************************************************************/

      private:

	typedef ACE_Hash_Map_Manager<ACE_CString, BulkDataFlowProducer<TSenderCallback> *, ACE_Null_Mutex> FepObjects;
	typedef ACE_Hash_Map_Iterator<ACE_CString, BulkDataFlowProducer<TSenderCallback> *, ACE_Null_Mutex>  FepObjectsIterator;

	typedef ACE_Hash_Map_Manager<ACE_CString, ACE_HANDLE, ACE_Null_Mutex> HandleMap;
	typedef ACE_Hash_Map_Iterator<ACE_CString, ACE_HANDLE, ACE_Null_Mutex>  HandleMapIterator;

	/** Initialize the A/V part A
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void initPartA();

	/** Create the part A Stream End Point
	 *  @return AVStreams::StreamEndPoint_A_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::StreamEndPoint_A_ptr createSepA();

	/** Create the Producer Flow End Point
	 * @param flowname
	 * @param AVStreams::protocolSpec
	 * @param format
	 *  @return AVStreams::FlowProducer_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::FlowProducer_ptr createFepProducerA(ACE_CString &flowname,
						       AVStreams::protocolSpec protocols,
						       ACE_CString &format,
						       TAO_StreamCtrl *strctrl_p);


	/** Add the Flow End Point to the Stream End Point
	 * @param AVStreams::StreamEndPoint_A_ptr
	 * @param AVStreams::FlowProducer_ptr
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void addFepToSep(AVStreams::StreamEndPoint_A_ptr locSepA_p, AVStreams::FlowProducer_ptr locFepA_p);


	/** Create the Stream Control
	 *  @return TAO_StreamCtrl *
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	TAO_StreamCtrl *createStreamCtrl();     


	/** Create the QoS
	 *  @return AVStreams::streamQoS_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	//      AVStreams::streamQoS * create_QoS();


	/** Create the Forward Flow Specifications
	 * @param flowname
	 * @param direction
	 * @param format_name
	 * @param flow_protocol
	 * @param carrier_protocol
	 * @param local_address
	 * @param remote_address
	 *  @return  const char *
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	const char * createFwdFlowSpec(ACE_CString &flowname,
				       ACE_CString &direction,
				       ACE_CString &formatName,
				       ACE_CString &flowProtocol,
				       ACE_CString &carrierProtocol,
				       ACE_CString &localAddress,
				       ACE_CString &remoteAddress);


	/** Set the Receiver configuration
	 * @param *recv_config
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void setReceiverConfig(bulkdata::BulkDataReceiverConfig *recvConfig_p);

                    

	/** Accessor for the part A Stream End Point
	 *  @return  AVStreams::StreamEndPoint_A_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::StreamEndPoint_A_ptr getStreamEndPointA();


	/** Delete stream ctrl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void deleteStreamCtrl();



	/** Delete allocated feps
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void deleteFepsA();

	/** Delete allocated seps
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void deleteSepA();

	void deleteConnector();

	void deleteHandler();

	/** Create the Flow Specifications
	 * @param flowname
	 * @param fep_protocol
	 *  @return  const char *
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	const char * createFlowSpec(ACE_CString &flowname,
				    ACE_CString &fepProtocol);

	void mergeFlowSpecs();

	/**
	 * The endpoint strategy used by the sender
	 */
	TAO_AV_Endpoint_Reactive_Strategy_A<TAO_StreamEndPoint_A,TAO_VDev,AV_Null_MediaCtrl> endpointStrategy_m;

	AVStreams::StreamEndPoint_A_var sepA_p;

	AVStreams::StreamEndPoint_B_var sepB_p;


	//AVStreams::streamQoS_var the_qos;
	struct FepsCfgA
	{
	    ACE_CString fepsFlowname;
	    ACE_CString fepsFormat;
	    ACE_CString fepsProtocol;
	};

	FepObjects fepMap_m;

	HandleMap handleMap_m;

 	AVStreams::flowSpec_var recvFeps_p;

	AVStreams::flowSpec senderFeps_m;

	TAO_StreamEndPoint_A * sepRefCount_p;

	CORBA::Boolean disconnectPeerFlag;
    
	AVStreams::flowSpec flowSpec_m;

	TAO_StreamCtrl *streamctrl_p;

	/**
	 * ALMA C++ coding standards state copy operators should be disabled.
	 */
	//void operator=(const BulkDataSender&);

//      public:
// this part should be private
// deprecated and back-incompatible
// needed for the distributer but will be removed as soon as possible
//
//	AVStreams::flowSpec flowSpec_m;
//	TAO_StreamCtrl *streamctrl_p;
    };
}

#include "bulkDataSender.i"

#endif /* _BULKDATA_SENDER_H */
