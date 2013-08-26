#ifndef _BULKDATA_RECEIVER_H
#define _BULKDATA_RECEIVER_H
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

#include <vector>

#include "orbsvcs/AV/AVStreams_i.h"
#include "orbsvcs/AV/Endpoint_Strategy.h"
#include "orbsvcs/AV/Protocol_Factory.h"
#include "orbsvcs/AV/Flows_T.h"
#include "orbsvcs/AV/Transport.h"
#include "orbsvcs/AV/Policy.h"

#include "ACSBulkDataError.h"
#include "bulkDataFlowConsumer.h"

#include "bulkDataC.h"

/** @file bulkDataReceiver.h  
 */

namespace AcsBulkdata
{  
    /** @defgroup BULKDATARECEIVERDOC Bulk Data Receiver
     *  @{
     * @htmlonly
     <hr size="2" width="100%">
     <div align="left">
     <h2>Description</h2>
     bulkDataReceiver.h implements the Bulk Data Receiver
     <br>
     <br>
     <h2>Links</h2>
     <ul>
     <li><a href="classBulkDataReceiver.html">BulkDataReceiver Class Reference</a></li>
     </ul>
     </div>
     @endhtmlonly
     * @}
     */

    template<class TReceiverCallback>
    class BulkDataReceiver
    {
      public:
      
	/**
	 * Constructor
	 */
	BulkDataReceiver();
      
	/**
	 * Destructor
	 */
	virtual ~BulkDataReceiver();
      
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
	 *  @param fepsConfig
     *  @throw ACSBulkDataError::AVStreamEndpointErrorExImpl 
     *  @throw ACSBulkDataError::AVInvalidFlowNumberExImpl
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void createMultipleFlows(const char *fepsConfig);

	/** Get the receiver flow and sep configuration
     *  @throw ACSBulkDataError::AVReceiverConfigErrorExImpl
	 *  @return bulkdata::BulkDataReceiverConfig * 
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	bulkdata::BulkDataReceiverConfig * getReceiverConfig();

	/** Accessor to allocated receiver callback
	 * @param ACE_CString 
	 * @param TReceiverCallback 
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void getFlowCallback(ACE_CString &flowName, TReceiverCallback *&cb_p);

	/** Accessor to allocated receiver callback
	 * @param CORBA::ULong
	 * @param TReceiverCallback 
     *  @throw ACSBulkDataError::AVInvalidFlowNumberExImpl
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void getFlowCallback(CORBA::ULong flowNumber, TReceiverCallback *&cb_p);

	/** Close the Receiver
     *  @throw ACSBulkDataError::AVCloseReceiverErrorExImpl
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void closeReceiver();

	/** Get the names of the connected flows
	 *  @return vector<string>
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	std::vector<std::string> getFlowNames();

	/** Set receiver name in receiver callback
	 *  @param ACE_CString
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void setReceiverName(ACE_CString recvName);

	/** Subscribe to the Notification Mechanism
     *  @throw ACSBulkDataError::AVNotificationMechanismErrorExImpl
	 *  @param ACS::CBvoid_ptr
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void subscribeNotification(ACS::CBvoid_ptr notifCb);

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
	void fwdData2UserCB(CORBA::Boolean enable);

     /*
     *  @throw ACSBulkDataError::AVNotificationMechanismErrorExImpl
     */
	void notifySender(const ACSErr::Completion& comp);


	// method called by the distributor object to set the reactor handle
        // on its internal receiver
	void addHandle(ACE_CString flowName, ACE_HANDLE handle)
	    {
		handleMap_m.rebind(flowName,handle);	
	    }

	void setCbTimeout(const char * cbTimeout);


      private:

	typedef ACE_Hash_Map_Manager<ACE_CString, BulkDataFlowConsumer<TReceiverCallback> *, ACE_Null_Mutex> FepObjects;
	typedef ACE_Hash_Map_Iterator<ACE_CString, BulkDataFlowConsumer<TReceiverCallback> *, ACE_Null_Mutex>  FepObjectsIterator;

	typedef ACE_Hash_Map_Manager<ACE_CString, ACE_HANDLE, ACE_Null_Mutex> HandleMap;
	typedef ACE_Hash_Map_Iterator<ACE_CString, ACE_HANDLE, ACE_Null_Mutex>  HandleMapIterator;

	/** Initialize the A/V part B
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void initPartB();

	/** Create the part B Stream End Point
	 *  @return AVStreams::StreamEndPoint_B_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::StreamEndPoint_B_ptr createSepB();

	/** Create the Consumer Flow End Point
	 * @param flowname
	 * @param AVStreams::protocolSpec
	 * @param format
	 *  @return AVStreams::FlowConsumer_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::FlowConsumer_ptr createFepConsumerB(ACE_CString &flowName, AVStreams::protocolSpec protocols, ACE_CString &format);


	/** Add the Flow End Point to the Stream End Point
	 * @param AVStreams::StreamEndPoint_B_ptr
	 * @param AVStreams::FlowConsumer_ptr
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void addFepToSep(AVStreams::StreamEndPoint_B_ptr locSepB_p,AVStreams::FlowConsumer_ptr locFepB_p);

	/** Accessor for the part B Stream End Point
	 *  @return  AVStreams::StreamEndPoint_B_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::StreamEndPoint_B_ptr getStreamEndPointB();

	/** Accessor for the part B configuration
	 *  @return  AVStreams::StreamEndPoint_B_ptr
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	AVStreams::flowSpec * getFepsConfig();

	/** Delete allocated feps
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void deleteFepsB();


	/** Delete allocated sep
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void deleteSepB();

	void deleteAcceptor();

	void deleteHandler();

	void closeSocket();

	/** Create the Flow Specifications
	 * @param flowname
	 * @param fep_protocol
	 *  @return  const char *
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	const char * createFlowSpec(ACE_CString &flowName,
				    ACE_CString &fepProtocol);


      public:
    /**
     *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
    */
	bulkdata::Connection checkFlowCallbacks();

	bulkdata::Connection getSenderConnectionState()
	    {
		return recvConfig_p->connectionState;
	    }

      private:

	FepObjects fepMap_m;

	HandleMap handleMap_m;

	/**
	 * The endpoint strategy used by the receiver
	 */
	TAO_AV_Endpoint_Reactive_Strategy_B <TAO_StreamEndPoint_B,TAO_VDev,AV_Null_MediaCtrl> reactiveStrategy_m;    

	AVStreams::StreamEndPoint_B_var sepB_p;

	struct FepsCfgB
	{
	    ACE_CString fepsFlowname;
	    ACE_CString fepsFormat;
	    ACE_CString fepsProtocol;
	};

	AVStreams::flowSpec fepsData;

	//bulkdata::BulkDataReceiverConfig_var recvConfig_p; 
	bulkdata::BulkDataReceiverConfig * recvConfig_p; 

	TAO_StreamEndPoint_B *sepRefCount_p;

	CORBA::Boolean closeReceiverFlag;

	ACS::CBvoid_ptr locNotifCb_p;

	ACE_Time_Value cbTimeout_m;

	/**
	 * ALMA C++ coding standards state copy operators should be disabled.
	 */
	//void operator=(const BulkDataReceiver&);
    };
}

#include "bulkDataReceiver.i"

#endif /* _BULKDATA_RECEIVER_H */
