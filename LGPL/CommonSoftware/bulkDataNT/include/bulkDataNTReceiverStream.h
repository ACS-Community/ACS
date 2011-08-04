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
* "@(#) $Id: bulkDataNTReceiverStream.h,v 1.7 2011/08/04 11:22:12 bjeram Exp $"
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


template<class TReceiverCallback>
class BulkDataNTReceiverStream : public BulkDataNTStream
//, public AcsBulkdata::BulkDataNTDDSSubscriber

{
  public:

/**
 * Constructor
 */
	BulkDataNTReceiverStream(const char* streamName, const ReceiverStreamConfiguration &cfg=ReceiverStreamConfiguration());

/**
 * Destructor
 */
virtual ~BulkDataNTReceiverStream();



// TBD: is this better than createSingleFlow and createMultipleFlows
	// do we have to provide a name or just a number, for example we need three flows
	// if we decide for name then we have to change send methods as well.
	// here we should connect to the DDS topic
	// TBD: here we can also send the callback?
	BulkDataNTReceiverFlow* createFlow(const char *flowName, BulkDataCallback *cb=0, bool releaseCB=false);

	BulkDataNTReceiverFlow* getFlow(const char* flowName);

/**
 *  Create multiple flows (user defined)
 *  @param fepsConfig
 *  @throw ACSBulkDataError::AVStreamEndpointErrorExImpl
 *  @throw ACSBulkDataError::AVInvalidFlowNumberExImpl
 *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
 *  @return void
 *  @htmlonly
 <br><hr>
 @endhtmlonly
*/
void createMultipleFlowsFromConfig(const char *config);

/** Get the receiver flow and sep configuration
 *  @throw ACSBulkDataError::AVReceiverConfigErrorExImpl
 *  @return bulkdata::BulkDataReceiverConfig *
 *  @htmlonly
 <br><hr>
 @endhtmlonly
*/



/// bulkdata::BulkDataReceiverConfig * getReceiverConfig();

/** Accessor to allocated receiver callback
 * @param ACE_CString
 * @param TReceiverCallback
 *  @throw ACSBulkDataError::AVFlowEndpointErrorExImpl
 *  @return void
 *  @htmlonly
 <br><hr>
 @endhtmlonly
*/


/// void getFlowCallback(ACE_CString &flowName, TReceiverCallback *&cb_p);

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
/// void getFlowCallback(CORBA::ULong flowNumber, TReceiverCallback *&cb_p);

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
/// std::vector<std::string> getFlowNames();


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
