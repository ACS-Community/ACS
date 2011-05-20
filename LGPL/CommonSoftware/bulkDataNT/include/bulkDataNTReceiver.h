#ifndef _BULK_DATA_NT_RECEIVER_H_
#define _BULK_DATA_NT_RECEIVER_H_
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
* "@(#) $Id: bulkDataNTReceiver.h,v 1.1 2011/05/20 13:39:23 bjeram Exp $"
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

#include "bulkDataNTBase.h"
#include "bulkDataNTDDSSubscriber.h"

namespace AcsBulkdata
{

template<class TReceiverCallback>
class BulkDataReceiverNT
{
  public:

/**
 * Constructor
 */
	BulkDataReceiverNT();

/**
 * Destructor
 */
virtual ~BulkDataReceiverNT();

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



void setCbTimeout(const char * cbTimeout);

};//class BulkDataReceiverNT

};//namespace AcsBulkdata

#endif /*!_H*/
