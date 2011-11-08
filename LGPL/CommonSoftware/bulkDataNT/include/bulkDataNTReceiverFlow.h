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
* "@(#) $Id: bulkDataNTReceiverFlow.h,v 1.12 2011/11/08 11:12:04 bjeram Exp $"
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


#include "bulkDataNTDDSSubscriber.h"
#include "bulkDataNTStream.h"
#include "bulkDataNTCallback.h"
#include "bulkDataNTReaderListener.h"



namespace AcsBulkdata
{

class BulkDataNTReceiverStreamBase;

class BulkDataNTReceiverFlow
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTReceiverFlow(BulkDataNTReceiverStreamBase *receiverStream,
							const char* flowName,
							const ReceiverFlowConfiguration &rcvCfg,
							BulkDataNTCallback *cb,
							bool releaseCB);

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTReceiverFlow();

	void setReceiverName(char* recvName);

	BulkDataNTCallback* getCallbackObject() { return callback_m; }

	template<class T>
	T* getCallback() { return dynamic_cast<T>(callback_m); }

	//TBD:: setCB();
protected:
	AcsBulkdata::BulkDataNTReceiverStreamBase *receiverStream_m;
	std::string flowName_m;

	AcsBulkdata::BulkDataNTDDSSubscriber *ddsSubscriber_m;
	ACSBulkData::BulkDataNTFrameDataReader *ddsDataReader_m;
	DDS::Topic *ddsTopic_m;
	BulkDataNTReaderListener *dataReaderListener_m;  // we can keep it in  CB ?
	BulkDataNTCallback *callback_m; //we can keep it in "Subscriber" ?
	bool releaseCB_m;

	/// disable default - empty constructor
	BulkDataNTReceiverFlow();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTReceiverFlow&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTReceiverFlow(const BulkDataNTReceiverFlow&);
};//class BulkDataSenderFlow

};


#endif /*!_H*/
