#ifndef _BULKDATA_DISTRIBUTER_H
#define _BULKDATA_DISTRIBUTER_H
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
 * oat       02/03/05  created 
 */


#include "bulkDataSender.h"
#include "bulkDataReceiver.h"

#include <acsQoS.h>

/** @file bulkDataDistributer.h  
 */

using namespace std;
using namespace ACSBulkDataError;

namespace AcsBulkdata
{  
    /** @defgroup BULKDATADISTRIBUTERDOC Bulk Data Distributer
     *  @{
     * @htmlonly
     <hr size="2" width="100%">
     <div align="left">
     <h2>Description</h2>
     bulkDataDistributer.h implements the Bulk Data Distributer
     <br>
     <br>
     <h2>Links</h2>
     <ul>
     <li><a href="classBulkDataDistributer.html">BulkDataDistributer Class Reference</a></li>
     </ul>
     </div>
     @endhtmlonly
     * @}
     */


    template<class TReceiverCallback, class TSenderCallback>
    class BulkDataDistributer
    { 

	enum Flow_Status
	{
	    FLOW_AVAILABLE,
	    FLOW_NOT_AVAILABLE
	};

	typedef ACE_Hash_Map_Manager <ACE_CString, BulkDataSender<TSenderCallback> *,ACE_Null_Mutex>  Sender_Map;
	typedef ACE_Hash_Map_Entry <ACE_CString, BulkDataSender<TSenderCallback> * > Sender_Map_Entry;
	typedef ACE_Hash_Map_Iterator <ACE_CString, BulkDataSender<TSenderCallback> * ,ACE_Null_Mutex>  Sender_Map_Iterator;

	typedef ACE_Hash_Map_Manager <CORBA::ULong, Flow_Status, ACE_Null_Mutex> Flows_Status_Map;
	typedef ACE_Hash_Map_Entry <CORBA::ULong, Flow_Status> Flows_Status_Map_Entry;
	typedef ACE_Hash_Map_Iterator <CORBA::ULong, Flow_Status, ACE_Null_Mutex> Flows_Status_Map_Iterator;

	typedef ACE_Hash_Map_Manager <ACE_CString, CORBA::ULong, ACE_Null_Mutex> Recv_Status_Map;
	typedef ACE_Hash_Map_Entry <ACE_CString, CORBA::ULong> Recv_Status_Map_Entry;
	typedef ACE_Hash_Map_Iterator <ACE_CString, CORBA::ULong, ACE_Null_Mutex> Recv_Status_Map_Iterator;



      public:
      
	/**
	 * Constructor
	 */
	BulkDataDistributer();
      
	/**
	 * Destructor
	 */
	virtual ~BulkDataDistributer();

	virtual void multiConnect(bulkdata::BulkDataReceiverConfig *recvConfig_p, const char *fepsConfig, const ACE_CString& receiverName)
	    throw (AVConnectErrorExImpl);
	
	virtual void multiDisconnect(const ACE_CString& receiverName)
	    throw (AVDisconnectErrorExImpl);

	virtual BulkDataReceiver<TReceiverCallback> *getReceiver() 
	    {
		return &receiver_m;
	    }

	virtual Sender_Map *getSenderMap() 
	    {
		return &senderMap_m;
	    }

	virtual bool isRecvConnected (const ACE_CString& receiverName);

	virtual void distSendStart (ACE_CString& flowName, CORBA::ULong flowNumber);

	virtual int distSendDataHsk (ACE_CString& flowName, ACE_Message_Block * frame_p, CORBA::ULong flowNumber);

	virtual int distSendData (ACE_CString& flowName, ACE_Message_Block * frame_p, CORBA::ULong flowNumber);

	virtual CORBA::Boolean distSendStopTimeout (ACE_CString& flowName, CORBA::ULong flowNumber);

	virtual void distSendStop (ACE_CString& flowName, CORBA::ULong flowNumber);

	void setTimeout (CORBA::ULong user_timeout) 
	    { timeout_m = user_timeout; }

	void setContSvc (ContainerServices * services_p)
	    {  contSvc_p = services_p; }  

      private:

	CORBA::Boolean getFlowReceiverStatus(const ACE_CString& receiverName, CORBA::ULong flowNumber);

	CORBA::Boolean isFlowReceiverAvailable(const ACE_CString& receiverName, CORBA::ULong flowNumber);

	BulkDataSender<TSenderCallback> *sender_p;
	
	BulkDataReceiver<TReceiverCallback> receiver_m;

	Sender_Map senderMap_m;

	Recv_Status_Map recvStatusMap_m;
	Flows_Status_Map flowsStatusMap_m;

	CORBA::ULong timeout_m;
	CORBA::ULong numberOfFlows;
	CORBA::ULong offset;

	ContainerServices *contSvc_p;
    };
}

#include "bulkDataDistributer.i"

#endif /* _BULKDATA_DISTRIBUTER_H */
