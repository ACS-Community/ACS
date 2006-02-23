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

	typedef ACE_Hash_Map_Manager <ACE_CString, BulkDataSender<TSenderCallback> *,ACE_Null_Mutex>  Sender_Map;
	typedef ACE_Hash_Map_Entry <ACE_CString, BulkDataSender<TSenderCallback> * > Sender_Map_Entry;
	typedef ACE_Hash_Map_Iterator <ACE_CString, BulkDataSender<TSenderCallback> * ,ACE_Null_Mutex>  Sender_Map_Iterator;
	
      public:
      
	/**
	 * Constructor
	 */
	BulkDataDistributer();
      
	/**
	 * Destructor
	 */
	virtual ~BulkDataDistributer();


	virtual void multiConnect(bulkdata::BulkDataReceiverConfig *recvConfig_p, const char *fepsConfig, const ACE_CString& receiverName);

	virtual void multiDisconnect(const ACE_CString& receiverName);

	virtual BulkDataReceiver<TReceiverCallback> * getReceiver() 
	    {
		return & receiver_m;
	    }

	virtual Sender_Map * getSenderMap() 
	    {
		return & senderMap_m;
	    }

	virtual bool isRecvConnected (const ACE_CString& receiverName);


	virtual void distSendStart (ACE_CString& flowName);

	virtual int distSendData (ACE_CString& flowName, ACE_Message_Block * frame_p);

	virtual void distSendStop (ACE_CString& flowName);


      private:


	BulkDataSender<TSenderCallback> *sender_p;
	
	BulkDataReceiver<TReceiverCallback> receiver_m;

	Sender_Map senderMap_m;

    };
}

#include "bulkDataDistributer.i"

#endif /* _BULKDATA_DISTRIBUTER_H */
