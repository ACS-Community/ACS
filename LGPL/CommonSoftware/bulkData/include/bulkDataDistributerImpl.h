#ifndef _BULKDATA_DISTRIBUTER_IMPL_H
#define _BULKDATA_DISTRIBUTER_IMPL_H
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

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <baci.h>
#include <baciCharacteristicComponentImpl.h>
#include <maciHelper.h>
#include <maciContainerServices.h>

#include <Pair_T.h>

#include "bulkDataDistributerS.h"
#include "bulkDataDistributer.h"

//#include "bulkDataReceiverS.h"

#include "ACSBulkDataStatus.h"

using namespace baci;
using namespace maci;
using namespace ACSBulkDataStatus;
using namespace ACSBulkDataStatus;
using namespace ACSErrTypeCommon;

/** @file bulkDataDistributerImpl.h  
 */

/** @defgroup BULKDATADISTRIBUTERIMPLDOC Bulk Data Distributer
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
bulkDataImpl.h implements the BulkDataDistributer interface
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classBulkDataDistributerImpl.html">Bulk Data Distributer Class Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */


template<class TReceiverCallback, class TSenderCallback = BulkDataSenderDefaultCallback>
class BulkDataDistributerImpl : public CharacteristicComponentImpl,
				public virtual POA_bulkdata::BulkDataDistributer
{
    typedef ACE_Pair< bulkdata::BulkDataReceiver_ptr, AcsBulkdata::BulkDataSender<TSenderCallback> *> Sender_Map_Pair;

    typedef ACE_Hash_Map_Manager <ACE_CString, Sender_Map_Pair, ACE_Null_Mutex>  Sender_Map;
    typedef ACE_Hash_Map_Entry <ACE_CString, Sender_Map_Pair > Sender_Map_Entry;
    typedef ACE_Hash_Map_Iterator <ACE_CString, Sender_Map_Pair ,ACE_Null_Mutex>  Sender_Map_Iterator;

  public:
    
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other Components. 
     * @param name component name.
     */
    BulkDataDistributerImpl(const ACE_CString& name,ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataDistributerImpl();

    virtual void initialize()
	throw (ACSErr::ACSbaseExImpl);

    virtual void cleanUp();


/********************* Sender part ********************/

    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiverObj_p reference of the Receiver Component.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
	throw (CORBA::SystemException, AVConnectErrorEx);


    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiverObj_p reference of the Receiver Component.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void multiConnect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
	throw (CORBA::SystemException, AVConnectErrorEx);

    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiverName_p name of the Receiver Component.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connectByName(const char *receiverName_p)
	throw (CORBA::SystemException, AVConnectErrorEx);

    virtual void disconnect()
	throw (CORBA::SystemException, AVDisconnectErrorEx);

    virtual void multiDisconnect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
	throw (CORBA::SystemException, AVDisconnectErrorEx);

    virtual void disconnectByName(const char *receiverName_p)
	throw (CORBA::SystemException, AVDisconnectErrorEx);


    /** 
     *  Calls the Receiver handle_start() method once the connection is established.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void startSend()
	throw (CORBA::SystemException, AVStartSendErrorEx);

    /**
     *  Sends data to the Receiver calling the receive_frame() method on the Receiver side.
     *  This method must be overriden by the user to send his own data.
     *  @param size buffer size of the sent data.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void paceData()
	throw (CORBA::SystemException, AVPaceDataErrorEx);

    /** 
     *  Calls the Receiver handle_stop() method.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void stopSend()
	throw (CORBA::SystemException, AVStopSendErrorEx);


/************************ Receiver part ********************/

    /**
     *  Opens connection creating an out-of-bound channel using TAO A/V services.
     *  It creates the Receiver Stream End Point and Flow End Point for the
     *  connection with the Sender. The Receiver Stream End Point can be retrieved
     *  as an attribute. 
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void openReceiver() 
	throw (CORBA::SystemException, AVOpenReceiverErrorEx);

    bulkdata::BulkDataReceiverConfig * getReceiverConfig()
	throw (CORBA::SystemException, AVReceiverConfigErrorEx);
    
    virtual void closeReceiver() 
	throw (CORBA::SystemException, AVCloseReceiverErrorEx);

    virtual void setReceiver(const bulkdata::BulkDataReceiverConfig &receiverConfig) 
	throw (CORBA::SystemException, AVSetReceiverErrorEx);

    //protected:

    virtual AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback> *getDistributer() 
	{
	    return &distributer;
	}

    virtual ACSErr::Completion *getCbStatus(CORBA::ULong flowNumber) 
	throw (CORBA::SystemException, AVInvalidFlowNumberEx, AVFlowEndpointErrorEx);

    virtual ACSErr::Completion *getReceiverCbStatus(const char *recvName, CORBA::ULong flowNumber) 
	throw (CORBA::SystemException);

    virtual void setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout) 
	throw (CORBA::SystemException, AVInvalidFlowNumberEx)
	{
	    //empty
	}

    virtual void setRecvName(const char *recvName) 
	throw (CORBA::SystemException, AVSetReceiverNameErrorEx)
	{
	    //empty
	}

  private:

    ContainerServices *containerServices_p;
    CDB::DAL_ptr dal_p;

    AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback> distributer;
};


#include "bulkDataDistributerImpl.i"

#endif /*!_BULKDATA_DISTRIBUTER_IMPL_H*/
