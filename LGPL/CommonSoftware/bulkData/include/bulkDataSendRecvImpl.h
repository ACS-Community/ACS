#ifndef _BULKDATA_SENDRECV_IMPL_H
#define _BULKDATA_SENDRECV_IMPL_H
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
 * oat       28/01/05  created 
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <baci.h>
#include <baciCharacteristicComponentImpl.h>
#include <maciHelper.h>
#include <maciContainerServices.h>

#include "bulkDataSendRecvS.h"

#include "bulkDataReceiverC.h"

#include "bulkDataReceiver.h"
#include "bulkDataSender.h"

#include "bulkDataSenderDefaultCb.h"

#include "ACSBulkDataStatus.h"

using namespace baci;
using namespace maci;
using namespace ACSBulkDataStatus;

/** @file bulkDataSendRecvImpl.h 
 */

/** @defgroup BULKDATASENDERIMPLDOC Bulk Data SendRecv Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSendRecvImpl.h implements the BulkDataSendRecv interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSendRecvImpl.html">Bulk Data SendRecv Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

template<class TReceiverCallback, class TSenderCallback = BulkDataSenderDefaultCallback>
class BulkDataSendRecvImpl : public virtual CharacteristicComponentImpl,
			     public POA_bulkdata::BulkDataSendRecv
{    
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components 
     * @param name component name
     */
    BulkDataSendRecvImpl(const ACE_CString& name,ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataSendRecvImpl();
  

    void cleanUp();


    /***************************** Sender part ****************************/


    /**
     *  Negotiate and initialize connection with the SendRecv object.
     *  @param receiver reference of the Receiver Component.
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connect(bulkdata::BulkDataReceiver_ptr receiverObj_p)
	throw (CORBA::SystemException, AVConnectErrorEx);

    virtual void disconnect()
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

    /*************************** Receiver part **********************/

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

    virtual ACSErr::Completion *getCbStatus(CORBA::ULong flowNumber) 
	throw (CORBA::SystemException, AVInvalidFlowNumberEx)
	{
	    ACS_TRACE("BulkDataSendRecvImpl::getCbStatus");

	    AVCbReadyCompletion *comp = new AVCbReadyCompletion();
	    
	    return comp->returnCompletion();
	}

    virtual void setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout) 
	throw (CORBA::SystemException, AVInvalidFlowNumberEx)
	{
	}

    virtual void setRecvName(const char *recvName) 
	throw (CORBA::SystemException, AVSetReceiverNameErrorEx)
	{
	}

  private:

    AcsBulkdata::BulkDataSender<TSenderCallback> sender;

    AcsBulkdata::BulkDataReceiver<TReceiverCallback> receiver;

    ContainerServices *containerServices_p;

    CDB::DAL_ptr dal_p;

    CDB::DAO_ptr dao_p;
};


#include "bulkDataSendRecvImpl.i"

#endif /* _BULKDATA_SENDRECV_IMPL_H */
