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
class BulkDataSendRecvImpl : public virtual baci::CharacteristicComponentImpl,
			     public POA_bulkdata::BulkDataSendRecv
{    
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components 
     * @param name component name
     */
    BulkDataSendRecvImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataSendRecvImpl();
  

    void cleanUp();


    /***************************** Sender part ****************************/


    /**
     *  Negotiate and initialize connection with the SendRecv object.
     *  @param receiver reference of the Receiver Component.
     *  @throw ACSBulkDataError::AVConnectErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connect(bulkdata::BulkDataReceiver_ptr receiverObj_p);

    void openReceiverStream(const char * stream_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataSendRecvImpl<>::openReceiverStream NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverStreamCfg (const char * stream_name,  const char * stream_cfg)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataSendRecvImpl<>::openReceiverStreamCfg NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverFlow (const char * stream_name, const char * flow_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataSendRecvImpl<>::openReceiverFlow NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverFlowCfg (const char * stream_name, const char * flow_name, const char * flow_cfg)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataSendRecvImpl<>::openReceiverFlowCfg NOT implemented. The method is implemented just in bulkDataNT!"));
    }


    /**
     * @throw ACSBulkDataError::AVDisconnectErrorEx
     */
    virtual void disconnect();

    /** 
     *  Calls the Receiver handle_start() method once the connection is established.
     *  @throw ACSBulkDataError::AVStartSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */

    void closeReceiverStream(const char * stream_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataSendRecvImpl<>::closeReceiverStream NOT implemented"));
    }

    virtual void startSend();

    /**
     *  Sends data to the Receiver calling the receive_frame() method on the Receiver side.
     *  This method must be overriden by the user to send his own data.
     *  @param size buffer size of the sent data.
     *  @throw ACSBulkDataError::AVPaceDataErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void paceData();

    /** 
     *  Calls the Receiver handle_stop() method.
     *  @throw ACSBulkDataError::AVStopSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void stopSend();

    /*************************** Receiver part **********************/

    /**
     *  Opens connection creating an out-of-bound channel using TAO A/V services.
     *  It creates the Receiver Stream End Point and Flow End Point for the
     *  connection with the Sender. The Receiver Stream End Point can be retrieved
     *  as an attribute. 
     *  @throw ACSBulkDataError::AVOpenReceiverErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void openReceiver(); 

    /**
     *  @throw ACSBulkDataError::AVReceiverConfigErrorEx
     */
    bulkdata::BulkDataReceiverConfig * getReceiverConfig();

    /**
     *  @throw ACSBulkDataError::AVCloseReceiverErrorEx
     */
    virtual void closeReceiver();

    /**
     *  @throw ACSBulkDataError::AVInvalidFlowNumberEx
     */
    virtual ACSErr::Completion *getCbStatus(CORBA::ULong flowNumber) 
	{
	    ACS_TRACE("BulkDataSendRecvImpl::getCbStatus");

	    ACSBulkDataStatus::AVCbReadyCompletion *comp = new ACSBulkDataStatus::AVCbReadyCompletion();
	    
	    return comp->returnCompletion();
	}

    /**
     *  @throw ACSBulkDataError::AVInvalidFlowNumberEx
     */
    virtual void setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout)
	{
	}

    /**
     *  @throw ACSBulkDataError::AVSetReceiverNameErrorEx
     */
    virtual void setRecvName(const char *recvName)
	{
	}

    /**
     *  @throw ACSBulkDataError::AVNotificationMechanismErrorEx
     */
    virtual void subscribeNotification(ACS::CBvoid_ptr notifCb)
	{
	}

    /**
     *   NOT implemented for distributor
       		 <br><hr>
       		 @endhtmlonly
     */
    void fwdData2UserCB(CORBA::Boolean enable)
    {
    	ACS_SHORT_LOG((LM_WARNING,"BulkDataDistributerImpl<>::fwdData2UserCB not implemnted!"));
    }
  private:

    AcsBulkdata::BulkDataSender<TSenderCallback> sender;

    AcsBulkdata::BulkDataReceiver<TReceiverCallback> receiver;

    maci::ContainerServices *containerServices_p;

    CDB::DAL_ptr dal_p;

    CDB::DAO_ptr dao_p;
};


#include "bulkDataSendRecvImpl.i"

#endif /* _BULKDATA_SENDRECV_IMPL_H */
