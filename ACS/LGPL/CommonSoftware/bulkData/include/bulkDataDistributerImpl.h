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

#include <ace/Pair_T.h>

#include "bulkDataDistributerS.h"
#include "bulkDataDistributer.h"

//#include "bulkDataReceiverS.h"

#include "ACSBulkDataStatus.h"

//forward declaration
template<class TReceiverCallback, class TSenderCallback>
class BulkDataDistributerNotifCb;

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
class BulkDataDistributerImpl : public baci::CharacteristicComponentImpl,
				public virtual POA_bulkdata::BulkDataDistributer
{


    typedef ACE_Pair< AcsBulkdata::RecvData /*bulkdata::BulkDataReceiver_ptr*/, AcsBulkdata::BulkDataSender<TSenderCallback> *> Sender_Map_Pair;

    typedef ACE_Hash_Map_Manager <ACE_CString, Sender_Map_Pair, ACE_Null_Mutex>  Sender_Map;
    typedef ACE_Hash_Map_Entry <ACE_CString, Sender_Map_Pair > Sender_Map_Entry;
    typedef ACE_Hash_Map_Iterator <ACE_CString, Sender_Map_Pair ,ACE_Null_Mutex>  Sender_Map_Iterator;

  public:
    
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other Components. 
     * @param name component name.
     */
    BulkDataDistributerImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataDistributerImpl();

    /*
    *  @throw ACSErr::ACSbaseExImpl
    */
    virtual void initialize();

    virtual void cleanUp();


/********************* Sender part ********************/

    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiverObj_p reference of the Receiver Component.
     *  @throw ACSBulkDataError::AVConnectErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connect(bulkdata::BulkDataReceiver_ptr receiverObj_p);


    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiverObj_p reference of the Receiver Component.
     *  @throw ACSBulkDataError::AVConnectErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void multiConnect(bulkdata::BulkDataReceiver_ptr receiverObj_p);

    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiverName_p name of the Receiver Component.
     *  @throw ACSBulkDataError::AVConnectErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connectByName(const char *receiverName_p);

    /*
     *  @throw ACSBulkDataError::AVDisconnectErrorEx 
    */
    virtual void disconnect();
	
    /*
     *  @throw ACSBulkDataError::AVDisconnectErrorEx 
    */
    virtual void multiDisconnect(bulkdata::BulkDataReceiver_ptr receiverObj_p);

    /*
     *  @throw ACSBulkDataError::AVDisconnectErrorEx 
    */
    virtual void disconnectByName(const char *receiverName_p);


    /** 
     *  Calls the Receiver handle_start() method once the connection is established.
     *  @throw ACSBulkDataError::AVStartSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
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

/************************ Receiver part ********************/

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

    void openReceiverStream(const char * stream_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::openReceiverStream NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverStreamCfg (const char * stream_name,  const char * stream_cfg)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::openReceiverStreamCfg NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverFlow (const char * stream_name, const char * flow_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::openReceiverFlow NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverFlowCfg (const char * stream_name, const char * flow_name, const char * flow_cfg)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::openReceiverFlowCfg NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    /*
     *  @throw ACSBulkDataError::AVReceiverConfigErrorEx 
     */
    bulkdata::BulkDataReceiverConfig * getReceiverConfig();
    
    /*
     *  @throw ACSBulkDataError::AVCloseReceiverErrorEx
     */
    virtual void closeReceiver(); 


    void closeReceiverStream(const char * stream_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataDistributerImpl::closeReceiverStream NOT implemented"));
    }

    /*
     *  @throw ACSBulkDataError::AVSetReceiverErrorEx
     */
    virtual void setReceiver(const bulkdata::BulkDataReceiverConfig &receiverConfig);

    //protected:

    virtual AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback> *getDistributer() 
	{
	    return &distributer;
	}

    /*
     *  @throw ACSBulkDataError::AVInvalidFlowNumberEx 
     *  @throw ACSBulkDataError::AVFlowEndpointErrorEx
     */
    virtual ACSErr::Completion *getCbStatus(CORBA::ULong flowNumber);

    virtual ACSErr::Completion *getReceiverCbStatus(const char *recvName, CORBA::ULong flowNumber); 

    /*
     *  @throw ACSBulkDataError::AVInvalidFlowNumberEx
     */
    virtual void setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout) 
	{
	    //empty
	}

    /*
     *  @throw ACSBulkDataError::AVSetReceiverNameErrorEx
     */
    virtual void setRecvName(const char *recvName) 
	{
	    //empty
	}

    /*
     *  @throw ACSBulkDataError::AVNotificationMechanismErrorEx
     */
    virtual void subscribeNotification(ACS::CBvoid_ptr notifCb);

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

    maci::ContainerServices *containerServices_p;
    CDB::DAL_ptr dal_p;

    AcsBulkdata::BulkDataDistributer<TReceiverCallback, TSenderCallback> distributer;

    void rmEntryFromSenderMap(bulkdata::BulkDataReceiver_ptr receiverObj_p);
    void rmEntryFromSenderMap(const char *receiverName_p);
};



#include "bulkDataDistributerImpl.i"

#endif /*!_BULKDATA_DISTRIBUTER_IMPL_H*/
