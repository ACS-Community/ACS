#ifndef _BULKDATA_SENDER_IMPL_H
#define _BULKDATA_SENDER_IMPL_H
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

#include "bulkDataSenderS.h"
#include "bulkDataReceiverC.h"

#include "bulkDataSender.h"


/** @file bulkDataSenderImpl.h 
 */

/** @defgroup BULKDATASENDERIMPLDOC Bulk Data Sender Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSenderImpl.h implements the BulkDataSender interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderImpl.html">Bulk Data Sender Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

template<class TSenderCallback=BulkDataSenderDefaultCallback>
class BulkDataSenderImpl : public baci::CharacteristicComponentImpl,
			   public virtual POA_bulkdata::BulkDataSender
{    
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components 
     * @param name component name
     */
    BulkDataSenderImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataSenderImpl();
  

    void cleanUp();

    /**
     *  Negotiate and initialize connection with the Sender object.
     *  @param receiver reference of the Receiver Component.
     *  @throw ACSBulkDataError::AVConnectErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void connect(bulkdata::BulkDataReceiver_ptr receiverObj_p);

    /**
     *  @throw ACSBulkDataError::AVDisconnectErrorEx
    */
    virtual void disconnect();


    virtual AcsBulkdata::BulkDataSender<TSenderCallback> *getSender() 
	{
	    return &sender;
	}


    /** 
     *  Calls the Receiver handle_start() method once the connection is established.
     *  @throw ACSBulkDataError::AVStartSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void startSend()  =0;

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
    virtual void paceData()  =0;

    /** 
     *  Calls the Receiver handle_stop() method.
     *  @throw ACSBulkDataError::AVStopSendErrorEx
     *  @return void
     *  @htmlonly
     <br><hr>
     @endhtmlonly
    */
    virtual void stopSend() =0;



  protected:

    /** 
     *  Pointer to the dataProtocol on which the send_frame(...) method
     *  is called in order to actually send data.
     */
 
 
  private:

    maci::ContainerServices *containerServices_p;

    AcsBulkdata::BulkDataSender<TSenderCallback> sender;

    bulkdata::BulkDataReceiver_ptr receiverObj_m;
};

typedef BulkDataSenderImpl<> BulkDataSenderDefaultImpl;

#include "bulkDataSenderImpl.i"

#endif /* _BULKDATA_SENDER_IMPL_H */
