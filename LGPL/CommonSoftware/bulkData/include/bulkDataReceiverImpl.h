#ifndef _BULKDATA_RECEIVER_IMPL_H
#define _BULKDATA_RECEIVER_IMPL_H
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
 * oat       30/03/04  created 
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

#include "ACSBulkDataStatus.h"

#include "bulkDataReceiverS.h"

#include "bulkDataReceiver.h"


/** @file bulkDataReceiverImpl.h  
 */

/** @defgroup BULKDATARECEIVERIMPLDOC Bulk Data Receiver
 *  @{
 * @htmlonly
<hr size="2" width="100%">
<div align="left">
<h2>Description</h2>
bulkDataImpl.h implements the BulkDataReceiver interface
<br>
<br>
<h2>Links</h2>
<ul>
  <li><a href="classBulkDataReceiverImpl.html">Bulk Data Receiver Class Reference</a></li>
</ul>
</div>
   @endhtmlonly
 * @}
 */


template<class TCallback>
class BulkDataReceiverImpl : public baci::CharacteristicComponentImpl,
		             public virtual POA_bulkdata::BulkDataReceiver
{
  public:
    
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other Components. 
     * @param name component name.
     */
    BulkDataReceiverImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataReceiverImpl();

    void cleanUp();

    virtual AcsBulkdata::BulkDataReceiver<TCallback> * getReceiver()
	{
	    return & receiver;
	}

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
    virtual void openReceiver() ;

    void openReceiverStream(const char * stream_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::openReceiverStream NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverStreamCfg (const char * stream_name,  const char * stream_cfg)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::openReceiverStreamCfg NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverFlow (const char * stream_name, const char * flow_name)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::openReceiverFlow NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    void openReceiverFlowCfg (const char * stream_name, const char * flow_name, const char * flow_cfg)
    {
    	ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::openReceiverFlowCfg NOT implemented. The method is implemented just in bulkDataNT!"));
    }

    /**
     *  @throw ACSBulkDataError::AVReceiverConfigErrorEx
     */
    bulkdata::BulkDataReceiverConfig * getReceiverConfig();
    
    /**
     *  @throw ACSBulkDataError::AVCloseReceiverErrorEx
     */
    virtual void closeReceiver();

    virtual void closeReceiverStream(const char * stream_name);

    /**
     *  @throw ACSBulkDataError::AVInvalidFlowNumberEx
     *  @throw ACSBulkDataError::AVFlowEndpointErrorEx 
     */
    virtual ACSErr::Completion *getCbStatus(CORBA::ULong flowNumber);

    /**
     *  @throw ACSBulkDataError::AVInvalidFlowNumberEx 
     *  @throw ACSBulkDataError::AVFlowEndpointErrorEx
     */
    virtual void setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout);

    /**
     *  @throw ACSBulkDataError::AVSetReceiverNameErrorEx
     */
    virtual void setRecvName(const char *recvName);

    /**
     *  @throw ACSBulkDataError::AVNotificationMechanismErrorEx
     */
    virtual void subscribeNotification(ACS::CBvoid_ptr notifCb);

    /**
   	 *  Enable or disable that data are sent to the user's CB.
   	 *  By default this is enable.
   	 *  This operation has to be use with caution!!!
   	 *  @param enable true -> data will be sent to the user's CB,
   	 *                false -> data will *not* be sent to the user's CB,
   	 *  @return void
   	*/
    void fwdData2UserCB(CORBA::Boolean enable);

  protected: 

    
  private:

    AcsBulkdata::BulkDataReceiver<TCallback> receiver;

    maci::ContainerServices *containerServices_p;
};

#include "bulkDataReceiverImpl.i"

#endif /*!_BULKDATA_RECEIVER_IMPL_H*/
