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


using namespace baci;
using namespace maci;
using namespace ACSBulkDataStatus;

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
class BulkDataReceiverImpl : public CharacteristicComponentImpl,
		             public virtual POA_bulkdata::BulkDataReceiver
{
  public:
    
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other Components. 
     * @param name component name.
     */
    BulkDataReceiverImpl(const ACE_CString& name,ContainerServices* containerServices);
  
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
	throw (CORBA::SystemException, AVInvalidFlowNumberEx, AVFlowEndpointErrorEx);

    virtual void setTimeout(CORBA::ULong flowNumber, CORBA::ULong timeout) 
	throw (CORBA::SystemException, AVInvalidFlowNumberEx, AVFlowEndpointErrorEx);

    virtual void setRecvName(const char *recvName) 
	throw (CORBA::SystemException, AVSetReceiverNameErrorEx);

    virtual void subscribeNotification(ACS::CBvoid_ptr notifCb)
	throw (CORBA::SystemException, AVNotificationMechanismErrorEx);

  protected: 

    
  private:

    AcsBulkdata::BulkDataReceiver<TCallback> receiver;

    ContainerServices *containerServices_p;
};

#include "bulkDataReceiverImpl.i"

#endif /*!_BULKDATA_RECEIVER_IMPL_H*/
