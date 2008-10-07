#ifndef _BULKDATA_SENDER_DISTR_IMPL_H
#define _BULKDATA_SENDER_DISTR_IMPL_H
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

#include "bulkDataSenderDistrS.h"

#include "bulkDataSenderImpl.h"

#include "bulkDataSenderDefaultCb.h"


/** @file bulkDataSenderDistrImpl.h 
 */

/** @defgroup BULKDATASENDERDISTRIMPLDOC Bulk Data Sender Distr Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSenderDistrImpl.h implements the BulkDataSenderDistr interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderDistrImpl.html">Bulk Data Sender Distr Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

template<class TSenderCallback>
class BulkDataSenderDistrImpl : public virtual BulkDataSenderImpl<TSenderCallback>,
				public virtual POA_bulkdatadistr::BulkDataSenderDistr
{    
  public:
    /**
     * Constructor
     * @param poa poa which will activate this and also all other components 
     * @param name component name
     */
    BulkDataSenderDistrImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataSenderDistrImpl();
  
    void cleanUp();

    /**
     * @throw ACSBulkDataError::AVStartSendErrorEx
    */
    virtual void startSend();

    /**
     * @throw ACSBulkDataError::AVPaceDataErrorEx
    */
    virtual void paceData();

    /**
     * @throw ACSBulkDataError::AVStopSendErrorEx
    */
    virtual void stopSend();
};

#include "bulkDataSenderDistrImpl.i"

#endif /* _BULKDATA_SENDER_DISTR_IMPL_H */
