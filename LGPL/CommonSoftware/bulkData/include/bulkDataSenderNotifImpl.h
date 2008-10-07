#ifndef _BULKDATA_SENDER_NOTIF_IMPL_H
#define _BULKDATA_SENDER_NOTIF_IMPL_H
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
 * oat       25/09/07  created 
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include "bulkDataSenderS.h"

#include "bulkDataSenderImpl.h"


/** @file bulkDataSenderNotifImpl.h 
 */

/** @defgroup BULKDATASENDERNOTIFIMPLDOC Bulk Data Sender Notif Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSenderNotifImpl.h implements the BulkDataSender interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderNotifImpl.html">Bulk Data Sender Notif Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

class BulkDataSenderNotifImpl : public virtual BulkDataSenderDefaultImpl
{    
 public:

    BulkDataSenderNotifImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
    
    virtual ~BulkDataSenderNotifImpl();
      
    /*
     * @throw ACSBulkDataError::AVStartSendErrorEx
     */
    virtual void startSend();
    
    /*
     * @throw ACSBulkDataError::AVPaceDataErrorEx
     */
    virtual void paceData ();
	
    /*
     * @throw ACSBulkDataError::AVStopSendErrorEx
     */
    virtual void stopSend();
};

#endif /* _BULKDATA_SENDER_NOTIF_IMPL_H */
