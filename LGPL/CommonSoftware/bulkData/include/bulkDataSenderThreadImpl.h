#ifndef _BULKDATA_SENDER_THREAD_IMPL_H_
#define _BULKDATA_SENDER_THREAD_IMPL_H_
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
 * oat       17/03/08  created 
 */

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include "bulkDataSenderDistrS.h"

#include "bulkDataSenderImpl.h"

#include "bulkDataSenderDefaultCb.h"

#include <acsThread.h>

#include <vector>

/** @file bulkDataSenderThreadImpl.h 
 */

/** @defgroup BULKDATASENDERTHREADIMPLDOC Bulk Data Sender Thread Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSenderThreadImpl.h implements the BulkDataSenderDistr interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderThreadImpl.html">Bulk Data Sender Thread Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */


//forward declaration
class BulkDataSenderThreadImpl;

class SenderThread : public ACS::Thread
{ 
  public:
    SenderThread(const ACE_CString& name, 
		 BulkDataSenderThreadImpl *sender,
		 CORBA::ULong flowNumber,
		 const ACS::TimeInterval& responseTime=ThreadBase::defaultResponseTime, 
		 const ACS::TimeInterval& sleepTime=ThreadBase::defaultSleepTime);

    ~SenderThread();

    virtual void run();

  private:

    BulkDataSenderThreadImpl *sender_p;

    ACE_Message_Block *mb_p;

    CORBA::ULong flowNumber_m;
};



class BulkDataSenderThreadImpl : public virtual BulkDataSenderDefaultImpl,
				 public virtual POA_bulkdatadistr::BulkDataSenderDistr
{    
  public:

    BulkDataSenderThreadImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    virtual ~BulkDataSenderThreadImpl();

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

  private:

    CORBA::ULong numberOfFlows;
    std::vector<SenderThread*> thread_p;
};

#endif /*  _BULKDATA_SENDER_THREAD_IMPL_H_ */
