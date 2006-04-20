#ifndef _BULKDATA_SENDER_PERF_H
#define _BULKDATA_SENDER_PERF_H

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
 * oat       20/04/06  created 
 */



#include "bulkDataSenderPerfS.h"
#include "bulkDataSenderImpl.h"
#include "bulkDataSenderDefaultCb.h"

/** @defgroup BULKDATASENDERPERFIMPLDOC Bulk Data Sender Perf Impl
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataSenderPerfImpl.h implements the BulkDataSenderPerf interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataSenderEx2Impl.html">Bulk Data Sender Perf Impl Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */

template<class TSenderCallback>
class BulkDataSenderPerfImpl : public virtual BulkDataSenderImpl<TSenderCallback>,
		   public virtual POA_bulkdata::BulkDataSenderPerf
{    
  public:
    BulkDataSenderPerfImpl(const ACE_CString& name,ContainerServices* containerServices);
  
    virtual ~BulkDataSenderPerfImpl();
  
    void cleanUp();

    virtual void startSend()
	throw (CORBA::SystemException, AVStartSendErrorEx);

    virtual void paceData()
	throw (CORBA::SystemException, AVPaceDataErrorEx);

    virtual void stopSend()
	throw (CORBA::SystemException, AVStopSendErrorEx);
};

#endif
