#ifndef _BULKDATA_RECEIVER_DISTR2_IMPL_H
#define _BULKDATA_RECEIVER_DISTR2_IMPL_H
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

#include "bulkDataReceiverDistr2S.h"

#include "bulkDataReceiverImpl.h"

#include "bulkDataReceiverCbDistr2.h"

/** @file bulkDataReceiverDistr2Impl.h  
 */

/** @defgroup BULKDATARECEIVERDISTR2IMPLDOC Bulk Data Receiver Distr2
 *  @{
 * @htmlonly
 <hr size="2" width="100%">
 <div align="left">
 <h2>Description</h2>
 bulkDataImpl.h implements the BulkDataReceiverDistr2 interface
 <br>
 <br>
 <h2>Links</h2>
 <ul>
 <li><a href="classBulkDataReceiverDistr2Impl.html">Bulk Data Receiver Distr2 Class Reference</a></li>
 </ul>
 </div>
 @endhtmlonly
 * @}
 */


class BulkDataReceiverDistr2Impl : public virtual BulkDataReceiverImpl<BulkDataReceiverCbDistr2>,
				   public virtual POA_bulkdatadistr::BulkDataReceiverDistr2
{
  public:
    
    /**
     * Constructor
     * @param poa Poa which will activate this and also all other Components. 
     * @param name component name.
     */
    BulkDataReceiverDistr2Impl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    /**
     * Destructor
     */
    virtual ~BulkDataReceiverDistr2Impl();

    void cleanUp();
};

#endif /*!_BULKDATA_RECEIVER_DISTR2_IMPL_H*/
