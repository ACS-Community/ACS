#ifndef _BULKDATA_RECEIVER1_PERF_IMPL_H
#define _BULKDATA_RECEIVER1_PERF_IMPL_H

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

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataReceiver1PerfS.h"
#include "bulkDataReceiverImpl.h"

/** @file bulkDataReceiver1PerfImpl.h  
 */

template<class TCallback>
class BulkDataReceiver1PerfImpl : public virtual BulkDataReceiverImpl<TCallback>,
		      public virtual POA_bulkdata::BulkDataReceiver1Perf
{
  public:
    
    BulkDataReceiver1PerfImpl(const ACE_CString& name,maci::ContainerServices* containerServices);
  
    virtual ~BulkDataReceiver1PerfImpl();

    void cleanUp();
};

#include "bulkDataReceiver1PerfImpl.i"

#endif
