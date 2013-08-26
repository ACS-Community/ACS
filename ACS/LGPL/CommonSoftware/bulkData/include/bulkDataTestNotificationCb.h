#ifndef _BULKDATA_TEST_NOTIFICATION_CB_H
#define _BULKDATA_TEST_NOTIFICATION_CB_H
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
 * oat       30/07/07  created 
 */

#include <baci.h>


// User notification callback
class BulkDataTestNotificationCb: public virtual POA_ACS::CBvoid
{
  public:

    BulkDataTestNotificationCb()
	{
	    ACS_TRACE("BulkDataTestNotificationCb::BulkDataTestNotificationCb");
	}

    ~BulkDataTestNotificationCb()
	{
	    ACS_TRACE("BulkDataTestNotificationCb::~BulkDataTestNotificationCb");
	}

    void working(const Completion &comp, const ACS::CBDescOut &desc) 
	{
	    ACS_TRACE("BulkDataNotificationCb::working");
	}
	
    void done(const Completion &comp, const ACS::CBDescOut &desc) 
	{
	    ACS_TRACE("BulkDataNotificationCb::done");

	    CompletionImpl complImp = comp;
	    complImp.log();
	}

    CORBA::Boolean negotiate (ACS::TimeInterval timeToTransmit, const ACS::CBDescOut &desc) 
	{
	    ACS_TRACE("BulkDataNotificationCb::negotiate");

	    return true;
	}
};   

#endif /* _BULKDATA_TEST_NOTIFICATION_CB_H */
