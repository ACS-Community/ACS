#ifndef logging_acsstructuredpushsupplier_H
#define logging_acsstructuredpushsupplier_H
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
 * "@(#) $Id: loggingACSStructuredPushSupplier.h,v 1.2 2006/07/18 16:52:43 sharring Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * msekoran  2001-06-17  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acsutil.h>

#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include <basencSupplier.h>

class ACSStructuredPushSupplier : public BaseSupplier
{
    // = TITLE
    //   StructuredPushSupplier
    //
    // = DESCRIPTION
    //   Supplier
    //
  public:
    // = Initialization and Termination code
    ACSStructuredPushSupplier ();
    // Constructor.
    
    //void connect (CosNotifyChannelAdmin::SupplierAdmin_ptr supplier_admin);
    // Connect the Supplier to the EventChannel.
    // Creates a new proxy supplier and connects to it.
    
    //void disconnect ();
    // Disconnect from the supplier.
    
    virtual void send_event (const CosNotification::StructuredEvent& event);
    // Send one event.
    
    
  protected:
/*
    // = Data members
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxy_consumer_;
    // The proxy that we are connected to.
    
    CosNotifyChannelAdmin::ProxyID proxy_consumer_id_;
    // This supplier's id.
*/
    
    virtual ~ACSStructuredPushSupplier ();
    // Destructor

	/**
 	 * Overridden.
	 */
	virtual const char* getChannelKind() 
	{ 
		return acscommon::LOGGING_CHANNEL_KIND; 
	}

	virtual const char* getEventType()
	{ 
		return acscommon::LOGGING_TYPE;
	}

	virtual const char* getChannelDomain()
	{
		return acscommon::LOGGING_DOMAIN;
	}

	virtual const char* getNotificationFactoryName()
	{
		return acscommon::LOGGING_NOTIFICATION_FACTORY_NAME;
	}

};

#endif
