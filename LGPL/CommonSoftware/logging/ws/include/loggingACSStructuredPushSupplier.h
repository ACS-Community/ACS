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
 * "@(#) $Id: loggingACSStructuredPushSupplier.h,v 1.6 2008/09/29 08:36:42 cparedes Exp $"
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

/**
 * NOTE: This class contains quite a bit of duplicated code that could be reduced if it inherited
 * from the basencSupplier class - however, due to build order dependency issues, this is not possible.
 */
class ACSStructuredPushSupplier : public POA_CosNotifyComm::StructuredPushSupplier,
				  public PortableServer::RefCountServantBase
{
    // = TITLE
    //   StructuredPushSupplier
    //
    // = DESCRIPTION
    //   Supplier
    //
  public:
    // = Initialization and Termination code
    ACSStructuredPushSupplier (void);
    // Constructor.
    
    void connect (CosNotifyChannelAdmin::SupplierAdmin_ptr supplier_admin);
    // Connect the Supplier to the EventChannel.
    // Creates a new proxy supplier and connects to it.
    
    void disconnect ();
    // Disconnect from the supplier.
    
    virtual void send_event (const CosNotification::StructuredEvent& event);
    // Send one event.
    
    
    // = NotifySubscribe
    /*
    * = NotifySubscribe
    * \throw CosNotifyComm::InvalidEventType
    */
    virtual void subscription_change (const CosNotification::EventTypeSeq & added,
				      const CosNotification::EventTypeSeq & removed);
    
    // = StructuredPushSupplier method
    virtual void disconnect_structured_push_supplier ();

  protected:
    // = Data members
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxy_consumer_;
    // The proxy that we are connected to.
    
    CosNotifyChannelAdmin::ProxyID proxy_consumer_id_;
    // This supplier's id.
    
    // = Protected Methods
    virtual ~ACSStructuredPushSupplier ();
    // Destructor
    
};

#endif
