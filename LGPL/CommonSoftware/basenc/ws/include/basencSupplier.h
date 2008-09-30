#ifndef basenc_supplier_H
#define basenc_supplier_H
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
 * "@(#) $Id: basencSupplier.h,v 1.6 2008/09/30 09:34:43 cparedes Exp $"
 *
 * who       when        what
 * --------  ----------  ----------------------------------------------
 * dfugate  2005-11-10  created
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

/** @file basencSupplier.h
 * Provides base class declaration for publishing events.
 */

#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include <orbsvcs/CosNamingC.h>
#include <acsncS.h>
#include "basencHelper.h"

/**
 * Abstract baseclass designed to send structured events out.
 */
class BaseSupplier : public POA_acsnc::OSPushSupplier,
		     public PortableServer::RefCountServantBase,
		     public BaseHelper
{
  public:
    
    /**
     * Standard constructor.
     * @param channelName Name of the channel to use.
     * @param notifyServiceDomainName Name of the notification service domain name used to determine notification service.
     */
    BaseSupplier(const char* channelName, const char* notifyServiceDomainName = 0);
    
    /**
     * Overridden.
     */ 
    virtual void 
    disconnect();
    
    /**
     * Overridden. This object will not function until this method has
     * been invoked.
     */
    void
    init(CosNaming::NamingContext_ptr nc_p);
    
    //--CORBA methods defined here-----------------------------------------
    /**
     * CORBA method we do not really care about from this class
     * other than the fact that it must be overridden.
     * @throw CosNotifyComm::InvalidEventType
     */
    virtual void 
    subscription_change(const CosNotification::EventTypeSeq &added,
			const CosNotification::EventTypeSeq &removed);
    
    /**
     * CORBA method we do not really care about from this class
     * other than the fact that it must be overridden.
     */
    virtual void 
    disconnect_structured_push_supplier();
    //----------------------------------------------------------------------
  protected:
    /**
     * Destructor
     */
    virtual ~BaseSupplier();
    
    /**
     * Send an entire structured event. Subclasses should create their own
     * structured event and then call this method to do the real publishing
     * of events.
     * @param event A CORBA StructuredEvent
     */
    void 
    publishEvent(const CosNotification::StructuredEvent& event);

    /**
     * Developer's responsibility to call this method before sending
     * any event.
     */
    virtual void
    populateHeader(CosNotification::StructuredEvent& event);
    
    
    /**
     * This method returns a constant character pointer to the type of event.
     * @return pointer to a constant string.
     */
    virtual const char*
    getEventType() = 0;

    /**
     * This method returns a constant character pointer to the type of event.
     * @return pointer to a constant string.
     */
    virtual const char*
    getEventName()
	{ return ""; }


    
    

    

    /**
     * Utility method only used by Supplier and should not
     * be called by your code directly. Provided just in case
     * someone wants to create this object's underlying CORBA reference
     * using other libraries/POAs/etc
     * @return CORBA reference to ourself
     */
    virtual acsnc::OSPushSupplier_ptr
    getCORBARef();

    

  private:
    /**
     * Connect the Supplier to the EventChannel.
     * @param supplier_admin A supplier admin object which we can connect
     * this instance of BaseSupplier to.
     */
    void 
    connect();
    
    /** 
     *  Supplier Admin object is responsible for creating & managing proxy consumers
     *  w/ a common set of QoS property settings & filter objects. 
     */
    CosNotifyChannelAdmin::SupplierAdmin_var supplierAdmin_m;

    /**
     * The proxy that we are connected to.
     */
    CosNotifyChannelAdmin::StructuredProxyPushConsumer_var proxyConsumer_m;
    
    /**
     * This supplier's id.
     */
    CosNotifyChannelAdmin::ProxyID proxyConsumerID_m;

    

    /**
     * Supplier admin ID.
     */
     CosNotifyChannelAdmin::AdminID adminID_m;

    /**
     * CORBA reference to myself.
     */
    acsnc::OSPushSupplier_var corbaRef_m;
};

#endif
