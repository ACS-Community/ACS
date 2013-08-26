#ifndef _ACS_NOTIFICATION_SERVICE_MONITOR_H_
#define _ACS_NOTIFICATION_SERVICE_MONITOR_H_

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
* "@(#) $Id: acsNotificationServiceMonitor.h,v 1.1 2009/06/12 13:32:14 msekoran Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>

// Carefully create the return value to avoid arithmetic overflow
#define GET_TIMESTAMP_NOW(X) \
  CORBA::ULongLong X; { \
  ACE_Time_Value const macro_now = ACE_OS::gettimeofday (); \
  X = \
   (static_cast<CORBA::ULongLong> (macro_now.sec ()) * (ACE_UINT32) 1000000 + \
    static_cast<CORBA::ULongLong> (macro_now.usec ())); \
  }

class PushSupplierImpl
: public POA_CosNotifyComm::PushSupplier
{
  virtual void disconnect_push_supplier(void) {}
  virtual void subscription_change(const CosNotification::EventTypeSeq&, const CosNotification::EventTypeSeq&) {}
};

class PushConsumerImpl
: public POA_CosNotifyComm::PushConsumer
{
public:
  PushConsumerImpl() : rtt(0) {};
  virtual void disconnect_push_consumer(void) {}
  virtual void offer_change(const CosNotification::EventTypeSeq&, const CosNotification::EventTypeSeq&) {}
  virtual void push(const CORBA::Any &data)
  {
    CORBA::ULongLong timestamp;
    data >>= timestamp;
    GET_TIMESTAMP_NOW(now);
    rtt = now - timestamp;
  }

  CORBA::ULongLong getAndResetRTT()
  {
    CORBA::ULongLong retVal = rtt;
    rtt = 0;
    return retVal;
  };

private:
  CORBA::ULongLong rtt;
};

class NotificationServiceMonitor 
{
public:
  NotificationServiceMonitor(CosNotifyChannelAdmin::EventChannelFactory_ptr factory);

  bool init(void);
  void destroy(void);

  bool issuePingEvent(void);

  CORBA::ULongLong getAndResetRTT() const { return pushConsumerImpl->getAndResetRTT(); };
protected:
  // = Data Members
  CosNotifyChannelAdmin::EventChannel_var ec_;
  // The one channel that we create using the factory.

  PushSupplierImpl* pushSupplierImpl;
  // Supplier

  PushConsumerImpl* pushConsumerImpl;
  // Consumer

  CosNotifyChannelAdmin::ProxyPushConsumer_var proxyConsumer;
  // Proxy Consumer

  CosNotifyChannelAdmin::EventChannelFactory_var notify_factory_;
  // Channel factory.
};


NotificationServiceMonitor::NotificationServiceMonitor(
  CosNotifyChannelAdmin::EventChannelFactory_ptr factory) : 
  notify_factory_(CosNotifyChannelAdmin::EventChannelFactory::_duplicate(factory))
{
}

void
NotificationServiceMonitor::destroy(void)
{
  try 
  {
    PortableServer::POA_var poa = pushConsumerImpl->_default_POA();
    PortableServer::ObjectId_var oid = poa->servant_to_id(pushConsumerImpl);
    poa->deactivate_object(oid.in()); // this will also dispose the object itself
    poa = pushSupplierImpl->_default_POA();
    oid = poa->servant_to_id(pushSupplierImpl);
    poa->deactivate_object(oid.in()); // this will also dispose the object itself
    
    if (!CORBA::is_nil(ec_))
      ec_->destroy();
  } catch (...) {
    // noop (e.g. service might be down, etc.)
  }
}

bool
NotificationServiceMonitor::init (void)
{
  
  // Create channel...  
  CosNotifyChannelAdmin::ChannelID id;
  CosNotification::QoSProperties initial_qos;
  CosNotification::AdminProperties initial_admin;
  ec_ = notify_factory_->create_channel (initial_qos,
                                         initial_admin,
                                         id);
  // Create admins...

  CosNotifyChannelAdmin::AdminID supplier_admin_id;
  CosNotifyChannelAdmin::SupplierAdmin_var supplier_admin_ =
    ec_->new_for_suppliers (CosNotifyChannelAdmin::OR_OP, supplier_admin_id);

  if (CORBA::is_nil (supplier_admin_.in ()))
    return false;

  CosNotifyChannelAdmin::AdminID consumer_admin_id;
  CosNotifyChannelAdmin::ConsumerAdmin_var consumer_admin_ =
    ec_->new_for_consumers (CosNotifyChannelAdmin::OR_OP, consumer_admin_id);

  if (CORBA::is_nil (consumer_admin_.in ()))
    return false;


  CosNotifyChannelAdmin::ProxyID proxy_id;
  CosNotifyChannelAdmin::ProxySupplier_var ps =
    consumer_admin_->obtain_notification_push_supplier(
          CosNotifyChannelAdmin::ANY_EVENT,
          proxy_id);

  CosNotifyChannelAdmin::ProxyPushSupplier_var anyps =
    CosNotifyChannelAdmin::ProxyPushSupplier::_narrow(ps.in());


  CosNotifyChannelAdmin::ProxyConsumer_var pc =
    supplier_admin_->obtain_notification_push_consumer(
          CosNotifyChannelAdmin::ANY_EVENT,
          proxy_id);

  proxyConsumer =
    CosNotifyChannelAdmin::ProxyPushConsumer::_narrow(pc.in());

  // Connect a PushConsumer and PushSupplier
  pushSupplierImpl = new PushSupplierImpl();
  CosNotifyComm::PushSupplier_var pushSupplier = pushSupplierImpl->_this();
  pushSupplierImpl->_remove_ref();
  pushConsumerImpl = new PushConsumerImpl(); 
  CosNotifyComm::PushConsumer_var  pushConsumer = pushConsumerImpl->_this();
  pushConsumerImpl->_remove_ref();
 
  proxyConsumer->connect_any_push_supplier(pushSupplier.in());
  anyps->connect_any_push_consumer(pushConsumer.in());

  return true;
}

bool
NotificationServiceMonitor::issuePingEvent()
{
  if (CORBA::is_nil(proxyConsumer.in()))
    return false;

  try
  {
    CORBA::Any data;
    GET_TIMESTAMP_NOW(ts);
    data <<= ts;
    proxyConsumer->push(data);

    return true;
  }
  catch (...) {
    return false;
  }
}

#endif
