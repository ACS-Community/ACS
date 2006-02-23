/*******************************************************************************
*     ALMA - Atacama Large Millimiter Array
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
* "@(#) $Id: loggingACSStructuredPushSupplier.cpp,v 1.1 2005/09/09 21:33:45 dfugate Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/


#include "loggingACSStructuredPushSupplier.h"

/*****************************************************************/

ACSStructuredPushSupplier::ACSStructuredPushSupplier (void)
{
}

ACSStructuredPushSupplier::~ACSStructuredPushSupplier ()
{
}

void
ACSStructuredPushSupplier::connect (CosNotifyChannelAdmin::SupplierAdmin_ptr supplier_admin
				    )
{
  CosNotifyComm::StructuredPushSupplier_var objref =
    this->_this ();
  

  CosNotifyChannelAdmin::ProxyConsumer_var proxyconsumer =
    supplier_admin->obtain_notification_push_consumer (CosNotifyChannelAdmin::STRUCTURED_EVENT, 
						       proxy_consumer_id_);
  

  ACE_ASSERT (!CORBA::is_nil (proxyconsumer.in ()));

  // narrow
  this->proxy_consumer_ =
    CosNotifyChannelAdmin::StructuredProxyPushConsumer::_narrow (proxyconsumer.in ());
  

  ACE_ASSERT (!CORBA::is_nil (proxy_consumer_.in ()));

  proxy_consumer_->connect_structured_push_supplier (objref.in ()
                                                     );
  
}

void
ACSStructuredPushSupplier::disconnect ()
{
  ACE_ASSERT (!CORBA::is_nil (this->proxy_consumer_.in ()));
  this->proxy_consumer_->disconnect_structured_push_consumer();
}

void
ACSStructuredPushSupplier::subscription_change (const CosNotification::EventTypeSeq & /*added*/,
						const CosNotification::EventTypeSeq & /*removed */)
  throw (CORBA::SystemException,
         CosNotifyComm::InvalidEventType)
{
  //No-Op.
}

void
ACSStructuredPushSupplier::send_event (const CosNotification::StructuredEvent& event
				       )
{
  ACE_ASSERT (!CORBA::is_nil (this->proxy_consumer_.in ()));

  proxy_consumer_->push_structured_event (event);
}

void
ACSStructuredPushSupplier::disconnect_structured_push_supplier ()
  throw (CORBA::SystemException)
{
  // No-Op.
}

/*****************************************************************/
