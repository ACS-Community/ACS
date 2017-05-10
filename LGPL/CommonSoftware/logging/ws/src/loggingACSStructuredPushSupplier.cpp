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
* "@(#) $Id: loggingACSStructuredPushSupplier.cpp,v 1.6 2008/09/29 08:36:42 cparedes Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/


#include "loggingACSStructuredPushSupplier.h"
#include "loggingACEMACROS.h"
#include <iostream>
#include <loggingService.h>

/*****************************************************************/

ACSStructuredPushSupplier::ACSStructuredPushSupplier (void)
: m_service(NULL), m_numLostLogs(0)
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
{
  //No-Op.
}


void 
ACSStructuredPushSupplier::set_logging_service(LoggingService *service)
{
    m_service = service;
}

void 
ACSStructuredPushSupplier::update_num_lost_logs(const CosNotification::StructuredEvent& event)
{
	const char * xmlLog;
    Logging::XmlLogRecordSeq *xmlSeq;
	if(event.remainder_of_body >>= xmlLog)
    {
        ++m_numLostLogs;
    } else if(event.remainder_of_body >>= xmlSeq) {
        m_numLostLogs += xmlSeq->length();
    }
}

void
ACSStructuredPushSupplier::send_event (const CosNotification::StructuredEvent& event
				       )
{
  ACE_ASSERT (!CORBA::is_nil (this->proxy_consumer_.in ()));

  try
    {
        proxy_consumer_->push_structured_event (event);
        if(m_numLostLogs > 0)
        {
            // Create new log with the number of logs lost
            ACS_SHORT_LOG((LM_INFO, "%d logs have been lost", m_numLostLogs));
            m_numLostLogs = 0;
        }
    }
  catch(CORBA::COMM_FAILURE ex)
	{
    update_num_lost_logs(event);
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;
	
	std::cerr << "ERROR (CORBA::COMM_FAILURE): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
    catch(CORBA::TRANSIENT ex)
	{
    update_num_lost_logs(event);
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;
	
	std::cerr << "ERROR (CORBA::TRANSIENT): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
    catch(CORBA::OBJECT_NOT_EXIST ex)
    {
    update_num_lost_logs(event);
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (CORBA::OBJECT_NOT_EXIST): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog; 
	std::cerr << "'!" << std::endl;
    if(m_service != NULL && true == m_service->autoreconnect())
    {
        try {
            m_service->reinit();
            proxy_consumer_->push_structured_event (event);
            --m_numLostLogs;
        } catch(...) {}
    }
    }
    catch(CORBA::BAD_OPERATION ex) 
    {  
    update_num_lost_logs(event);
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (CORBA::BAD_OPERATION): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog; 
	std::cerr << "'!" << std::endl;
    if(m_service != NULL && true == m_service->autoreconnect())
    {
        try {
            m_service->reinit();
            proxy_consumer_->push_structured_event (event);
            --m_numLostLogs;
        } catch(...) {}
    }
    }
    catch(...)
	{
    update_num_lost_logs(event);
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (Unkwown): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
}

void
ACSStructuredPushSupplier::disconnect_structured_push_supplier ()
{
  // No-Op.
}

/*****************************************************************/
