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
* "@(#) $Id: loggingACSStructuredPushSupplierBin.cpp,v 1.2 2007/05/28 06:23:39 cparedes Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/


#include "loggingACSStructuredPushSupplierBin.h"
#include "loggingACEMACROS.h"
#include <iostream>
#include <loggingService.h>

/*****************************************************************/


void
ACSStructuredPushSupplierBin::send_event (const CosNotification::StructuredEvent& event)
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
    ++m_numLostLogs;
//	const char * xmlLog;
//	event.remainder_of_body >>= xmlLog;
	
	std::cerr << "ERROR (CORBA::COMM_FAILURE): ";
	std::cerr << "failed to send a logging event  - '";
//	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
    catch(CORBA::TRANSIENT ex)
	{
    ++m_numLostLogs;
//	const char * xmlLog;
//	event.remainder_of_body >>= xmlLog;
	
	std::cerr << "ERROR (CORBA::TRANSIENT): ";
	std::cerr << "failed to send a logging event  - '";
//	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
    catch(CORBA::OBJECT_NOT_EXIST ex)
    {
    ++m_numLostLogs;
//	const char * xmlLog;
//	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (CORBA::OBJECT_NOT_EXIST): ";
	std::cerr << "failed to send a logging event  - '";
//	std::cerr << xmlLog; 
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
    ++m_numLostLogs;
//	const char * xmlLog;
//	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (CORBA::BAD_OPERATION): ";
	std::cerr << "failed to send a logging event  - '";
//	std::cerr << xmlLog; 
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
    ++m_numLostLogs;
//	const char * xmlLog;
//	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (Unkwown): ";
	std::cerr << "failed to send a logging event  - '";
//	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
}

/*****************************************************************/
