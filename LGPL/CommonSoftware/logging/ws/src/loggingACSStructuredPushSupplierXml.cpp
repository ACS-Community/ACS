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
* "@(#) $Id: loggingACSStructuredPushSupplierXml.cpp,v 1.2 2007/05/28 06:23:39 cparedes Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/


#include "loggingACSStructuredPushSupplierXml.h"
#include <iostream>

/*****************************************************************/

void
ACSStructuredPushSupplierXml::send_event (const CosNotification::StructuredEvent& event)
{
  ACE_ASSERT (!CORBA::is_nil (this->proxy_consumer_.in ()));

  try
      {
      proxy_consumer_->push_structured_event (event);
      }
  catch(CORBA::COMM_FAILURE ex)
	{
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;
	
	std::cerr << "ERROR (CORBA::COMM_FAILURE): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
    catch(CORBA::TRANSIENT ex)
	{
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;
	
	std::cerr << "ERROR (CORBA::TRANSIENT): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
    catch(...)
	{
	const char * xmlLog;
	event.remainder_of_body >>= xmlLog;

	std::cerr << "ERROR (Unkwown): ";
	std::cerr << "failed to send a logging event  - '";
	std::cerr << xmlLog;
	std::cerr << "'!" << std::endl;
	}
}


/*****************************************************************/
