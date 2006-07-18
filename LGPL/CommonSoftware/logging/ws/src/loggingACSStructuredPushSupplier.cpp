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
* "@(#) $Id: loggingACSStructuredPushSupplier.cpp,v 1.3 2006/07/18 16:52:43 sharring Exp $"
*
* who       when        what
* --------  ---------   ----------------------------------------------
*/


#include "loggingACSStructuredPushSupplier.h"
#include <iostream>

/*****************************************************************/

ACSStructuredPushSupplier::ACSStructuredPushSupplier() : BaseSupplier(acscommon::LOGGING_CHANNEL_NAME)
{
}

ACSStructuredPushSupplier::~ACSStructuredPushSupplier ()
{
}


void ACSStructuredPushSupplier::send_event (const CosNotification::StructuredEvent& event)
{
	//BaseSupplier::populateHeader((CosNotification::StructuredEvent&)event);
	BaseSupplier::publishEvent(event);
}



/*****************************************************************/
