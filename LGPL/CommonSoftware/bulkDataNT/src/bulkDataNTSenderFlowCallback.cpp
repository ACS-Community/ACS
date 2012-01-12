/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2012 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*
* "@(#) $Id: bulkDataNTSenderFlowCallback.cpp,v 1.2 2012/01/12 14:49:14 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2012-01-12  created 
*/

#include "bulkDataNTSenderFlowCallback.h"

void AcsBulkdata::BulkDataNTSenderFlowCallback::onError(ACSErr::CompletionImpl & error)
{
	error.log();
}

void AcsBulkdata::BulkDataNTSenderFlowCallback::onReceiverConnect(unsigned short  newRcvs, unsigned short  totalRcvs)
{
}



void AcsBulkdata::BulkDataNTSenderFlowCallback::onReceiverDisconnect(unsigned short  discRcvs, unsigned short  totalRcvs)
{
}

//onError



/*___oOo___*/
