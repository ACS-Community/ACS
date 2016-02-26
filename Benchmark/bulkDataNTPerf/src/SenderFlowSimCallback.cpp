/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2016-02-25  created 
*/

#include "SenderFlowSimCallback.h"

void SenderFlowSimCallback::onError(ACSErr::CompletionImpl & error)
{
	error.log();
	std:: cout << "An error occurred to flow " << flowName << ": type=" << error.getType() << " code=" << error.getCode() << std::endl;
}

void SenderFlowSimCallback::onReceiverConnect(unsigned short  totalRcvs)
{
	std:: cout << "A new receiver connected to " << flowName << ": number of connected receivers " << totalRcvs << std::endl;
}



void SenderFlowSimCallback::onReceiverDisconnect(unsigned short  totalRcvs)
{
	std:: cout << "A new receiver disconnected from " << flowName << ": number of connected receivers " << totalRcvs << std::endl;
}








