/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2011
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
* "@(#) $Id: bulkDataNTCallback.cpp,v 1.12 2012/09/06 10:50:30 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTCallback.h"

void AcsBulkdata::BulkDataNTCallback::onError(ACSErr::CompletionImpl &error)
{
	error.log();
}//onError

void AcsBulkdata::BulkDataNTCallback::setCBReceiveProcessTimeout(double to){
	cbReceiveProcessTimeout_m=to;
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_DEBUG, "cbReceiveProcessTimeout set to: %f sec",
			cbReceiveProcessTimeout_m));
}

void AcsBulkdata::BulkDataNTCallback::setCBReceiveAvgProcessTimeout(double to){
	cbReceiveAvgProcessTimeout_m=to;
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_DEBUG, "cbReceiveAvgProcessTimeout set to: %f sec",
			cbReceiveAvgProcessTimeout_m));
}

void AcsBulkdata::BulkDataNTCallback::onDataLost(unsigned long frameCount, unsigned long totalFrames, ACSErr::CompletionImpl &error)
{
	error.log();
}//onDataLost
