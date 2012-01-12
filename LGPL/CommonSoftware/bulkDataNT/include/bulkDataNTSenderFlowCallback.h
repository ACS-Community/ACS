#ifndef _BULKDATANT_FLOW_CALLBACK_H_
#define _BULKDATANT_FLOW_CALLBACK_H_
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
* "@(#) $Id: bulkDataNTSenderFlowCallback.h,v 1.3 2012/01/12 14:49:14 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2011-10-18  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <acserr.h>

namespace AcsBulkdata
{
class BulkDataNTSenderFlowCallback
{
public:
	virtual ~BulkDataNTSenderFlowCallback(){};


	/**
	 * The method is called when an asychronous error happen on sender flow.
	 * @param error
	 */
	virtual void onError(ACSErr::CompletionImpl &error);

	/**
	 * This method is called when new receiver connects
	 * @param newRcvs number of newly connected receivers
	 * @param totalRcvs number of all connected receivers so far
	 */
	virtual void onReceiverConnect(unsigned short newRcvs, unsigned short totalRcvs);

	/**
	 * This method is called when a receiver disconnects
	 * @param discRecvs number of disconnected receivers
	 * @param totalRcvs total number of receivers remain after the disconnect
	 */
	virtual void onReceiverDisconnect(unsigned short discRcvs, unsigned short totalRcvs);

};//class BulkDataNTSenderFlowCallback

};//namespace

#endif /*!_H*/
