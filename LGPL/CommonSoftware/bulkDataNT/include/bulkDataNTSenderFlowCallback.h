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
* "@(#) $Id: bulkDataNTSenderFlowCallback.h,v 1.6 2012/02/03 14:38:55 bjeram Exp $"
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

	void setFlowName (const char* name) { flowName_m =name; }
	const char* getFlowName () { return flowName_m.c_str(); }

	void setStreamName (const char* name) { streamName_m =name; }
	const char* getStreamName () { return streamName_m.c_str(); }

	/**
	 * The method is called when an asychronous error happen on sender flow.
	 * @param error as completion. It can be:
	 * #DDSOfferedIncompatibleQoSCompletion
	 * #DDSOffeeredDeadlineMissedCompletion
	 * #DDSLivelinesLostCompletion
	 */
	virtual void onError(ACSErr::CompletionImpl &error);

	/**
	 * This method is called when new receiver connects
	 * @param totalRcvs number of all connected receivers so far
	 */
	virtual void onReceiverConnect(unsigned short totalRcvs);

	/**
	 * This method is called when a receiver disconnects
	 * @param totalRcvs total number of receivers remain after the disconnect
	 */
	virtual void onReceiverDisconnect(unsigned short totalRcvs);

protected:
	std::string flowName_m;
	std::string streamName_m;

};//class BulkDataNTSenderFlowCallback

};//namespace

#endif /*!_H*/
