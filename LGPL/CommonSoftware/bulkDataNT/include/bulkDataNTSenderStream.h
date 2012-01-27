#ifndef _BULK_DATA_NT_SENDER_STREAM_H_
#define _BULK_DATA_NT_SENDER_STREAM_H_
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
* "@(#) $Id: bulkDataNTSenderStream.h,v 1.11 2012/01/27 14:40:37 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataNTStream.h"
#include "bulkDataNTSenderFlow.h"
#include "bulkDataNTSenderFlowCallback.h"

#include <map>


namespace AcsBulkdata
{

class BulkDataNTSenderFlow;

class BulkDataNTSenderStream : public BulkDataNTStream
{
	friend class BulkDataNTSenderFlow;
public:

	/**
	 * Sender Stream constructor.
	 * @param name name of the sender stream that should be constructed
	 * @param cfg(optional) sender stream configuration
	 * @exception #StreamCreateProblemExImpl
	 */
	BulkDataNTSenderStream(const char* name, const SenderStreamConfiguration &cfg=SenderStreamConfiguration());

	/**
	 * Sender stream destruction which destroys also all created flows.
	*/
	virtual ~BulkDataNTSenderStream();


	/**
	 * The method creates a flow an the sender stream
	 * @param flowName name of the flow that should be created
	 * @param cfg (optional) sender flow configuration
	 * @param cb (optional) Sender flow status callback
	 * @param releaseCB should be the callback released when the flow is destroyed
	 * @return pointer to created sender flow object
	 */
	BulkDataNTSenderFlow* createFlow(const char* flowName, const SenderFlowConfiguration &cfg=SenderFlowConfiguration(),
			BulkDataNTSenderFlowCallback *cb=0, bool releaseCB=false);



	//TBD should we use this and feps if QoS XML ?
	// now we have it just for backward compatibility
	// is this method the same for Receiver and Sender ?
	void createMultipleFlowsFromConfig(const char *config);

	/**
	 * It returns pointer to Sender Flow with the specified name
	 * @param flowName the name of the flow
	 * @return sender flow if present in the stream, otherwise exception
	 * @exception #FlowNotExistExImpl
	 */
	BulkDataNTSenderFlow* getFlow(const char* flowName);

	/**
	 *Method just to check if a flow exists in the stream.
	 * @param flowName the name of the flow
	 * @return true if the flow with name flowName exists in the stream otherwise false
	 */
	bool existFlow(const char* flowName);

protected:
	virtual void removeFlowFromMap(const char* flow);

	typedef  std::map<std::string, BulkDataNTSenderFlow*> SenderFlowMap;
	SenderFlowMap flows_m;
	// we need a flag that prevents elements to be removed from map when we delete flows from dtor
	bool notRemoveFromMap_m;

	/// disable default - empty constructor
	BulkDataNTSenderStream();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTSenderStream&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTSenderStream(const BulkDataNTSenderStream&);
};//class BulkDataNTSenderStream

};


#endif /*!_H*/
