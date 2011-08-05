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
* "@(#) $Id: bulkDataNTSenderStream.h,v 1.6 2011/08/05 14:09:12 bjeram Exp $"
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


#include <map>



namespace AcsBulkdata
{

class BulkDataNTSenderFlow;


// TBD: default class for TSenderCallback
//template<class TSenderCallback>
class BulkDataNTSenderStream : public BulkDataNTStream
//, public AcsBulkdata::BulkDataNTDDSPublisher
{
	friend class BulkDataNTSenderFlow;
public:

	/**
	 * Constructor
	 */
	//TBD should we allow to create a stream w/o configuration ?
	BulkDataNTSenderStream(const char* name, const SenderStreamConfiguration &cfg=SenderStreamConfiguration());

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTSenderStream();

	// TBD: is this better than createSingleFlow and createMultipleFlows
	// do we have to provide a name or just a number, for example we need three flows
	// if we decide for name then we have to change send methods as well.
	// here we should connect to the DDS topic
	// TBD: here we can also send the callback?
	BulkDataNTSenderFlow* createFlow(const char* flowName, const SenderFlowConfiguration &cfg=SenderFlowConfiguration());



	//TBD should we use this and feps if QoS XML ?
	// now we have it just for backward compatibility
	// is this method the same for Receiver and Sender ?
	void createMultipleFlowsFromConfig(const char *config);

	BulkDataNTSenderFlow* getFlow(const char* flowName);



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
