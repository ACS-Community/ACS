#ifndef _BULK_DATA_NT_STREAM_H_
#define _BULK_DATA_NT_STREAM_H_
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
* "@(#) $Id: bulkDataNTStream.h,v 1.7 2011/08/04 11:22:16 bjeram Exp $"
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


#include <vector>
#include <string>
#include <Message_Block.h>
#include "bulkDataNTDDS.h"
#include "ACS_BD_Errors.h"
#include "bulkDataNTConfiguration.h"

namespace AcsBulkdata
{

typedef unsigned long FlowNumberType;


/**
 *  base class for Bulk data sender and receiver
 *  TBD probably we do not need it
 */
class BulkDataNTStream
{
	friend class BulkDataNTReceiverFlow;
public:

	/**
	 * Constructor
	 */
	BulkDataNTStream(const char* name, const StreamConfiguration &cfg);

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTStream();

	std::string getName() { return streamName_m; }

	//void destroyFlow(const  char* flowName);

	/** Set receiver name in receiver callback
	 *  @param ACE_CString
	 *  @return void
	 *  @htmlonly
	 <br><hr>
	 @endhtmlonly
	*/
	void setReceiverName(char * recvName);

	const char* getReceiverName() { return receiverName_m.c_str(); }

protected:

	DDS::DomainParticipant* getDDSParticipant(){ return participant_m; }

	virtual void removeFlowFromMap(const char* flow)=0;

	std::string streamName_m;
	std::string receiverName_m;

	const StreamConfiguration  &configuration_m; //configuration
	// those two methods and members should probably go to another class  ??
	void createDDSFactory();
	void createDDSParticipant();
	void destroyDDSParticipant();
	DDS::DomainParticipantFactory *factory_m;
	DDS::DomainParticipant* participant_m;

};//class BulkDataNTStream

};

#endif
