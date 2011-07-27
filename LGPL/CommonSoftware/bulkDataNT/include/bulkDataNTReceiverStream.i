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
* "@(#) $Id: bulkDataNTReceiverStream.i,v 1.2 2011/07/27 14:05:51 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??
#include <ACSBulkDataError.h>   // error definition  ??

using namespace AcsBulkdata;


template<class TReceiverCallback>
BulkDataNTReceiverStream<TReceiverCallback>::BulkDataNTReceiverStream(const char* name)
: BulkDataNTStream(name)
{


}


template<class TReceiverCallback>
BulkDataNTReceiverStream<TReceiverCallback>::~BulkDataNTReceiverStream()
{
	//destroyFlows(); //if flows have not been deleted
}


template<class TReceiverCallback>
BulkDataNTReceiverFlow* BulkDataNTReceiverStream<TReceiverCallback>::createFlow(const char *flowName, BulkDataCallback *cb)
{
	BulkDataCallback *callback;

	if (this->getFlow(flowName)!=0)
	{
		std::cerr << "Flow: " << flowName << " already exists" << std::endl;
		return 0;
	}

	callback = (cb==0) ? new TReceiverCallback() : cb;

	BulkDataNTReceiverFlow* flow = new BulkDataNTReceiverFlow(this, flowName, callback);

	receiverFlows_m.insert(std::pair<std::string, BulkDataNTReceiverFlow*>(flowName, flow));

	return flow;
}

template<class TReceiverCallback>
BulkDataNTReceiverFlow* BulkDataNTReceiverStream<TReceiverCallback>::getFlow(const char *flowName)
{
	ReceiverFlowMap::iterator iter = receiverFlows_m.find(flowName);
	if ( iter != receiverFlows_m.end() )
		return iter->second;
	else
		return 0;
}//getFlow

template<class TReceiverCallback>
void BulkDataNTReceiverStream<TReceiverCallback>::removeFlowFromMap(const char* flowName)
{
	// could we jsut use receiverFlows_m.erase(flowname); ??
	ReceiverFlowMap::iterator iter = receiverFlows_m.find(flowName);
	if ( iter != receiverFlows_m.end() )
	{
		receiverFlows_m.erase(iter);
	}
	else
	{
		//TBD: error handling
	}
}//removeFlowFromMap

template<class TReceiverCallback>
void BulkDataNTReceiverStream<TReceiverCallback>::createMultipleFlowsFromConfig(const char *config)
{
	try
	{
		if(ACE_OS::strcmp(config, "") == 0)
		{
			createFlow("00");
			return;
		}

		TAO_Tokenizer addressToken(config, '/');

		int numOtherFeps = addressToken.num_tokens();
		if(numOtherFeps > 19)
		{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createMultipleFlows too many flows specified - maximum 19"));
			ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
			throw err;
		}
		char strFlowNumber[2];
		for (int i=0; i<numOtherFeps; i++)
		{
			sprintf(strFlowNumber,"%d",i);
			this->createFlow(strFlowNumber);
		}//for

	}catch(...)
	{
		printf("... ERROR in createMultipleFlows using fepsConfig\n");

	}//try-catch
}




template<class TReceiverCallback>
void BulkDataNTReceiverStream<TReceiverCallback>::setReceiverName(ACE_CString recvName)
{
	printf("setReceiverName not implemented yest\n");
}//setReceiverName
