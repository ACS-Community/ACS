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
* "@(#) $Id: bulkDataNTReceiverStream.i,v 1.21 2012/01/11 15:26:16 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#include <ACSBulkDataError.h>   // error definition  ??
#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??

using namespace AcsBulkdata;
using namespace ACS_BD_Errors;

template<class TReceiverCallback>
BulkDataNTReceiverStream<TReceiverCallback>::BulkDataNTReceiverStream(const char* streamName, const ReceiverStreamConfiguration &cfg)
: BulkDataNTReceiverStreamBase(streamName, cfg), notRemoveFromMap_m(false)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
}


template<class TReceiverCallback>
BulkDataNTReceiverStream<TReceiverCallback>::BulkDataNTReceiverStream(const char* receiverName, const char* streamName, const ReceiverStreamConfiguration &cfg)
: BulkDataNTReceiverStreamBase(receiverName, streamName, cfg), notRemoveFromMap_m(false)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
}


template<class TReceiverCallback>
BulkDataNTReceiverStream<TReceiverCallback>::~BulkDataNTReceiverStream()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	notRemoveFromMap_m = true; //elements should not be removed from the map
	ReceiverFlowMap::iterator i = receiverFlows_m.begin();
	for(;i!=receiverFlows_m.end(); i++)
		delete (i->second);
	receiverFlows_m.clear();
}//~BulkDataNTReceiverStream


template<class TReceiverCallback>
BulkDataNTReceiverFlow* BulkDataNTReceiverStream<TReceiverCallback>::createFlow(const char *flowName, const ReceiverFlowConfiguration &cfg, BulkDataNTCallback *cb, bool releaseCB)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	BulkDataNTCallback *callback=0;
	BulkDataNTReceiverFlow* flow;

	if (this->existFlow(flowName))
	{
		FlowAlreadyExistsExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(streamName_m.c_str());
		ex.setFlowName(flowName);
		throw ex;
	}//if
	try{
		callback = (cb==0) ? new TReceiverCallback() : cb;
		flow = new BulkDataNTReceiverFlow(this, flowName, cfg, callback, (cb==0)||releaseCB);
		receiverFlows_m.insert(std::pair<std::string, BulkDataNTReceiverFlow*>(flowName, flow));
		return flow;
	}catch(const ACSErr::ACSbaseExImpl &acsEx)
	{
		FlowCreateProblemExImpl ex(acsEx, __FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(streamName_m.c_str());
		ex.setFlowName(flowName);
		throw ex;
	}
	catch(const std::exception &stdEx)
	{
		ACSErrTypeCommon::StdExceptionExImpl stdExWrapper(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		stdExWrapper.setWhat(stdEx.what());

		FlowCreateProblemExImpl ex(stdExWrapper, __FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(streamName_m.c_str());
		ex.setFlowName(flowName);
		throw ex;
	}
	catch(...)
	{
		ACSErrTypeCommon::UnexpectedExceptionExImpl uex(__FILE__, __LINE__,__PRETTY_FUNCTION__);

		FlowCreateProblemExImpl ex(uex, __FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(streamName_m.c_str());
		ex.setFlowName(flowName);
		throw ex;
	}//try-catch
}//createFlow

template<class TReceiverCallback>
BulkDataNTReceiverFlow* BulkDataNTReceiverStream<TReceiverCallback>::getFlow(const char *flowName)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	ReceiverFlowMap::iterator iter = receiverFlows_m.find(flowName);
	if ( iter != receiverFlows_m.end() )
	{
		return iter->second;
	}
	else
	{
		FlowNotExistExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(streamName_m.c_str());
		ex.setFlowName(flowName);
		throw ex;
	}
}//getFlow

template<class TReceiverCallback>
bool BulkDataNTReceiverStream<TReceiverCallback>::existFlow(const char* flowName)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	return ( receiverFlows_m.find(flowName) != receiverFlows_m.end() );
}//existFlow

template<class TReceiverCallback>
void BulkDataNTReceiverStream<TReceiverCallback>::removeFlowFromMap(const char* flowName)
{
	if (notRemoveFromMap_m) return;
	AUTO_TRACE(__PRETTY_FUNCTION__);

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
			ReceiverFlowConfiguration cfg; //just temporary
			createFlow("00", cfg);
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
			ReceiverFlowConfiguration cfg; //just temporary
			sprintf(strFlowNumber,"%d",i);
			this->createFlow(strFlowNumber, cfg);
		}//for

	}catch(const ACSErr::ACSbaseExImpl &ex)
	{
		ex.log();
	}
	catch(...)
	{
		printf("... ERROR in createMultipleFlows using fepsConfig\n");

	}//try-catch
}

template<class TReceiverCallback>
std::vector<std::string> BulkDataNTReceiverStream<TReceiverCallback>::getFlowNames()
{
  std::vector<std::string> flowNames;

  ReceiverFlowMap::iterator i = receiverFlows_m.begin();
  for(;i!=receiverFlows_m.end(); i++)
    flowNames.push_back(i->first);

  return flowNames;
}//getFlowNames

template<class TReceiverCallback>
unsigned int BulkDataNTReceiverStream<TReceiverCallback>::getFlowNumber()
{
  return receiverFlows_m.size();
}//getFlowNumber
