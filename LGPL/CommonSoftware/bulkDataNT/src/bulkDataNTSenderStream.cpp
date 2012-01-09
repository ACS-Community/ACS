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
* "@(#) $Id: bulkDataNTSenderStream.cpp,v 1.12 2012/01/09 15:48:29 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??
#include <ACSBulkDataError.h>   // error definition  ??


static char *rcsId="@(#) $Id: bulkDataNTSenderStream.cpp,v 1.12 2012/01/09 15:48:29 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace AcsBulkdata;
using namespace std;
using namespace ACS_BD_Errors;
using namespace ACSErrTypeCommon;

BulkDataNTSenderStream::BulkDataNTSenderStream(const char* name, const SenderStreamConfiguration &cfg)
: BulkDataNTStream(name, cfg), notRemoveFromMap_m(false)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
}//BulkDataNTSenderStream

BulkDataNTSenderStream::~BulkDataNTSenderStream()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	notRemoveFromMap_m = true; //elements should not be removed from the map
	SenderFlowMap::iterator i = flows_m.begin();
	for(;i!=flows_m.end(); i++)
		delete (i->second);
	flows_m.clear();
 }//~BulkDataNTSenderStream

BulkDataNTSenderFlow* BulkDataNTSenderStream::createFlow(const char* flowName, const SenderFlowConfiguration &cfg,
															BulkDataNTSenderFlowStatusCallback *cb, bool releaseCB)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	BulkDataNTSenderFlow *flow = 0;
	if (this->existFlow(flowName))
	{
		FlowAlreadyExistsExImpl ex(__FILE__, __LINE__, __PRETTY_FUNCTION__);
		ex.setStreamName(streamName_m.c_str());
		ex.setFlowName(flowName);
		throw ex;
	}

	try{
		flow = new BulkDataNTSenderFlow(this, flowName, cfg);
		flows_m.insert(std::pair<std::string, BulkDataNTSenderFlow*>(flowName, flow));
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
    	StdExceptionExImpl stdExWrapper(__FILE__, __LINE__, __PRETTY_FUNCTION__);
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


BulkDataNTSenderFlow* BulkDataNTSenderStream::getFlow(const char* flowName)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);

	SenderFlowMap::iterator iter = flows_m.find(flowName);
	if ( iter != flows_m.end() )
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


bool BulkDataNTSenderStream::existFlow(const char* flowName)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	return ( flows_m.find(flowName) != flows_m.end() );
}//existFlow

void BulkDataNTSenderStream::removeFlowFromMap(const char* flowName)
{
	if (notRemoveFromMap_m) return;
	AUTO_TRACE(__PRETTY_FUNCTION__);
	SenderFlowMap::iterator iter = flows_m.find(flowName);
	if ( iter != flows_m.end() )
	{
		flows_m.erase(iter);
	}
	else
	{
		//TBD: error handling
	}
}//removeFlowFromMap


void BulkDataNTSenderStream::createMultipleFlowsFromConfig(const char *config)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
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
			ACS_SHORT_LOG((LM_ERROR,"BulkDataNTSender::createMultipleFlows too many flows specified - maximum 19"));
			ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlowsFromConfig");
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



/*___oOo___*/
