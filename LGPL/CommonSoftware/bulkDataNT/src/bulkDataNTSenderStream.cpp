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
* "@(#) $Id: bulkDataNTSenderStream.cpp,v 1.1 2011/07/25 13:51:01 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTSenderFlow.h"
#include <iostream>
#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??
#include <ACSBulkDataError.h>   // error definition  ??


static char *rcsId="@(#) $Id: bulkDataNTSenderStream.cpp,v 1.1 2011/07/25 13:51:01 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace AcsBulkdata;
using namespace std;

BulkDataNTSenderStream::BulkDataNTSenderStream(const char* name)
: BulkDataNTStream(name)
{
}



BulkDataNTSenderStream::~BulkDataNTSenderStream()
{

}



void BulkDataNTSenderStream::initialize()
{
	createDDSFactory();
	createDDSParticipant(); //should be somewhere else in initialize or createStream

}

BulkDataNTSenderFlow* BulkDataNTSenderStream::createFlow(const char* flowName/*, cb*/)
{
	if (this->getFlow(flowName)!=0)
	{
		std::cerr << "Flow: " << flowName << " already exists" << std::endl;
		return 0;
	}

	BulkDataNTSenderFlow* flow = new BulkDataNTSenderFlow(this, flowName);
	//add to map
	flows_m.insert(std::pair<std::string, BulkDataNTSenderFlow*>(flowName, flow));
	return flow;
}//createFlow



BulkDataNTSenderFlow* BulkDataNTSenderStream::getFlow(const char* flowName)
{
	SenderFlowMap::iterator iter = flows_m.find(flowName);
	if ( iter != flows_m.end() )
		return iter->second;
	else
		return 0;
}


void BulkDataNTSenderStream::removeFlowFromMap(const char* flowName)
{
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
