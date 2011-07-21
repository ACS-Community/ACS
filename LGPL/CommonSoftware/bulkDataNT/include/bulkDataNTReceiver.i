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
* "@(#) $Id: bulkDataNTReceiver.i,v 1.2 2011/07/21 15:14:04 bjeram Exp $"
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
BulkDataNTReceiver<TReceiverCallback>::BulkDataNTReceiver()
{
	receiverFlows_m = NULL;

}


template<class TReceiverCallback>
BulkDataNTReceiver<TReceiverCallback>::~BulkDataNTReceiver()
{
	destroyFlows(); //if flows have not been deleted
}


template<class TReceiverCallback>
void BulkDataNTReceiver<TReceiverCallback>::initialize()
{
	createDDSFactory();
	createDDSParticipant();
	this->streamName_m= "TestFlow";
}

template<class TReceiverCallback>
void BulkDataNTReceiver<TReceiverCallback>::createSingleFlow()
{
	this->createFlows(1);
}

template<class TReceiverCallback>
void BulkDataNTReceiver<TReceiverCallback>::createFlows(const unsigned short numberOfFlows)
{
	std::string topicName;
	// should we check if we already have flows ?
	if (receiverFlows_m!=NULL)
	{
		printf("flows already created !!");
		return;
	}
	receiverFlows_m = new ReceiverFlowData[numberOfFlows];
	numOfFlows_m = numberOfFlows;
	// + max number of flows ?
	char strFlowNumber[2];

	for(unsigned int i=0; i<numberOfFlows; i++)
	{
		sprintf(strFlowNumber,"%d",i);
		// error handling !!!
		// the anme of the flow is stream anme + flow number;
		std::string topicName = streamName_m + "#" + strFlowNumber;
		DDS::Topic *topic = createDDSTopic(topicName.c_str());

		receiverFlows_m[i].callback_m = new TReceiverCallback();
		receiverFlows_m[i].callback_m->setFlowname(topicName.c_str());

		receiverFlows_m[i].dataReaderListener = new BulkDataNTReaderListener(topicName.c_str(), receiverFlows_m[i].callback_m);

		DDS::Subscriber *sub = createDDSSubscriber();


		ACSBulkData::BulkDataNTFrameDataReader *dr= createDDSReader(sub, topic, receiverFlows_m[i].dataReaderListener);

		receiverFlows_m[i].topicName = topicName;
		receiverFlows_m[i].topic = topic;


		receiverFlows_m[i].subscriber = sub;
		receiverFlows_m[i].dataReader = dr;
	}//for
}



template<class TReceiverCallback>
void BulkDataNTReceiver<TReceiverCallback>::createMultipleFlows(const char *fepsConfig)
{
	try
	{
		if(ACE_OS::strcmp(fepsConfig, "") == 0)
		{
			createSingleFlow();
			return;
		}

		TAO_Tokenizer addressToken(fepsConfig, '/');

		int numOtherFeps = addressToken.num_tokens();
		if(numOtherFeps > 19)
		{
			ACS_SHORT_LOG((LM_ERROR,"BulkDataReceiver<>::createMultipleFlows too many flows specified - maximum 19"));
			ACSBulkDataError::AVInvalidFlowNumberExImpl err = ACSBulkDataError::AVInvalidFlowNumberExImpl(__FILE__,__LINE__,"BulkDataReceiver::createMultipleFlows");
			throw err;
		}
		 this->createFlows(numOtherFeps);

	}catch(...)
	{
		printf("... ERROR in createMultipleFlows using fepsConfig\n");

	}//try-catch
}

template<class TReceiverCallback>
unsigned int BulkDataNTReceiver<TReceiverCallback>::destroyFlows()
{
	unsigned int i=0;
	if (receiverFlows_m==NULL) return 0;

	for(i=0; i<numOfFlows_m; i++)
		{
		receiverFlows_m[i].subscriber->delete_datareader(dynamic_cast<DDS::DataReader*>(receiverFlows_m[i].dataReader));
			// do we have to destroy also topic and subscriber ?
		}//for

	delete[] receiverFlows_m;
	receiverFlows_m = NULL;
	numOfFlows_m =0;
	return i;
}

template<class TReceiverCallback>
void BulkDataNTReceiver<TReceiverCallback>::closeReceiver()
{
	this->destroyFlows();
}

template<class TReceiverCallback>
void BulkDataNTReceiver<TReceiverCallback>::setReceiverName(ACE_CString recvName)
{
	if (receiverFlows_m==NULL  )
	{
		printf("flows not created !!");
		return;
	}


	for(unsigned int i=0; i<numOfFlows_m; i++)
	{
		receiverFlows_m[i].callback_m->setReceiverName(recvName);
	}
}//setReceiverName
