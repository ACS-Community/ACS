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
* "@(#) $Id: bulkDataNTReceiverFlow.cpp,v 1.28 2013/02/07 13:34:04 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTReceiverFlow.h"
#include <iostream>
#include "bulkDataNTReceiverStream.h"
#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??


static char *rcsId="@(#) $Id: bulkDataNTReceiverFlow.cpp,v 1.28 2013/02/07 13:34:04 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace AcsBulkdata;
using namespace std;

BulkDataNTReceiverFlow::BulkDataNTReceiverFlow(BulkDataNTReceiverStreamBase *receiverStream,
    const char* flowName,
    const ReceiverFlowConfiguration &rcvCfg,
    BulkDataNTCallback *cb,
    bool releaseCB) :
    BulkDataNTFlow(flowName),
    receiverStream_m(receiverStream),
    callback_m(cb), releaseCB_m(releaseCB),
    rcvCfg_m(rcvCfg)
{
  AUTO_TRACE(__PRETTY_FUNCTION__);
  std::string streamName, topicName;
  streamName = receiverStream_m->getName();
  topicName =  streamName + "#" + flowName_m;
  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "Going to create Receiver Flow: %s @ Stream: %s ...", flowName_m.c_str(), streamName.c_str()));

  callback_m->setStreamName(receiverStream_m->getName().c_str());
  callback_m->setFlowName(flowName);
  callback_m->setReceiverName(receiverStream_m->getReceiverName());
  callback_m->setCBReceiveProcessTimeout(rcvCfg.getCbReceiveProcessTimeout());
  callback_m->setCBReceiveAvgProcessTimeout(rcvCfg.getCbReceiveAvgProcessTimeout());

  receiverStream->addDDSQoSProfile(rcvCfg_m, topicName.c_str());

  if (!rcvCfg_m.isEnableMulticast() &&  rcvCfg_m.getUnicastPort()==ReceiverFlowConfiguration::DEFAULT_UNICAST_PORT/*=0*/) //unicast && no unicast port was defined
  {
	  rcvCfg_m.setUnicastPort(receiverStream_m->getNextFlowUnicastPort()); //re-set unicast port on local(!) copy
  }
  // should be refactor to have just one object for comunication !! DDSDataWriter or similar
  ddsSubscriber_m = new BulkDataNTDDSSubscriber(receiverStream_m->getDDSParticipant(), rcvCfg_m);

  ddsTopic_m = ddsSubscriber_m->createDDSTopic(topicName.c_str());

  dataReaderListener_m = new BulkDataNTReaderListener(topicName.c_str(), callback_m);

  ddsDataReader_m= ddsSubscriber_m->createDDSReader(ddsTopic_m, dataReaderListener_m);
  ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "Receiver Flow: %s @ Stream: %s has been created.", flowName_m.c_str(), streamName.c_str()));
}//BulkDataNTReceiverFlow


BulkDataNTReceiverFlow::~BulkDataNTReceiverFlow()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	std::string streamName = receiverStream_m->getName();

	receiverStream_m->removeFlowFromMap(flowName_m.c_str());

	// remove QoS from DDS factory if any
	receiverStream_m->removeDDSQoSProfile(rcvCfg_m);

	// this part can go to BulkDataNTDDSPublisher, anyway we need to refactor
	DDS::DomainParticipant *participant = receiverStream_m->getDDSParticipant();
	if (participant!=0)
	{
		ddsSubscriber_m->destroyDDSReader(ddsDataReader_m);
		ddsDataReader_m = 0;
		delete dataReaderListener_m;

		ddsSubscriber_m->destroyDDSTopic(ddsTopic_m);
		ddsTopic_m = 0;
	}
	else
	{
		ACS_SHORT_LOG((LM_ERROR, "Problem deleting data reader and topic because participant is NULL"));
	}
	delete ddsSubscriber_m;
	if (releaseCB_m) delete callback_m;

	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__, (LM_INFO, "Receiver Flow: %s @ stream: %s has been destroyed.", flowName_m.c_str(), streamName.c_str()));
}//~BulkDataNTReceiverFlow

void BulkDataNTReceiverFlow::setReceiverName(char* recvName)
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	std::string oldReceiverName = callback_m->getReceiverName();
	callback_m->setReceiverName(receiverStream_m->getReceiverName());
	ACS_LOG(LM_RUNTIME_CONTEXT, __PRETTY_FUNCTION__, (LM_DEBUG, "Receiver name on callback for flow: %s on steam: %s has been changed from: %s to %s",
			flowName_m.c_str(), receiverStream_m->getName().c_str(),
			oldReceiverName.c_str(), recvName
		));
}//setReceiverName

void AcsBulkdata::BulkDataNTReceiverFlow::enableCallingCB()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	dataReaderListener_m->enableCallingCB();
}


void AcsBulkdata::BulkDataNTReceiverFlow::disableCallingCB()
{
	AUTO_TRACE(__PRETTY_FUNCTION__);
	dataReaderListener_m->disableCallingCB();
}

void AcsBulkdata::BulkDataNTReceiverFlow::dumpStatistics()
{
	DDS::DataReaderProtocolStatus drps;
	DDS::DataReaderCacheStatus drcs;

	ddsDataReader_m->get_datareader_protocol_status(drps);

	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_DEBUG, "DataReader protocol status for flow: %s [sample received: %lld (%lld). Sent: HB: %lld (%lld) ACKs: %lld (%lld) NACKs: %lld (%lld). Rejected: %lld]",
					flowName_m.c_str(),
					drps.received_sample_count_change, drps.received_sample_bytes_change,
					drps.received_heartbeat_count_change, drps.received_heartbeat_bytes_change,
					drps.sent_ack_count_change, drps.sent_ack_bytes_change,
					drps.sent_nack_count_change, drps.sent_nack_bytes_change,
					drps.rejected_sample_count_change));

	ddsDataReader_m->get_datareader_cache_status(drcs);
	ACS_LOG(LM_RUNTIME_CONTEXT, __FUNCTION__,
			(LM_DEBUG, "DataReader cache Status: sample count (peak): %lld (%lld)", drcs.sample_count, drcs.sample_count_peak));

}//void dumpStatistics()

/*___oOo___*/
