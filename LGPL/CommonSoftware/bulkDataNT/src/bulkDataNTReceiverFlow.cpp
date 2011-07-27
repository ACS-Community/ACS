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
* "@(#) $Id: bulkDataNTReceiverFlow.cpp,v 1.2 2011/07/27 07:12:10 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#include "bulkDataNTReceiverFlow.h"
#include <iostream>

#include <AV/FlowSpec_Entry.h>  // we need it for TAO_Tokenizer ??
#include <ACSBulkDataError.h>   // error definition  ??


static char *rcsId="@(#) $Id: bulkDataNTReceiverFlow.cpp,v 1.2 2011/07/27 07:12:10 bjeram Exp $";
static void *use_rcsId = ((void)&use_rcsId,(void *) &rcsId);

using namespace AcsBulkdata;
using namespace std;

BulkDataNTReceiverFlow::BulkDataNTReceiverFlow(BulkDataNTStream *receiverStream, const char* flowName, BulkDataCallback *cb) :
		receiverStream_m(receiverStream)
{
	std::string topicName;

	flowName_m = flowName;

	// should be refactor to have just one object for comunication !! DDSDataWriter or similar
	ddsSubscriber_m = new BulkDataNTDDSSubscriber(receiverStream_m->getDDSParticipant());

	topicName = receiverStream_m->getName() + "#" + flowName_m;
	ddsTopic_m = ddsSubscriber_m->createDDSTopic(topicName.c_str());

	callback_m = cb;

	callback_m->setFlowName(topicName.c_str());

	dataReaderListener_m = new BulkDataNTReaderListener(topicName.c_str(), callback_m);

	ddsDataReader_m= ddsSubscriber_m->createDDSReader(ddsTopic_m, dataReaderListener_m);

}


BulkDataNTReceiverFlow::~BulkDataNTReceiverFlow()
{
	receiverStream_m->removeFlowFromMap(flowName_m.c_str());
	delete ddsSubscriber_m;
	// delete callback_m
// +ddsDataWriter_m & ddsTopic_m ??
}


/*___oOo___*/
