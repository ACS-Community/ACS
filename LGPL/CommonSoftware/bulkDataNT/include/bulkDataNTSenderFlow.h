#ifndef _BULK_DATA_NT_SENDER_FLOW_H_
#define _BULK_DATA_NT_SENDER_FLOW_H_
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
* "@(#) $Id: bulkDataNTSenderFlow.h,v 1.13 2011/12/15 15:25:30 bjeram Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* bjeram  2011-04-19  created
*/

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include "bulkDataNTFlow.h"
#include "bulkDataNTDDSPublisher.h"
#include "bulkDataNTSenderStream.h"
#include "bulkDataNTWriterListener.h"


namespace AcsBulkdata
{

class BulkDataNTSenderStream;

class BulkDataNTSenderFlow : public BulkDataNTFlow
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTSenderFlow(BulkDataNTSenderStream *senderStream,
						const char* flowName,
						const SenderFlowConfiguration &sndCfg
						/*cb*/);

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTSenderFlow();


	unsigned int getNumberOfReceivers();

	// why do we have default parameter for msg block
	void startSend(ACE_Message_Block *param = 0);

	/* in old BD was const char now it is unsigned char !!!*/
	void startSend(const unsigned char *param, size_t len);

	// const char* -> const unsigned char* (should we add a new method with const char*)
	void sendData(const unsigned char *buffer, size_t len);

	void stopSend();
protected:
	AcsBulkdata::BulkDataNTSenderStream *senderStream_m;

	AcsBulkdata::BulkDataNTDDSPublisher *ddsPublisher_m;
	DDS::Topic *ddsTopic_m;
	BulkDataNTWriterListener *writerReaderListener_m;
	ACSBulkData::BulkDataNTFrameDataWriter *ddsDataWriter_m;

	DDS::Duration_t ackTimeout_m;  /// ACKs timeout
	void setACKsTimeout(double ACKsTimeout);  /// setter fro ackTimeout

	// common method for writing/sending data  from startSend and stopSend (could be also used for sendData ?)
	// should we add also timeout parameter ?
	// should it go to upper class Publisher ?
	void writeFrame(ACSBulkData::DataType dataType,  const unsigned char *param=0, size_t len=0, unsigned int restFrameCount=0);

	// frame
	ACSBulkData::BulkDataNTFrame *frame_m;

	/// disable default - empty constructor
	BulkDataNTSenderFlow();
	/// ALMA C++ coding standards state assignment operators should be disabled.
	void operator=(const BulkDataNTSenderFlow&);
	/// ALMA C++ coding standards state copy constructors should be disabled.
	BulkDataNTSenderFlow(const BulkDataNTSenderFlow&);
};//class BulkDataSenderFlow

};


#endif /*!_H*/
