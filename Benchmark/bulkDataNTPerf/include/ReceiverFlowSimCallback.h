#ifndef RECEIVERFLOWSIMCALLBACK_H
#define RECEIVERFLOWSIMCALLBACK_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2016 
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
*
* who       when      what
* --------  --------  ----------------------------------------------
* almadev  2016-02-26  created
*/

#include <list>
#include <bulkDataNTCallback.h>

/**
 * Callback for the BDNT receiver
*/

class  ReceiverFlowSimCallback :  public AcsBulkdata::BulkDataNTCallback
{
public:
	ReceiverFlowSimCallback();

	virtual ~ReceiverFlowSimCallback();

	int cbStart(unsigned char* userParam_p, unsigned  int size);

	int cbReceive(unsigned char* data, unsigned  int size);

	int cbStop();

	void setStoreData(bool shouldStoreData) {
		storeData = shouldStoreData;
	}

	std::list<unsigned char> getData() {
		return dataToStore;
	}

	uint16_t getUserParamSize() {
		return userParamSize;
	}

	static long cbDealy;
	static bool cbReceivePrint;

private:
	std::string fn; ///flow Name
	std::string sn; ///stream name
	unsigned int totalRcvData; ///total size of all received data
	unsigned int rcvDataStartStop; ///size of received data between Start/stop
	unsigned int frameCount; ///frame count
	bool storeData;
	uint16_t userParamSize;
	std::list<unsigned char> dataToStore;
};

#endif /*!RECEIVERFLOWSIMCALLBACK_H*/
