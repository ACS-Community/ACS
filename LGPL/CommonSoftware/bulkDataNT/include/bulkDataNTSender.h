#ifndef _BULK_DATA_NT_SENDER_H_
#define _BULK_DATA_NT_SENDER_H_
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
* "@(#) $Id: bulkDataNTSender.h,v 1.1 2011/05/20 13:39:23 bjeram Exp $"
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


#include "bulkDataNTBase.h"
#include "bulkDataNTDDSPublisher.h"

namespace AcsBulkdata
{


// TBD: default class for TSenderCallback
//template<class TSenderCallback>
class BulkDataNTSender : public AcsBulkdata::BulkDataNTBase, public AcsBulkdata::BulkDataNTDDSPublisher
{
public:

	/**
	 * Constructor
	 */
	BulkDataNTSender();

	/**
	 * Destructor
	 */
	virtual ~BulkDataNTSender();

	//TBD with stream name or better to have initalizeStream or createStream(const char* name, const char *config)
	// what should be a format of config parameter ?
	// can the stream name defeine QoS profile (problem that we need one for sender aone fro receiver(s) or similar ?
	// void initialize();


	// TBD: is this better than createSingleFlow and createMultipleFlows
	// do we have to provide a name or just a number, for example we need three flows
	// if we decide for name then we have to change send methods as well.
	// here we should connect to the DDS topic
	// TBD: here we can also send the callback?
	void createFlow(const unsigned short numberOfFlows=1); //const char* flowName);

	//TBD should we use this and feps if QoS XML ?
	//	void createMultipleFlows(const char *fepsConfig);

	/**
	 * destroys all created flows and returns number of destroyed flows
	 * @return number of destroyed flows
	 */
	unsigned int destroyFlows();

	// TBD: do we need to destroy single flow ??
	// void destroyFlow(FlowNumberType flownumber);

	// why do we have default parameter for msg block
	void startSend(FlowNumberType flownumber, ACE_Message_Block *param = 0);

	/* in old BD was const char now it is unsigned char !!!*/
	void startSend(FlowNumberType flownumber, const unsigned char *param, size_t len);

	void sendData(FlowNumberType flownumber, ACE_Message_Block *buffer);
	// const char* -> const unsigned char* (should we add a new method with const char*)
	void sendData(FlowNumberType flownumber, const unsigned char *buffer, size_t len);

	void stopSend(FlowNumberType flownumber);
protected:

	// common method for writing/sending data  from startSend adn stopSend (could be also used for sendDAta ?)
	// should we add also timeout parameter ?
	void writeFrame(FlowNumberType flownumber, ACSBulkData::DataType dataType,  const unsigned char *param=0, size_t len=0);

	/*std::vector<FlowData>*/
	SenderFlowData *senderFlows_m;

};//class BulkDataSender

};


#endif /*!_H*/
