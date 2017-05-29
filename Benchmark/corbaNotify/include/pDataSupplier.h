#ifndef PDATASUPPLIER_H
#define PDATASUPPLIER_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2014 
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
* almadev  2014-08-28  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <ORB.h>
#include <PortableServer.h>
#include <orbsvcs/CosNotifyChannelAdminC.h>
#include <stdint.h>
#include "TimevalUtils.h"
#include "QoSProps.h"

class SupplierTimer;

static const uint32_t DEFAULT_SEND_INTERVAL = 1000;
static const uint32_t DEFAULT_ARRAY_LENGTH = 1;
static const uint32_t DEFAULT_NUM_ITEMS = 0;
static const std::string DEFAULT_IOR_NS = "";
static const std::string DEFAULT_CHANNEL_FILE = "";
static const uint32_t DEFAULT_OUTPUT_DELAY = 1;
static const int32_t DEFAULT_CHANNEL_ID = -1;
static const std::string DEFAULT_ANTENNA_PREFIX_NAME = "ANTENNA_";


struct SuppParams {
	uint32_t sendInterval;
    uint32_t arrayLength;
	uint32_t nItems;
	std::string iorNS;
	std::string channelFile;
	uint32_t outputDelay;
	int32_t channelID;
	std::string antennaPrefixName;
	std::string ORBOptions;
    TimevalUtils::TimeoutMS timeout;
    QoSProps *qosProps;
};

class DataSupplier {
public:
	DataSupplier();
	virtual ~DataSupplier();

	/**
	 * Init ORB
	 */
	void init_ORB (int argc, char *argv []);

	void run(const SuppParams &params);

	void stop();

	uint64_t getNumEventsSent() const;
	uint64_t getNumEventsSentOk() const;
	uint64_t getNumEventsSentErrTimeout() const;
	uint64_t getNumEventsSentErrTransient() const;
	uint64_t getNumEventsSentErrObjNotExist() const;
	uint64_t getNumEventsSentErrCommFailure() const;
	uint64_t getNumEventsSentErrUnknown() const;

	/**
	 * Disconnect from the NC and close the ORB
	 */
	void shutdown ();
private:

	void saveChannelId(const std::string &file,CosNotifyChannelAdmin::ChannelID channelID);

	static const uint32_t POS_ERR_TIMEOUT=0;
	static const uint32_t POS_ERR_TRANSIENT=1;
	static const uint32_t POS_ERR_OBJ_NOT_EXIST=2;
	static const uint32_t POS_ERR_COMM_FAILURE=3;
	static const uint32_t POS_ERR_UNKNOWN=4;
	static const uint32_t NUM_ERR=5;

	CORBA::ORB_var orb;
	PortableServer::POA_var root_poa_;
	bool m_stop;
	uint64_t m_numEventsSent;
	uint64_t m_numEventsSentOk;
	uint64_t m_numEventsSentErr[NUM_ERR];
    SupplierTimer *m_timer;
    char *m_eventTypeName;
};

#endif /*!PDATASUPPLIER_H*/
