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

static const uint32_t DEFAULT_SEND_INTERVAL = 1000;
static const uint32_t DEFAULT_NUM_ITEMS = 0;
static const std::string DEFAULT_IOR_NS = "";
static const std::string DEFAULT_CHANNEL_FILE = "";
static const uint32_t DEFAULT_OUTPUT_DELAY = 1;
static const int32_t DEFAULT_CHANNEL_ID = -1;
static const std::string DEFAULT_ANTENNA_PREFIX_NAME = "ANTENNA_";

struct SuppParams {
	uint32_t sendInterval;
	uint32_t nItems;
	std::string iorNS;
	std::string channelFile;
	uint32_t outputDelay;
	int32_t channelID;
	std::string antennaPrefixName;
	std::string ORBOptions;
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

	/**
	 * Disconnect from the NC and close the ORB
	 */
	void shutdown ();
private:

	void saveChannelId(const std::string &file,CosNotifyChannelAdmin::ChannelID channelID);

	CORBA::ORB_var orb;
	PortableServer::POA_var root_poa_;
	bool m_stop;
	uint64_t m_numEventsSent;
};

#endif /*!PDATASUPPLIER_H*/
