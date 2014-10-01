#ifndef PDATACONSUMER_H
#define PDATACONSUMER_H
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

#include <orbsvcs/CosNotifyCommS.h>
//#include <orbsvcs/CosEventCommS.h>
//#include <orbsvcs/CosEventChannelAdminC.h>
#include <orbsvcs/CosNamingC.h>
#include <orbsvcs/CosNotificationC.h>
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/Notify/MonitorControlExt/NotifyMonitoringExtC.h>
#include "corbaNotifyTest_ifC.h"

static const std::string DT_CONSUMER = "CONSUMER";
static const std::string DT_SUPPLIER = "SUPPLIER";
static const std::string DT_SUPP_CON = "SUPP_CON";


static const CosNotifyChannelAdmin::ChannelID DEFAULT_CHANNEL_ID = -1;
static const std::string DEFAULT_IOR_NS = "";
static const double DEFAULT_MAX_DELAY_SEC = 0;
static const std::string DEFAULT_DELAY_TYPE = DT_SUPP_CON;
static const int32_t DEFAULT_INTERVAL = 0;


struct ConsumerParams {
	CosNotifyChannelAdmin::ChannelID channelID;
	std::string iorNS;
	double maxDelaySec;
	std::string delayType;
	int32_t interval;
	std::string ORBOptions;
};

class Consumer : public POA_CosNotifyComm::PushConsumer
{
public:
	Consumer(void);
	virtual ~Consumer();

	bool run (int argc, ACE_TCHAR* argv[],const ConsumerParams &params);

	virtual void push (const CORBA::Any &event);
	virtual void disconnect_push_consumer (void);
	virtual void offer_change(const CosNotification::EventTypeSeq&, const CosNotification::EventTypeSeq&);

	uint64_t getNumEventsReceived() const;

protected:
	bool init_ORB(int argc, ACE_TCHAR* argv[]);
	bool getNotificationChannel(const std::string &iorNS,
			CosNotifyChannelAdmin::ChannelID channelID,
			CosNotifyChannelAdmin::EventChannel_var &channel,
			std::string &errMsg);

	CORBA::ORB_ptr m_orb;
	timespec m_tLastEvent;
	timespec m_maxDelay;
	ACS::Time m_lastEventTimestamp;
	std::string m_delayType;
	uint64_t m_numEventsReceived;
};

#endif /*!PDATACONSUMER_H*/
