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

class Consumer : public POA_CosNotifyComm::PushConsumer
{
public:
	Consumer(void);

	bool run (int argc, ACE_TCHAR* argv[],
		  CosNotifyChannelAdmin::ChannelID channelID,
		  const std::string &iorNS,double maxDelaySec,
		  const std::string &delayType);

	virtual void push (const CORBA::Any &event);
	virtual void disconnect_push_consumer (void);
	virtual void offer_change(const CosNotification::EventTypeSeq&, const CosNotification::EventTypeSeq&);

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
};

#endif /*!PDATACONSUMER_H*/
