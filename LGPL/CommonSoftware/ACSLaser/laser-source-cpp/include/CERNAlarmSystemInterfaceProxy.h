#ifndef ALARM_SYSTEM_INTERFACE_PROXY_H
#define ALARM_SYSTEM_INTERFACE_PROXY_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) National Research Council of Canada, 2005
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
*/

#include "AlarmSystemInterface.h"
#include "AcsAlarmPublisher.h"

namespace laserSource
{
	class CERNAlarmSystemInterfaceProxy : public acsalarm::AlarmSystemInterface
	{
		public:
			CERNAlarmSystemInterfaceProxy();
			CERNAlarmSystemInterfaceProxy(std::string theSourceName);
			virtual ~CERNAlarmSystemInterfaceProxy();
			virtual void close();

		protected:
			bool publishMessage(acsalarm::ASIMessage msg);
	
		private:
			// initialization logic used by the constructors
			void init();

			// pointer to our publisher object
			laserSource::AcsAlarmPublisher * laserPublisher;
	};
};
#endif

