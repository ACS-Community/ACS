#ifndef CERN_ALARM_SYSTEM_INTERFACE_FACTORY_H
#define CERN_ALARM_SYSTEM_INTERFACE_FACTORY_H
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

#include "AlarmSystemInterfaceFactory.h"
#include <logging.h>

namespace laserSource
{
	class CERNAlarmSystemInterfaceFactory: public AlarmSystemInterfaceFactory
	{
		public:
		/** Default constructor/destructor */
		CERNAlarmSystemInterfaceFactory();
		virtual ~CERNAlarmSystemInterfaceFactory();
		
		/**
		 * Init the object of the class: must be called before using the other
		 * methods of this class otherwise an exception will be thrown.
		 * Return true if the initialization went ok
		 */
		virtual bool init();
		
		/**
		 * Release the resources: must be called when finished using the
		 * methods of this class
		 */
		virtual void done();
	
		/**
 		 * Create a new instance of an alarm system interface.
		 * @param sourceName the source name.
		 * @return the interface instance.
		 */
		virtual acsalarm::AlarmSystemInterface* createSource(std::string sourceName);
		
		/**
		 * Create a new instance of an alarm system interface without binding it to any source.
		 * @return the interface instance.
		 */
		virtual acsalarm::AlarmSystemInterface* createSource();
		
	};
};
#endif /* ifndef CERN_ALARM_SYSTEM_INTERFACE_FACTORY_H */

