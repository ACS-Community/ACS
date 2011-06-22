#ifndef ALARMTOQUEUE_H
#define ALARMTOQUEUE_H
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
* "@(#) $Id: AlarmToQueue.h,v 1.1 2011/06/22 20:56:15 acaproni Exp $"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2011-06-18  created
*/

#include <string>

#include "Properties.h"

namespace acsalarm
{
	/**
	 * An alarm to put in the queue ready to be published
	 */
	class AlarmToQueue {
	private:
		// FaultFamily
		std::string m_FF;

		// FaultMember
		std::string m_FM;

		// FaultCode
		int m_FC;

		// user properties
		Properties m_alarmProps;

		// The state active/terminate
		bool m_active;

	public:
		/**
		 * Constructor
		 *
		 * @param fF Fault family
		 * @param fM Fault member
		 * @param fC Fault code
		 * @param activestate true means alarm to activate, false alarm to terminate
		 */
		AlarmToQueue(std::string fF, std::string fM, int fC, bool activestate);

		/**
		 * Constructor
		 *
		 * @param fF Fault family
		 * @param fM Fault member
		 * @param fC Fault code
		 * @param props The user properties
		 * @param activestate true means alarm to activate, false alarm to terminate
		 */
		AlarmToQueue(std::string fF, std::string fM, int fC, Properties props, bool activestate);
	    bool isActive() const
	    {
	        return m_active;
	    }

	    Properties getAlarmProps() const
	    {
	        return m_alarmProps;
	    }

	    int getFC() const
	    {
	        return m_FC;
	    }

	    std::string getFF() const
	    {
	        return m_FF;
	    }

	    std::string getFM() const
	    {
	        return m_FM;
	    }

	};
};


#endif /*!ALARMTOQUEUE_H*/
