#ifndef ACS_ALARM_SYSTEM_INTERFACE_PROXY_H
#define ACS_ALARM_SYSTEM_INTERFACE_PROXY_H
/*******************************************************************************
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2006 
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
* "@(#) $Id$"
*
* who       when      what
* --------  --------  ----------------------------------------------
* acaproni  2006-07-12  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */

#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#import <AlarmSystemInterface.h>
#import <FaultState.h>
#import <loggingLogger.h>
#import <loggingACEMACROS.h>

using namespace Logging;

/**
 * Implementation of a source that log messages instead of sending msg
 * to the AlarmSrevice
 */
class ACSAlarmSystemInterfaceProxy: public laserSource::AlarmSystemInterface {
	private:
		/**
		 * The logger
		 */
		Logger::LoggerSmartPtr m_logger;
	
	virtual void close() {
	}
	
	public:
	
		ACSAlarmSystemInterfaceProxy(string name): AlarmSystemInterface() {
			setSourceName(name);
			// Get the logger
			m_logger=Logger::getGlobalLogger();
		}
		
		virtual ~ACSAlarmSystemInterfaceProxy() {}
		/**
	 	 * Push a fault state.
	 	 * @param state the fault state change to push.
	 	 */
		virtual void push(laserSource::FaultState & state) {
			string msg="Alarm sent: <";
			msg+=state.getFamily();
			msg+=",";
			msg+=state.getMember();
			msg+=",";
			msg+=state.getCode();
			msg+="> ";
			msg+=state.getDescriptor();
			m_logger->log(m_logger->LM_ALERT,msg.c_str());
		}
	
		/**
	 	 * Push a collection of fault states.
	 	 * @param states
	 	 */
		virtual void push(vector<laserSource::FaultState> & states) {
			for (unsigned int t=0; t<states.size(); t++) {
				laserSource::FaultState fs = states[t];
				push(fs);
			}
		}
	
		/**
	 	 * Push the set of active fault states.
	 	 * @param activeFaults the active fault states.
	 	 */
		virtual void pushActiveList(vector<laserSource::FaultState> & activeFaults) {
			push(activeFaults);
		}
};

#endif /*!ACS_ALARM_SYSTEM_INTERFACE_PROXY_H*/
