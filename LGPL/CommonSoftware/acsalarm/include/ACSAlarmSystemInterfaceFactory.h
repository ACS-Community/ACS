#ifndef ACS_ALARM_SYSTEM_INTERFACE_FACTORY_H
#define ACS_ALARM_SYSTEM_INTERFACE_FACTORY_H
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

#include "AbstractAlarmSystemInterfaceFactory.h"
#include "ACSAlarmSystemInterface.h"
#include "ACSAlarmSystemInterfaceFactory.h"
#include "ACSAlarmSystemInterfaceProxy.h"
#include "utilConstants.h"
#include "maciS.h"
#include "acsErrTypeAlarmSourceFactory.h"
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>

/**
 * The class to create sources and fault states.
 * It extends the laser source but it returns different implementations of the
 * sources depending of a value of a property of the CDB
 * 
 * The ACS implementation of the source logs a message for each alarm
 * 
 * Before using the static methods of this class, the init method must be called otherwise
 * an exception is thrown.
 * 
 */
class ACSAlarmSystemInterfaceFactory
// TODO: Alessandro, can this class extend AbstractAlarmSystemInterfaceFactory? 
// We would need to make static methods non-static to do so...
{
	private:
	// It is true if ACS implementation for sources must be used,  and
	// false means CERN implementation
	// The pointer is null if it has not yet been initialized (this is done by the init method)
	static bool* m_useACSAlarmSystem;
		
	// The manager
	static maci::Manager_ptr m_manager;

	// Pointer to CERN alarm system object; will remain null if we are not using CERN implementation
	static AbstractAlarmSystemInterfaceFactory * m_AlarmSystemInterfaceFactory_p;

	private:
	/** Default constructor.  */
	ACSAlarmSystemInterfaceFactory();
	virtual ~ACSAlarmSystemInterfaceFactory();
	
	public:

	/**
	 * Init the object of the class: must be called before using the other
	 * methods of this class otherwise an exception will be thrown.
	 * Return true if the initialization went ok
	 */
	static bool init(maci::Manager_ptr manager);

	/**
	 * Getter for whether we're using the ACS Alarm system (true) or not (false).
	 */
	static bool usingACSAlarmSystem();
		
	/**
	 * Release the resources: must be called when finished using the
	 * methods of this class
	 */
	static void done();
	
	/**
 	 * Create a new instance of an alarm system interface.
	 * @param sourceName the source name.
	 * @return the interface instance.
	 */
	static auto_ptr<laserSource::ACSAlarmSystemInterface> createSource(string sourceName);
		
	/**
	 * Create a new instance of an alarm system interface without binding it to any source.
	 * @return the interface instance.
	 */
	static auto_ptr<laserSource::ACSAlarmSystemInterface> createSource();
	
	/**
	 * Create a fault state with the given family, member and code
	 */
	static auto_ptr<laserSource::ACSFaultState>createFaultState(string family, string member, int code);
	
	/**
	 * Create a fault state 
	 */
	static auto_ptr<laserSource::ACSFaultState>createFaultState();

};

#endif /*!ACS_ALARM_SYSTEM_INTERFACE_FACTORY_H*/
