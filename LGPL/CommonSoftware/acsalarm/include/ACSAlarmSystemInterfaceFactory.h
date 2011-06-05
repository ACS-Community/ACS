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

#include "AlarmSystemInterfaceFactory.h"
#include "AlarmSystemInterface.h"
#include "ACSAlarmSystemInterfaceFactory.h"
#include "ACSAlarmSystemInterfaceProxy.h"
#include "utilConstants.h"
#include "maciS.h"
#include "acsErrTypeAlarmSourceFactory.h"
#include <orbsvcs/CosNotifyChannelAdminS.h>
#include <orbsvcs/CosNotifyCommC.h>
#include "asiConfigurationConstants.h"
#include "ace/Task.h"

/**
 * The type of alarm system in use
 */
enum AlarmSystemType {
	NOT_YET_INITIALIZED, // unknown type: not initialized
	CERN_AS, // CERN implementation
	ACS_AS  // ACS implementation
};

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
// TODO: Alessandro, can this class extend AlarmSystemInterfaceFactory? 
// We would need to make static methods non-static to do so...
{
	private:

	// used for loading/unloading the DLL which is used to decouple (for build purposes) 
	// the logic in ACSLaser/laser-source-cpp
	static void *dllHandle;

	// At the present all the alarms are mapped in the same NC
	// so we need only one source to publish all the alarms
	static acsalarm::AlarmSystemInterface* m_sourceSingleton_p;

	// The type of alarm system in use.
	static AlarmSystemType m_AlarmSystemType;
		
	// The manager
	static maci::Manager_ptr m_manager;

	// Pointer to CERN alarm system object; will remain null if we are not using CERN implementation
	static AlarmSystemInterfaceFactory * m_AlarmSystemInterfaceFactory_p;

	// used to synchronize access to shared vars
	static ACE_Recursive_Thread_Mutex main_mutex;

	/** Default constructor.  */
	ACSAlarmSystemInterfaceFactory();
	ACSAlarmSystemInterfaceFactory(const ACSAlarmSystemInterfaceFactory&);
	ACSAlarmSystemInterfaceFactory operator=(const ACSAlarmSystemInterfaceFactory&);
	~ACSAlarmSystemInterfaceFactory();

	static void cleanUpAlarmSystemInterfacePtr();
	static void cleanUpSourceSingleton();
	static void cleanUpDLL();
	static void cleanUpManagerReference();
	static void initImplementationType(maci::Manager_ptr manager);
	static bool initDLL();
	
	/**
	 * Return the source singleton;
	 *
	 * If the singleton is null, this method instatiates a new one.
	 */
	static acsalarm::AlarmSystemInterface* getSourceSingleton();

	public:

	static maci::Manager_ptr getManager();

	/**
	 * Init the object of the class: must be called before using the other
	 * methods of this class otherwise an exception will be thrown.
	 * @param manager ptr to the acs manager.
	 * @throw acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl if there was a problem loading the DLL
	 * @return true if the initialization went ok
	 */
	static bool init(maci::Manager_ptr manager) throw (acsErrTypeAlarmSourceFactory::ErrorLoadingCERNDLLExImpl);

	/**
	 * Getter for whether we're using the ACS Alarm system (true) or not (false).
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 * @return boolean indicating whether the ACS alarm system is in use (true) or not (false), where ACS
	 * alarm system means alarms are sent to the logs, otherwise they are sent to the alarm channel.
	 */
	static AlarmSystemType usingACSAlarmSystem() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);
		
	/**
	 * Release the resources: must be called when finished using the
	 * methods of this class
	 */
	static void done();
	
	/**
 	 * Create a new instance of an alarm system interface.
	 * @param sourceName the source name.
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 * @return the interface instance.
	 */
	static acsalarm::AlarmSystemInterface* createSource(std::string sourceName) throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);
		
	/**
	 * Create a new instance of an alarm system interface without binding it to any source.
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 * @return the interface instance.
	 */
	static acsalarm::AlarmSystemInterface* createSource() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);
	
	/**
	 * Create a fault state with the given family, member and code
	 * @param family a string indicating the 'family' of the alarm (see alarm system documentation for explanation).
	 * @param member a string indicating the 'member' of the alarm (see alarm system documentation for explanation).
	 * @param code an int indicating the 'code' of the alarm (see alarm system documentation for explanation).
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 */
	static std::auto_ptr<acsalarm::FaultState>createFaultState(std::string family, std::string member, int code) throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);
	
	/**
	 * Create a fault state 
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 */
	static std::auto_ptr<acsalarm::FaultState>createFaultState() throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

	/**
	 * Convenience API for creating/sending an alarm in a single step, without user defined properties.
	 * @param family a string indicating the 'family' of the alarm (see alarm system documentation for explanation).
	 * @param member a string indicating the 'member' of the alarm (see alarm system documentation for explanation).
	 * @param code an int indicating the 'code' of the alarm (see alarm system documentation for explanation).
	 * @param active a boolean indicating if the alarm is active (true) or not (false)
	 * @param sourceName the source name, defaults to ALARM_SOURCE_NAME constant defined in "asiConfigurationConstants.h"
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 */
	static void createAndSendAlarm(std::string & faultFamily, std::string & faultMember, int faultCode, bool active, std::string sourceName = asiConfigurationConstants::ALARM_SOURCE_NAME)
                          throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

	/**
	 * Convenience API for creating/sending an alarm in a single step, with user-defined properties.
	 * @param family a string indicating the 'family' of the alarm (see alarm system documentation for explanation).
	 * @param member a string indicating the 'member' of the alarm (see alarm system documentation for explanation).
	 * @param code an int indicating the 'code' of the alarm (see alarm system documentation for explanation).
	 * @param active a boolean indicating if the alarm is active (true) or not (false)
	 * @param faultProperties user-defined properties associated with the alarm.
	 * @param sourceName the source name, defaults to ALARM_SOURCE_NAME constant defined in "asiConfigurationConstants.h"
	 * @throw ACSASFactoryNotInitedExImpl if the alarm system has not been previously initialized.
	 */
	static void createAndSendAlarm(std::string & faultFamily, std::string & faultMember, int faultCode, bool active, acsalarm::Properties & faultProperties, 
		std::string sourceName = asiConfigurationConstants::ALARM_SOURCE_NAME) throw (acsErrTypeAlarmSourceFactory::ACSASFactoryNotInitedExImpl);

};

#endif /*!ACS_ALARM_SYSTEM_INTERFACE_FACTORY_H*/

