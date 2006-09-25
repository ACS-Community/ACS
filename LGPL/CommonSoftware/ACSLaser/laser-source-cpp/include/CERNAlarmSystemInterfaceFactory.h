#ifndef CERN_ALARM_SYSTEM_INTERFACE_FACTORY_H
#define CERN_ALARM_SYSTEM_INTERFACE_FACTORY_H

#include "AbstractAlarmSystemInterfaceFactory.h"
#include <logging.h>

namespace laserSourceCERN
{
	class CERNAlarmSystemInterfaceFactory: public AbstractAlarmSystemInterfaceFactory
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
		virtual auto_ptr<laserSource::ACSAlarmSystemInterface> createSource(string sourceName);
		
		/**
		 * Create a new instance of an alarm system interface without binding it to any source.
		 * @return the interface instance.
		 */
		virtual auto_ptr<laserSource::ACSAlarmSystemInterface> createSource();
	
		/**
		 * Create a fault state with the given family, member and code
		 */
		virtual auto_ptr<laserSource::ACSFaultState>createFaultState(string family, string member, int code);
	
		/**
		 * Create a fault state 
		 */
		virtual auto_ptr<laserSource::ACSFaultState>createFaultState();
	};
};
#endif /* ifndef CERN_ALARM_SYSTEM_INTERFACE_FACTORY_H */
