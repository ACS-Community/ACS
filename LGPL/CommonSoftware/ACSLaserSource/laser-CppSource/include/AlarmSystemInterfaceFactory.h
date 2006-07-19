#ifndef ALARM_SYSTEM_INTERFACE_FACTORY_H
#define ALARM_SYSTEM_INTERFACE_FACTORY_H

#include "FaultState.h"
#include "AlarmSystemInterface.h"
#include "AlarmSystemInterfaceProxy.h"
#include <asiConfigurationConstants.h>
#include "maciS.h"

using asiConfigurationConstants::ALARM_SOURCE_NAME;

namespace laserSource
{
	/**
	 * Factory class for creating new instances of alarm system interface.
	 * @author  sharring
	 * Based on CERN java class of the same name.
	 */
	class AlarmSystemInterfaceFactory 
	{
		/** Default constructor.
	  	 */
		private:
			AlarmSystemInterfaceFactory();
			virtual ~AlarmSystemInterfaceFactory();
			
			// The manager
			static maci::Manager_ptr m_manager;

		public:

		/** Factory method for creating FaultState instances.
		 * @return a new FaultState instance.
		 *
		 */
		static auto_ptr<FaultState> createFaultState() 
		{
			FaultState * fsPtr = new FaultState();
			auto_ptr<FaultState> fsAutoPtr(fsPtr);
			return fsAutoPtr;
		}

		/** Factory method for creating FaultState instances.
		 * @return a new FaultState instance.
		 * @param family the fault family.
		 * @param member the fault member.
		 * @param code the fault code.
		 */
		static auto_ptr<FaultState> createFaultState(string family, string member, int code) 
		{
			FaultState * fsPtr = new FaultState(family, member, code);
			auto_ptr<FaultState> fsAutoPtr(fsPtr);
			return fsAutoPtr;
		}

		/**
	 	 * Create a new instance of an alarm system interface.
		 * @param sourceName the source name.
		 * @return the interface instance.
		 * @throws ASIException if the AlarmSystemInterface instance can not be created.
		 */
		static auto_ptr<AlarmSystemInterface> createSource(string sourceName) //throws ASIException {
		{
			AlarmSystemInterfaceProxy * asIfProxyPtr = new AlarmSystemInterfaceProxy(sourceName);
			auto_ptr<AlarmSystemInterface> asIfAutoPtr(asIfProxyPtr);
			return asIfAutoPtr;
		}

		/**
		 * Create a new instance of an alarm system interface without binding it to any source.
		 * @return the interface instance.
		 * @throws ASIException if the AlarmSystemInterface instance can not be created.
		 */
		static auto_ptr<AlarmSystemInterface> createSource() //throws ASIException {
		{
			return createSource("UNDEFINED");
		}
		
		static bool init(maci::Manager_ptr manager) {
			m_manager=maci::Manager::_duplicate(manager);
		}
		
		static void done() {
			CORBA::release(m_manager);
			m_manager=maci::Manager::_nil();
		}
			
	};
};

#endif
