#ifndef FAULTSTATE_H_
#define FAULTSTATE_H_

#include <iostream>	
#include <memory>
#include "Properties.h"
#include "Timestamp.h"
#include "ACSFaultState.h"

using std::string;
using laserUtil::Properties;
using laserUtil::Timestamp;

namespace laserSource 
{
	/*
	 * Class representing a single fault state for use by cpp alarm source clients 
	 * which wish to send an alarm to the laser alarm server.
	 */
	class FaultState: public ACSFaultState
	{	
		public:

			/**
			 * Default constructor, values must be subsequently initialized using setters
			 */
			FaultState();

			/**
			 * Copy constructor.
			 */
			FaultState(const FaultState &);

			/**
			 * Constructor for initializing a fault state with values
			 */
			FaultState(string family, string member, int code);

			/*
			 * Destructor
			 */
			virtual ~FaultState();

			FaultState & operator=(const FaultState & rhs);
 
			/**
 			 * Returns an XML representation of the fault state. NOTE: this 
 			 * will not be a complete XML document, but just a fragment.
 			 *
			 * @param amountToIndent the amount (in spaces) to indent for readability
			 *
 			 * For example:
 			 *
 			 * <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
 			 *     <descriptor>TERMINATE</descriptor>
 			 *     <user-properties>
 			 *        <property name="ASI_PREFIX" value="prefix"/>
 			 *        <property name="TEST_PROPERTY" value="TEST_VALUE"/>
 			 *        <property name="ASI_SUFFIX" value="suffix"/>
 			 *     </user-properties>
 			 *     <user-timestamp seconds="1129902763" microseconds="105000"/>
 			 *  </fault-state>
 			 */
			virtual string toXML(int amountToIndent = 3);

			/** User properties accessor method.
			 * @param properties the user properties.
			 */
			virtual void setUserProperties(auto_ptr<Properties> theProperties) { userProperties = theProperties; }

			/** User properties accessor method.
			 * @return Properties the user properties.
			 */
			virtual Properties & getUserProperties() { return *userProperties; }

			/** Timestamp accessor method.
			 * @param timestamp the timestamp.
			 */
			virtual void setUserTimestamp(auto_ptr<Timestamp> theTimestamp) { userTimestamp = theTimestamp; }

			/** Timestamp accessor method.
			 * @return long the timestamp.
			 */
			virtual Timestamp & getUserTimestamp() { return *userTimestamp; }

			virtual bool getActivatedByBackup() { return activatedByBackup; }
			virtual void setActivatedByBackup(bool newActivatedByBackup) { activatedByBackup = newActivatedByBackup; }

			virtual bool getTerminatedByBackup() { return terminatedByBackup; }
			virtual void setTerminatedByBackup(bool newTerminatedByBackup) { terminatedByBackup = newTerminatedByBackup; }

		private:

			bool activatedByBackup; 
			bool terminatedByBackup;
			auto_ptr<Properties> userProperties;
			auto_ptr<Timestamp> userTimestamp;
	};
};
#endif /*FAULTSTATE_H_*/
