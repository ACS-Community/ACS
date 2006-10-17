#ifndef FAULTSTATE_H_
#define FAULTSTATE_H_

#include <iostream>	
#include <memory>
#include "Properties.h"
#include "Timestamp.h"

using std::string;
using acsalarm::Properties;
using acsalarm::Timestamp;

namespace acsalarm 
{
	/*
	 * Class representing a single fault state for use by cpp alarm source clients 
	 * which wish to send an alarm to the laser alarm server.
	 */
	class ACSFaultState
	{	
		public:

			/**
			 * Default constructor, values must be subsequently initialized using setters
			 */
			ACSFaultState();

			/**
			 * Copy constructor.
			 */
			ACSFaultState(const ACSFaultState &);

			/**
			 * Constructor for initializing a fault state with values
			 */
			ACSFaultState(string family, string member, int code);

			/*
			 * Destructor
			 */
			virtual ~ACSFaultState();

			ACSFaultState & operator=(const ACSFaultState & rhs);
 
			/** 
			 * Fault code accessor method.
			 * @param faultCode the fault code.
			 */
			void setCode(const int faultCode) {
				code=faultCode;
			}

			/** 
			 * Fault code accessor method.
			 * @return the fault code.
			 */
			int getCode() const {
				return code;
			}

			/** 
			 * Fault descriptor accessor method.
			 * @param descriptor the fault descriptor.
			 */
			void setDescriptor(const string & newDescriptor) {
				descriptor=newDescriptor;
			}

			/** Fault descriptor accessor method.
			 * @return string the fault descriptor.
			 */
			string getDescriptor() const {
				return descriptor;
			}

			/** 
			 * Fault family accessor method.
			 * @param faultFamily the fault family.
			 */
			void setFamily(const string & faultFamily) {
				family=faultFamily;
			}

			/** 
			 * Fault family accessor method.
			 * @return the fault family.
			 */
			string getFamily() const {
				return family;
			}
	 
			/** 
			 * Fault member accessor method.
			 * @param member the fault member.
			 */
			void setMember(const string & newFaultMember) {
				member=newFaultMember;
			}

			/** 
			 * Fault member accessor method.
			 * @return the fault member.
			 */
			string getMember() const {
				return member;
			}
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
			const virtual Properties & getUserProperties() const { return *userProperties; }

			/** Timestamp accessor method.
			 * @param timestamp the timestamp.
			 */
			virtual void setUserTimestamp(auto_ptr<Timestamp> theTimestamp) { userTimestamp = theTimestamp; }

			/** Timestamp accessor method.
			 * @return long the timestamp.
			 */
			const virtual Timestamp & getUserTimestamp() const { return *userTimestamp; }

			virtual bool getActivatedByBackup() const { return activatedByBackup; }
			virtual void setActivatedByBackup(bool newActivatedByBackup) { activatedByBackup = newActivatedByBackup; }

			virtual bool getTerminatedByBackup() const { return terminatedByBackup; }
			virtual void setTerminatedByBackup(bool newTerminatedByBackup) { terminatedByBackup = newTerminatedByBackup; }

		private:

			string member;
			string family;
			string descriptor;
			int code;
			bool activatedByBackup; 
			bool terminatedByBackup;
			auto_ptr<Properties> userProperties;
			auto_ptr<Timestamp> userTimestamp;
	};
};
#endif /*FAULTSTATE_H_*/
