#ifndef FAULTSTATE_H_
#define FAULTSTATE_H_

/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
#include <iostream>	
#include <memory>
#include "Properties.h"
#include "Timestamp.h"

namespace acsalarm 
{
	/*
	 * Class representing a single fault state for use by cpp alarm source clients 
	 * which wish to send an alarm to the laser alarm server.
	 */
	class FaultState
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
			FaultState(std::string family, std::string member, int code);

			/*
			 * Destructor
			 */
			virtual ~FaultState();

			FaultState & operator=(const FaultState & rhs);
 
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
			void setDescriptor(const std::string & newDescriptor) {
				descriptor=newDescriptor;
			}

			/** Fault descriptor accessor method.
			 * @return string the fault descriptor.
			 */
			std::string getDescriptor() const {
				return descriptor;
			}

			/** 
			 * Fault family accessor method.
			 * @return the fault family.
			 */
			std::string getFamily() const {
				return family;
			}
	 
			/** 
			 * Fault member accessor method.
			 * @return the fault member.
			 */
			std::string getMember() const {
				return member;
			}


			/** 
			  * Fault family accessor method.
			  * @param faultFamily the fault family.
			  */
			void  setFamily(const std::string & faultFamily);

			/** 
			  * Fault member accessor method.
			  * @param member the fault member.
			*/
			void  setMember(const std::string & newFaultMember);

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
			virtual std::string toXML(int amountToIndent = 3);

			/** User properties accessor method.
			 * @param properties the user properties.
			 */
			virtual void setUserProperties(std::auto_ptr<acsalarm::Properties> theProperties) { userProperties = theProperties; }

			/** User properties accessor method.
			 * @return Properties the user properties.
			 */
			const virtual acsalarm::Properties & getUserProperties() const { return *userProperties; }

			/** Timestamp accessor method.
			 * @param timestamp the timestamp.
			 */
			virtual void setUserTimestamp(std::auto_ptr<acsalarm::Timestamp> theTimestamp) { userTimestamp = theTimestamp; }

			/** Timestamp accessor method.
			 * @return long the timestamp.
			 */
			const virtual acsalarm::Timestamp & getUserTimestamp() const { return *userTimestamp; }

			virtual bool getActivatedByBackup() const { return activatedByBackup; }
			virtual void setActivatedByBackup(bool newActivatedByBackup) { activatedByBackup = newActivatedByBackup; }

			virtual bool getTerminatedByBackup() const { return terminatedByBackup; }
			virtual void setTerminatedByBackup(bool newTerminatedByBackup) { terminatedByBackup = newTerminatedByBackup; }

		private:

			std::string member;
			std::string family;
			std::string descriptor;
			int code;
			bool activatedByBackup; 
			bool terminatedByBackup;
			std::auto_ptr<acsalarm::Properties> userProperties;
			std::auto_ptr<acsalarm::Timestamp> userTimestamp;
	};
};
#endif /*FAULTSTATE_H_*/
