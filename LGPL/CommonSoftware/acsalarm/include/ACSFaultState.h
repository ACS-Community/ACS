#ifndef ACS_FAULT_STATE_H
#define ACS_FAULT_STATE_H
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
* acaproni  2006-08-16  created
*/

/************************************************************************
 *
 *----------------------------------------------------------------------
 */
 
#ifndef __cplusplus
#error This is a C++ include file and cannot be used from plain C
#endif

#include <string>

#include <Properties.h>
#include <Timestamp.h>
#include <iostream>

using namespace std;
using namespace laserUtil;

namespace laserSource
{
	/*
	 * The interface for a FaultState.
	 * It manages the triplet (family, member, code) and the descriptor.
	 * All other methods must be defined in the derived classes.
	 */
	class ACSFaultState 
	{	
		public:

			/**
			 * Default constructor, values must be subsequently initialized using setters
			 */
			ACSFaultState() {
				family=member=descriptor="";
				code=0;
			}

			/**
			 * Copy constructor.
			 */
			ACSFaultState(ACSFaultState & fltState) {
				*this=fltState;
			}

			/**
			 * Constructor for initializing a fault state with values
			 */
			ACSFaultState(string theFamily, string theMember, int theCode) {
				this->setFamily(theFamily);
				this->setMember(theMember);
				this->setCode(theCode);
			}

			/*
			 * Destructor
			 */
			virtual ~ACSFaultState() {}

			ACSFaultState & operator=(ACSFaultState & rhs) {
				family=rhs.getFamily();
				code=rhs.getCode();
				member=rhs.getMember();
				descriptor=rhs.getDescriptor();
				return (*this);
			}
 
			/**
 			 * Returns an XML representation of the fault state. 
 			 */
			virtual string toXML(int amountToIndent = 3)=0;

			/** 
			 * Fault code accessor method.
			 * @param faultCode the fault code.
			 */
			void setCode(int faultCode) {
				code=faultCode;
			}

			/** 
			 * Fault code accessor method.
			 * @return the fault code.
			 */
			int getCode() {
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
			string getDescriptor() {
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
			string getFamily() {
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
			string getMember() {
				return member;
			}

			/** 
			 * User properties accessor method.
			 * @param properties the user properties.
			 */
			virtual void setUserProperties(auto_ptr<Properties> theProperties) =0;

			/** 
			 * User properties accessor method.
			 * @return Properties the user properties.
			 */
			virtual Properties & getUserProperties() =0;

			/** 
			 * Timestamp accessor method.
			 * @param timestamp the timestamp.
			 */
			virtual void setUserTimestamp(auto_ptr<Timestamp> theTimestamp)=0;

			/** 
			 * Timestamp accessor method.
			 * @return long the timestamp.
			 */
			virtual Timestamp & getUserTimestamp()=0;

			virtual bool getActivatedByBackup() =0;

			virtual void setActivatedByBackup(bool newActivatedByBackup) =0;

			virtual bool getTerminatedByBackup() =0;

			virtual void setTerminatedByBackup(bool newTerminatedByBackup) =0;
		
		private:
			string member;
			string family;
			string descriptor;
			int code;

	};
};
#endif
