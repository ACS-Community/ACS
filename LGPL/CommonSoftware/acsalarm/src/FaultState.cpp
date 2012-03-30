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
#include "FaultState.h"
#include "utilConstants.h"
#include <sstream>

using std::string;
using std::stringstream;
using std::auto_ptr;
using acsalarm::FaultState;

/**
 * Default Constructor
 */
FaultState::FaultState():
	userProperties(new Properties()),
	userTimestamp(new Timestamp())
{
}

/**
 * Constructor for initializing a fault state with values
 */
FaultState::FaultState(string theFamily, string theMember, int theCode):
	userProperties(new Properties()),
	userTimestamp(new Timestamp())
{
	setFamily(theFamily);
	setMember(theMember);
	setCode(theCode);
}

/**
 * Copy constructor.
 */
FaultState::FaultState(const FaultState & fltState)
{
	*this = fltState;
}

/**
 * Destructor
 */
FaultState::~FaultState()
{
}

/*
 * Assignment operator
 */
FaultState & FaultState::operator=(const FaultState & rhs)
{
	setFamily(rhs.getFamily());
	setCode(rhs.getCode());
	setMember(rhs.getMember());
	setDescriptor(rhs.getDescriptor());
	setActivatedByBackup(rhs.getActivatedByBackup());
	setTerminatedByBackup(rhs.getTerminatedByBackup());

	if(NULL != rhs.userTimestamp.get())
	{
		Timestamp * tstampPtr = new Timestamp(*(rhs.userTimestamp));
		auto_ptr<Timestamp> tstampAptr(tstampPtr);
		setUserTimestamp(tstampAptr);
	}

	if(NULL != rhs.userProperties.get())
	{
		Properties * propsPtr = new Properties(*(rhs.userProperties));
		auto_ptr<Properties> propsAptr(propsPtr);
		setUserProperties(propsAptr);
	}

	return *this;
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
string FaultState::toXML(int amountToIndent)
{
	stringstream ret;

	// generate the fault-state opening element
	// e.g. <fault-state family="AlarmSource" member="ALARM_SOURCE_ANTENNA" code="1">
	for(int x = 0; x < amountToIndent; x++)
	{
		ret << SPACE;
	}

	ret << LESS_THAN_SIGN<<FAULT_STATE_ELEMENT_NAME<<SPACE;

	// output the fault's family
	ret << FAULT_STATE_FAMILY_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<getFamily()<<DOUBLE_QUOTE<<SPACE;

	// output the fault's member
	ret << FAULT_STATE_MEMBER_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<getMember()<<DOUBLE_QUOTE<<SPACE;

	// output the fault's code
	ret << FAULT_STATE_CODE_ATTRIBUTE_NAME<<EQUALS_SIGN<<DOUBLE_QUOTE<<getCode()<<DOUBLE_QUOTE<<GREATER_THAN_SIGN<<std::endl;

	// indent for readability
	for(int x = 0; x < amountToIndent+3; x++)
	{
		ret<<SPACE;
	}
	
	// generate the descriptor element
	// e.g. <descriptor>TERMINATE</descriptor>
	ret<< LESS_THAN_SIGN<<FAULT_STATE_DESCRIPTOR_ELEMENT_NAME<<GREATER_THAN_SIGN;
	ret<< getDescriptor();
	ret<< LESS_THAN_SIGN<<FORWARD_SLASH<<FAULT_STATE_DESCRIPTOR_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	// generate the properties element
	// e.g. 
	//
	// <user-properties>
	// 	<property name="ASI_PREFIX" value="prefix"/>
	// 	<property name="TEST_PROPERTY" value="TEST_VALUE"/>
	//		<property name="ASI_SUFFIX" value="suffix"/>
	// </user-properties>

	if(userProperties.get()!=NULL)
	{
		ret<<userProperties->toXML(amountToIndent+3);
	}

	// generate the user timestamp element
	// e.g. <user-timestamp seconds="1129902763" microseconds="105000"/>

	if(NULL == userTimestamp.get()) {
		// TODO: throw an exception or log an error
	}
	else {
		ret<<userTimestamp->toXML(USER_TIMESTAMP_ELEMENT_NAME, amountToIndent+3);
	}

	// generate the fault-state closing element
	// e.g. </fault-state>
	for(int x = 0; x < amountToIndent; x++)
	{
		ret<<SPACE;
	}
	ret << LESS_THAN_SIGN<<FORWARD_SLASH<<FAULT_STATE_ELEMENT_NAME<<GREATER_THAN_SIGN<<std::endl;

	return ret.str();
}

/** 
  * Fault family accessor method.
  * @param faultFamily the fault family.
  */
void  FaultState::setFamily(const string & faultFamily) {
	size_t pos;
	string nonConstFaultFamily(faultFamily);
	do
	{
		pos = nonConstFaultFamily.find(":");
		if (pos != string::npos)
		{
			nonConstFaultFamily.replace(pos,1,"#");
		}
	}
	while(pos != string::npos);
	family = nonConstFaultFamily;
}

/** 
  * Fault member accessor method.
  * @param member the fault member.
*/
void  FaultState::setMember(const string & newFaultMember) {
	size_t pos;
	string nonConstFaultMember(newFaultMember);
	do 
	{
		pos = nonConstFaultMember.find(":");
		if (pos != string::npos) 
		{
			nonConstFaultMember.replace(pos,1,"#");
		}
	} 
	while(pos != string::npos);
	member = nonConstFaultMember;
}

