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

