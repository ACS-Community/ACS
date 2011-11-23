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
#include "AlarmSystemInterfaceFactory.h"
#include "FaultState.h"

using std::auto_ptr;
using std::string;
using acsalarm::FaultState;

//TODO: add namespace

/** Factory method for creating FaultState instances.
 * @return a new FaultState instance.
 * @param family the fault family.
 * @param member the fault member.
 * @param code the fault code.
 */
auto_ptr<FaultState> AlarmSystemInterfaceFactory::createFaultState(string family, string member, int code)
{
	auto_ptr<FaultState> fsAutoPtr(new FaultState(family, member, code));
	return fsAutoPtr;
}

/**
 * Create a fault state 
 * @return a new FaultState instance
 */
auto_ptr<FaultState> AlarmSystemInterfaceFactory::createFaultState()
{
	auto_ptr<FaultState> fsAutoPtr(new FaultState());
	return fsAutoPtr;
}
