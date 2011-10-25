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
package alma.ACS.MasterComponentImpl.statemachine;

import alma.acs.genfw.runtime.sm.AcsStateActionException;

public interface AlmaSubsystemActions
{
	// todo: enum pattern
//	// action IDs for generic handling by the generated state machine
//	int ACTION_initSubsysPass1 = 0;
//	int ACTION_initSubsysPass2 = 1;
//	int ACTION_reinitSubsystem = 2;
//	int ACTION_shutDownSubsysPass1 = 3;
//	int ACTION_shutDownSubsysPass2 = 4;

	// action methods to be implemented by the state machine user application
	
	void initSubsysPass1() throws AcsStateActionException;

	void initSubsysPass2() throws AcsStateActionException;

	void reinitSubsystem() throws AcsStateActionException;

	void shutDownSubsysPass1() throws AcsStateActionException;

	void shutDownSubsysPass2() throws AcsStateActionException;

}
