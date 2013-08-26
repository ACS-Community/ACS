/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.ACS.SpecialTestMasterComponentImpl;

import java.util.logging.Logger;

import alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions;


/**
 * Here we use the option of having a separate class for the state machine actions
 * 
 * @author hsommer
 * created Apr 30, 2004 1:32:30 PM
 */
class SpecialTestSubsystemActions implements AlmaSubsystemActions
{
	private Logger m_actionLogger;

	SpecialTestSubsystemActions(Logger actionLogger)
	{
		m_actionLogger = actionLogger;
	}
	
	public void initSubsysPass1() {
		m_actionLogger.info("action method 'initSubsysPass1' called.");
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions#initSubsysPass2()
	 */
	public void initSubsysPass2() {
		m_actionLogger.info("action method 'initSubsysPass2' called.");
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions#reinitSubsystem()
	 */
	public void reinitSubsystem() {
		m_actionLogger.info("action method 'reinitSubsystem' called.");
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions#shutDownSubsysPass1()
	 */
	public void shutDownSubsysPass1() {
		m_actionLogger.info("action method 'shutDownSubsysPass1' called.");
	}

	/**
	 * @see alma.ACS.MasterComponentImpl.statemachine.AlmaSubsystemActions#shutDownSubsysPass2()
	 */
	public void shutDownSubsysPass2() {
		m_actionLogger.info("action method 'shutDownSubsysPass2' called.");
	}
}
