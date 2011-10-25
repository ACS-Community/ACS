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

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.genfw.runtime.sm.AcsState;
import alma.acs.logging.AcsLogger;

/**
 * Abstract class for substates of composite state 'Offline'.
 */
public abstract class OfflineSubStateAbstract implements AcsState 
{
	protected final AcsLogger logger;
	protected AlmaSubsystemContext m_superContext;
	protected OfflineState m_offlineContext;

    public OfflineSubStateAbstract(AlmaSubsystemContext superContext, OfflineState offlineContext, AcsLogger logger) {
    	this.logger = logger;
        m_superContext = superContext;
        m_offlineContext = offlineContext;
    }

	public abstract AcsState[] getStateHierarchy();
	public abstract String stateName();

    public abstract void entry();
    
    public void initPass1() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "initPass1");
    }

    public void initPass2() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "initPass2");
    }

    public void shutdownPass2() throws AcsJIllegalStateEventEx {
		m_superContext.illegalEvent(stateName(), "shutdownPass2");
    }

}
