/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2016
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
package alma.test.benchmark.components;

import java.util.Collections;
import java.util.List;
import java.util.Vector;

import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import si.ijs.maci.AdministratorOperations;
import si.ijs.maci.AuthenticationData;
import si.ijs.maci.ClientInfo;
import si.ijs.maci.ClientType;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.ImplLangType;

/**
 * <code>ComponentsLoadingAdminOperations</code> is a administrator clients
 * notified (among others) of components activation, deactivation and releasing.
 * <P>
 * The main purpose is to check cnsistency of gets and releasaes of components
 * during the test.
 * 
 * @author acaproni
 * @since ACS 2016.6
 */
public class ComponentsLoadingAdminOperations implements AdministratorOperations {
	
	/**
	 * A data structure to store data about components activated 
	 * 
	 * @author acaproni
	 */
	static class ActivatedComponentInfo {
		// Name of component
		public  final String compName;
		// Container where the component runs
		public final String contName;
		//Handle
		public final int h;
				
		/**
		 * Constructor
		 * @param name Name of the component
		 * @param containerName Container where the component runs
		 * @param h Handle
		 */
		public ActivatedComponentInfo(String name, String containerName, int h) {
			this.compName = name;
			this.contName = containerName;
			this.h = h;
		}
		
	}
	
	// Execution Id.
    private long executionId = 0;
    
    // Start time
    private long startTime = System.currentTimeMillis();
    
    // The components activated
    private final List<ActivatedComponentInfo> activatedComps = Collections.synchronizedList(new Vector<ActivatedComponentInfo>());
    
    // The components deactivated
    private final List<Integer> deactivatedComps = Collections.synchronizedList(new Vector<Integer>());
    
    // The components requested
    private final List<Integer> requestedComps = Collections.synchronizedList(new Vector<Integer>());
    
    // The components requested
    private final List<Integer> releasedComps = Collections.synchronizedList(new Vector<Integer>());
    
    /**
	 * The logger
	 */
	private AcsLogger logger;
	
	public ComponentsLoadingAdminOperations(AcsLogger theLogger) {
		if (theLogger==null) {
			throw new IllegalArgumentException("Invalid null logger");
		}
		this.logger=theLogger;
	}

	@Override
	public String name() {
		return this.getClass().getName();
	}

	@Override
	public void disconnect() {
	}

	@Override
	public AuthenticationData authenticate(long execution_id, String question) {
		if (this.executionId == 0) {
			this.executionId = executionId;
		}
		logger.log(AcsLogLevel.INFO, "Authenticate with id="+execution_id);
		return new AuthenticationData("", ClientType.ADMINISTRATOR_TYPE, ImplLangType.JAVA, true, startTime, this.executionId);
	}

	@Override
	public void message(short type, String message) {
	}

	@Override
	public void taggedmessage(short type, short messageID, String message) {
	}

	@Override
	public boolean ping() {
		return true;
	}

	@Override
	public void components_available(ComponentInfo[] components) {
	}

	@Override
	public void components_unavailable(String[] component_names) {
	}

	@Override
	public void client_logged_in(ClientInfo info, long timestamp, long execution_id) {
	}

	@Override
	public void client_logged_out(int h, long timestamp) {
	}

	@Override
	public void container_logged_in(ContainerInfo info, long timestamp, long execution_id) {
	}

	@Override
	public void container_logged_out(int h, long timestamp) {
	}

	@Override
	public void components_requested(int[] clients, int[] components, long timestamp) {
		for (int comp: components) {
			requestedComps.add(Integer.valueOf(comp));
		}
	}

	@Override
	public void components_released(int[] clients, int[] components, long timestamp) {
		for (int comp: components) {
			releasedComps.add(Integer.valueOf(comp));
		}
	}

	@Override
	public void component_activated(ComponentInfo info, long timestamp, long execution_id) {
		activatedComps.add(new ActivatedComponentInfo(info.name, info.container_name, info.h));
	}

	@Override
	public void component_deactivated(int h, long timestamp) {
		deactivatedComps.add(Integer.valueOf(h));
	}
	
	@Override
	public String toString() {
		StringBuilder outStr = new StringBuilder("Notification from the AdministratorOperations:\n");
		outStr.append('\t');
		outStr.append(requestedComps.size());
		outStr.append(" requested components\n");
		outStr.append('\t');
		outStr.append(activatedComps.size());
		outStr.append(" activated components\n");
		outStr.append('\t');
		outStr.append(deactivatedComps.size());
		outStr.append(" deactivated components\n");
		outStr.append('\t');
		outStr.append(releasedComps.size());
		outStr.append(" released components\n");
		return outStr.toString();
	}
	
	/**
	 * 
	 * @return The number of activated components
	 */
	public int getNumOfActivatedComps() {
		return activatedComps.size();
	}
	
	/**
	 * 
	 * @return The number of deactivated components
	 */
	public int getNumOfDeactivatedComps() {
		return deactivatedComps.size();
	}
	
	/**
	 * 
	 * @return The number of released components
	 */
	public int getNumOfReleasedComps() {
		return releasedComps.size();
	}
	
	/**
	 * 
	 * @return The number of requested components
	 */
	public int getNumOfRequestedComps() {
		return requestedComps.size();
	}
}
