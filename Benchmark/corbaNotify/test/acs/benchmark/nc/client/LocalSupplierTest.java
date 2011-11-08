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
package acs.benchmark.nc.client;

import si.ijs.maci.ComponentSpec;

import acs.benchmark.util.ComponentAccessUtil;
import acs.benchmark.util.ContainerUtil;
import acs.benchmark.util.ContainerUtil.ContainerLogLevelSpec;

import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.benchmark.CHANNELNAME_CONTROL_REALTIME;
import alma.benchmark.CorbaNotifySupplierOperations;
import alma.benchmark.NcEventSpec;
import alma.maci.containerconfig.types.ContainerImplLangType;

/**
 * A simple test that uses the daemon / component test framework
 * directly, without declarative layer on top which probably will be needed
 * for larger test setups.
 * 
 * @author hsommer
 */
public class LocalSupplierTest extends ComponentClientTestCase
{
	public LocalSupplierTest() throws Exception {
		super(LocalSupplierTest.class.getSimpleName());
	}

	public void testOneCompOneChannelOneEventType() throws Exception {
		ContainerUtil containerUtil = null;
		String containerName = "localSupplierContainer1";
		ComponentAccessUtil componentAccessUtil = null; 
		CorbaNotifySupplierOperations supplierComp = null;
		try {
			// run a local container
			containerUtil = new ContainerUtil(getContainerServices(), m_acsManagerProxy);
			containerUtil.startContainer(null, ContainerImplLangType.JAVA, containerName, null);
			// @TODO sync on container start
			Thread.sleep(4000);
			m_logger.info("Assuming that container " + containerName + " is ready.");
			
			// configure container log levels
			containerUtil.loginToManager();
			ContainerLogLevelSpec contLogLevelSpec = new ContainerLogLevelSpec(AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.DEBUG);
			contLogLevelSpec.addNamedLoggerSpec("jacorb@"+containerName, AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.WARNING);
			containerUtil.setContainerLogLevels(containerName, contLogLevelSpec);
			assertTrue(containerUtil.isContainerLoggedIn(containerName));
			
			// Create dynamic supplier component
			componentAccessUtil = new ComponentAccessUtil(getContainerServices());
			ComponentSpec compSpec = createJavaSupplierComponentSpec("JavaSupplier-1", containerName);
			supplierComp = componentAccessUtil.getDynamicComponent(compSpec, CorbaNotifySupplierOperations.class);
			
			// supplier setup
			String[] ncNames = new String[] {CHANNELNAME_CONTROL_REALTIME.value};
			supplierComp.ncConnect(ncNames);
			m_logger.info("Connected to NC " + ncNames[0]);
			
			NcEventSpec[] ncEventSpecs = new NcEventSpec[] {
					new NcEventSpec(ncNames[0], 
					new String[] {"MountStatusData"}, 
					"") }; // don't care about antenna name here
			
			int callTimeInternalMillis = supplierComp.sendEvents(ncEventSpecs, -1, 10);
			m_logger.info("Single supplier comp '" + compSpec.component_name + "' sent 10 MountStatusData events at max speed to '" 
					+ ncNames[0] + "' in " + callTimeInternalMillis + " ms.");
		} catch (Exception ex) {
			throw ex;
		} finally {
			// clean up
			if (supplierComp != null) supplierComp.ncDisconnect();
			if (componentAccessUtil != null) componentAccessUtil.releaseAllComponents();
			if (containerUtil != null) {
				containerUtil.stopContainer(null, containerName);
				containerUtil.logoutFromManager();
			}
		}
	}
	
	/**
	 * Helper for tests that create dynamic supplier components.
	 * <p>
	 * @TODO: Find better class to put this in.
	 */
	public static ComponentSpec createJavaSupplierComponentSpec(String componentName, String containerName) {
		ComponentSpec compSpec = new ComponentSpec(componentName, 
				"IDL:alma/benchmark/CorbaNotifySupplier:1.0", 
				"acs.benchmark.nc.supplier.CorbaNotifySupplierComponentHelper", 
				containerName);
		return compSpec;
	}
}
