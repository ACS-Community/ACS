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
package acs.benchmark.util;

import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import junit.framework.Assert;
import junit.framework.AssertionFailedError;

import org.omg.CORBA.ORB;

import si.ijs.maci.Administrator;
import si.ijs.maci.AdministratorPOATie;
import si.ijs.maci.ComponentInfo;
import si.ijs.maci.ContainerInfo;
import si.ijs.maci.LoggingConfigurableHelper;
import si.ijs.maci.LoggingConfigurableOperations;
import si.ijs.maci.LoggingConfigurablePackage.LogLevels;

import alma.ACSErrTypeCommon.BadParameterEx;
import alma.ACSErrTypeCommon.IllegalArgumentEx;
import alma.JavaContainerError.wrappers.AcsJContainerEx;
import alma.acs.container.AcsManagerProxy;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogger;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.ACSPorts;
import alma.acs.util.AcsLocations;
import alma.acsdaemon.ContainerDaemonHelper;
import alma.acsdaemon.ContainerDaemonOperations;
import alma.acsdaemonErrType.FailedToStartContainerEx;
import alma.acsdaemonErrType.FailedToStopContainerEx;
import alma.maci.containerconfig.types.ContainerImplLangType;
import alma.maciErrType.LoggerDoesNotExistEx;
import alma.maciErrType.NoPermissionEx;


/**
 * Provides direct container access to test code.
 * <p>
 * The constructors only store parameters. You must call {@link #loginToManager()} explicitly 
 * before using the other methods.  * Make sure to call {@link #logoutFromManager()} when done.
 * <p>
 * Some methods use JUnit asserts from {@link Assert} which means that runtime exceptions such as 
 * {@link AssertionFailedError} may be thrown.
 * <p>
 * TODO: Merge with similar class in ACS/LGPL/CommonSoftware/containerTests/contLogTest 
 *       (which builds before this, but does not get installed).
 * 
 * @author hsommer
 */
public class ContainerUtil
{
	protected ContainerServices containerServices;
	protected AcsLogger logger;
	protected AcsManagerProxy adminProxy;
	protected boolean loggedInToManager;
	protected ORB orb;

	
	/////////////////////////////////////////////////////////////////////
	// c'tor stuff
	/////////////////////////////////////////////////////////////////////
	
	/**
	 * Constructor for use as stand-alone application.
	 * 
	 * @param cs
	 * @param managerLoc
	 */
	public ContainerUtil(ContainerServices cs) {
		String managerLoc = AcsLocations.figureOutManagerLocation();
		cs.getLogger().fine("Will use managerLoc='" + managerLoc + "'.");
		init(cs, new AcsManagerProxy(managerLoc, cs.getAdvancedContainerServices().getORB(), cs.getLogger()));
	}
	
	
	/**
	 * Constructor for use in a ComponentClientTestCase.
	 * <p>
	 * Allows access to the manager as an administrator client, on top of the existing manager connection 
	 * from <code>managerProxy</code>;
	 * @param cs  
	 * @param managerProxy  As inherited from ComponentClientTestCase
	 * @see ManagerAdminClient
	 */
	public ContainerUtil(ContainerServices cs, AcsManagerProxy managerProxy) {
		Assert.assertNotNull(managerProxy);
		init(cs, managerProxy.createInstance());
	}
	
	/**
	 * Common to both constructors.
	 * @param cs
	 * @param adminProxy
	 */
	protected void init(ContainerServices cs, AcsManagerProxy adminProxy) {
		Assert.assertNotNull(cs);
		Assert.assertNotNull(adminProxy);
		this.containerServices = cs;
		this.logger = cs.getLogger();
		this.adminProxy = adminProxy;
		loggedInToManager = false;
		orb = cs.getAdvancedContainerServices().getORB();
	}
	
	
	/////////////////////////////////////////////////////////////////////
	// Manager login 
	/////////////////////////////////////////////////////////////////////
	
	public void loginToManager() throws AcsJContainerEx {
		if (!loggedInToManager) {
			AdministratorPOATie adminpoa = new AdministratorPOATie(new ManagerAdminClient(containerServices.getName(), logger));
			Administrator adminCorbaObj = adminpoa._this(orb);
			adminProxy.loginToManager(adminCorbaObj, false);
			int adminManagerHandle = adminProxy.getManagerHandle();
			Assert.assertTrue(adminManagerHandle > 0);
			loggedInToManager = true;
		}
	}
	
	public void logoutFromManager() {
		if (loggedInToManager) {
			adminProxy.logoutFromManager();
			loggedInToManager = false;
		}
	}

	
	/////////////////////////////////////////////////////////////////////
	// Container access 
	/////////////////////////////////////////////////////////////////////
	
	public List<ContainerInfo> getAllContainerInfos() throws NoPermissionEx, AcsJContainerEx {
		if (!loggedInToManager) {
			throw new IllegalStateException("must be logged in to the manager.");
		}
		ContainerInfo[] containerInfos = adminProxy.getManager().get_container_info(adminProxy.getManagerHandle(), new int[0], "*");
		return Arrays.asList(containerInfos);
	}

	/**
	 * Asks the manager if the specified container is logged in. 
	 * @param containerName
	 * @return true if container is logged in.
	 * @throws AcsJContainerEx
	 * @throws NoPermissionEx
	 */
	public boolean isContainerLoggedIn(String containerName) throws AcsJContainerEx, NoPermissionEx {
		if (!loggedInToManager) {
			throw new IllegalStateException("must be logged in to the manager.");
		}
		ContainerInfo[] containerInfos = 
			adminProxy.getManager().get_container_info(adminProxy.getManagerHandle(), new int[0], containerName);
		Assert.assertTrue("Expected 0 or 1 container of name " + containerName + " but found " + containerInfos.length, 
				containerInfos.length <= 1);
		
		return (containerInfos.length == 1);
	}
	
	/**
	 * Gets a reference to the LoggingConfigurableOperations interface of a container with a given name. 
	 * <p>
	 * Note that only in test code like here we are allowed to talk directly with the manager.
	 * For operational code, the ContainerServices methods must be used and extended if necessary.
	 *  
	 * @param containerName
	 */
	public LoggingConfigurableOperations getContainerLoggingIF(String containerName) throws AcsJContainerEx, NoPermissionEx {
		for (ContainerInfo containerInfo : getAllContainerInfos()) {
			if (containerInfo.name.equals(containerName)) {
				return LoggingConfigurableHelper.narrow(containerInfo.reference);
			}
		}
		Assert.fail("No container '" + containerName + "' found.");
		return null; // to appease the compile which does not see the terminal nature of Assert.fail
	}
	
	
	/**
	 * Complete logging spec for a container.
	 */
	public static class ContainerLogLevelSpec {
		private AcsLogLevelDefinition defaultMin;
		private AcsLogLevelDefinition defaultMinLocal;
		private Map<String, AcsLogLevelDefinition[]> namedLoggerMap = new HashMap<String, AcsLogLevelDefinition[]>();
		public ContainerLogLevelSpec(AcsLogLevelDefinition defaultMin, AcsLogLevelDefinition defaultMinLocal) {
			this.defaultMin = defaultMin;
			this.defaultMinLocal = defaultMinLocal;
		}
		public void addNamedLoggerSpec(String name, AcsLogLevelDefinition levelMin, AcsLogLevelDefinition levelMinLocal) {
			AcsLogLevelDefinition[] levels = new AcsLogLevelDefinition[] {levelMin, levelMinLocal};
			namedLoggerMap.put(name, levels);
		}
		public void configure(LoggingConfigurableOperations target) {
			LogLevels defaultLogLevels = new LogLevels(false, (short)defaultMin.value, (short)defaultMinLocal.value);
			try {
				target.set_default_logLevels(defaultLogLevels);
			} catch (IllegalArgumentEx ex) {
				ex.printStackTrace();
			}
			
			for (String loggerName : namedLoggerMap.keySet()) {
				AcsLogLevelDefinition[] levels = namedLoggerMap.get(loggerName);
				LogLevels namedLogLevels = new LogLevels(false, (short)levels[0].value, (short)levels[1].value);
				try {
					target.set_logLevels(loggerName, namedLogLevels);
				} catch (LoggerDoesNotExistEx ex) {
					ex.printStackTrace();
				} catch (IllegalArgumentEx ex) {
					ex.printStackTrace();
				}
			}
		}
	}
	
	/**
	 * Dynamically configures container log levels, using {@link #getContainerLoggingIF(String)}.
	 * @param containerName
	 * @param defaultConfig
	 * @param namedLoggerConfigs
	 */
	public void setContainerLogLevels(String containerName, ContainerLogLevelSpec spec) throws AcsJContainerEx, NoPermissionEx {
		LoggingConfigurableOperations loggingConfigurable = getContainerLoggingIF(containerName);
		spec.configure(loggingConfigurable);
	}
	
	/**
	 * Gets the name of the container running (or configured to run) the component of the given name.
	 */
	public String resolveContainerName(String componentName) throws AcsJContainerEx, NoPermissionEx {
		if (!loggedInToManager) {
			throw new IllegalStateException("must be logged in to the manager.");
		}
		ComponentInfo[] componentInfos = adminProxy.getManager().get_component_info(adminProxy.getManagerHandle(), new int[0], componentName, "*", false);
		Assert.assertNotNull(componentInfos);
		Assert.assertEquals("Exactly one match for component name '" + componentName + "' expected.", 1, componentInfos.length);
		String containerName = componentInfos[0].container_name;
		Assert.assertNotNull(containerName);
		return containerName;
	}

	
	/////////////////////////////////////////////////////////////////////
	// Container daemon access 
	/////////////////////////////////////////////////////////////////////
	
	/**
	 * @TODO possibly cache the daemon references for the various host machines.
	 * @param host
	 * @return
	 */
	public ContainerDaemonOperations getContainerDaemon(String host) {
		String daemonCORBALOC = AcsLocations.convertToContainerDaemonLocation(host);
		ContainerDaemonOperations daemon = null;
		try {
			org.omg.CORBA.Object obj = orb.string_to_object(daemonCORBALOC);
			daemon = ContainerDaemonHelper.narrow(obj);
			if (daemon == null) {
				throw new NullPointerException("Daemon object was null");
			}
		} catch (Throwable thr) {
			throw new RuntimeException("Failed to resolve daemon reference for " + daemonCORBALOC, thr);
		}

		return daemon;
	}
	
	/**
	 * Starts (possibly remote) containers using container daemons. 
	 * <p>
	 * Note that outside of performance tests it is required to start containers either through the OMC
	 * or by the manager in case of CDB-configured autostart containers. 
	 * Here we do it from application code in order to run flexible tests that don't require a CDB setup.
	 * @param host
	 * @param containerType
	 * @param containerName
	 * @param flags
	 * @throws FailedToStartContainerEx 
	 * @throws BadParameterEx 
	 */
	public void startContainer(String host, ContainerImplLangType containerType, String containerName, String flags) throws BadParameterEx, FailedToStartContainerEx {
		if (host == null || host.isEmpty()) {
			host = ACSPorts.getIP();
		}
		if (flags == null) {
			flags = "";
		}
		ContainerDaemonOperations daemon = getContainerDaemon(host);
		String containerTypeName = containerType.toString(); // TODO check that string is as expected by daemon, e.g. "py" vs. "python"
		short instanceNumber = (short) ACSPorts.getBasePort();
		daemon.start_container(containerTypeName, containerName, instanceNumber, new String[0], flags);
	}

	public void stopContainer(String host, String containerName) throws BadParameterEx, FailedToStopContainerEx {
		if (host == null || host.isEmpty()) {
			host = ACSPorts.getIP();
		}
		ContainerDaemonOperations daemon = getContainerDaemon(host);
		short instanceNumber = (short) ACSPorts.getBasePort();
		daemon.stop_container(containerName, instanceNumber, "");
	}
}
