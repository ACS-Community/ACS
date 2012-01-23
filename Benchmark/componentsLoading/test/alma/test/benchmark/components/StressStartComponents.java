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
package alma.test.benchmark.components;

import java.util.Calendar;
import java.util.HashSet;
import java.util.Set;
import java.util.Vector;
import java.util.concurrent.CancellationException;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorCompletionService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.FutureTask;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import si.ijs.maci.ComponentSpec;

import acs.benchmark.util.ConcurrentComponentAccessUtil;
import acs.benchmark.util.ConcurrentComponentAccessUtil.InstrumentedFutureTask;
import acs.benchmark.util.ContainerUtil;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogLevel;
import alma.acs.logging.AcsLogger;
import alma.maci.containerconfig.types.ContainerImplLangType;
import alma.testManager.MountOperations;

/**
 * Start components in containers in parallel.
 * <P>
 * The test starts {@link #totComponentsToStart} components in {@link #totContainersToStart} containers.
 * The hosts to start the containers into are described in {@link #containerHostNames}.
 * <P>
 * To allow the test to be run by NRI, {@link #containerHostNames} contains only the
 * local host (i.e. <code>null</code>). 
 * <BR>If you wish to run this test by running containers
 * on different hosts, you have to manually add the host names into {@link #containerHostNames} and
 * start the container daemon on those hosts.
 * 
 * @author acaproni
 *
 */
public class StressStartComponents extends ComponentClientTestCase {
	
	/**
	 * An object to associate each started container to its name and host
	 * 
	 * @author acaproni
	 *
	 */
	private class ContainerData {
		
		public ContainerData(String name, String host) {
			this.name=name;
			this.host=host;
		}
		
		public final String name;
		public final String host;
	}

	/**
	 * Number of components to start
	 */
	private static final int totComponentsToStart = 1000;
	
	/**
	 * Number of conatiners to start
	 */
	private static final int totContainersToStart = 10;
	
	
	/**
	 * Helper class to handle components operations
	 */
	private ConcurrentComponentAccessUtil componentAccessUtil;
	
	/**
	 * Container services
	 */
	private ContainerServices contSvcs; 
	
	/**
	 * The object to manage containers
	 */
	private ContainerUtil containerUtil;
	
	/**
	 * The logger
	 */
	private AcsLogger logger;
	
	/**
	 * The names of the containers successfully started in order to 
	 * shutdown in case of failures.
	 */
	private final Set<ContainerData> startedContainers = new HashSet<ContainerData>();
	
	/**
	 * A trailing string for the component name.
	 * The real component name is build by appending a number to this trailing 
	 * string so that it is possible to better monitor activation and deactivation
	 * of components.
	 * 
	 */
	private final String componentNameTrailing = "Mount_";
	
	/**
	 * A trailing string for the container name.
	 * The real container name is build by appending a number to this trailing 
	 * string so that it is possible to better monitor activation and deactivation
	 * of containers.
	 * 
	 */
	private final String containerNameTrailing = "bilboContainer_";
	
	/**
	 * The test cycles though this host names while activating the containers
	 * i.e. it activates the first container in the first host name, 
	 * the second in the second host and so on. If the container to activate
	 * are more then the host names, it restarts from the beginning. 
	 * <P>
	 * <code>null</code> mean local host.
	 * <P>
	 * To allow NRI to run this test, we set this variable only contains
	 * the local host (i.e. <code>null</code>).
	 */
	private String[] containerHostNames = {
		null,
	};
	
	/**
	 * Constructor 
	 * 
	 * @throws Exception
	 */
	public StressStartComponents() throws Exception {
		super("StressStartComponents");
	}

	@Override
	protected void setUp() throws Exception {
		super.setUp();
		contSvcs=super.getContainerServices();
		assertNotNull(contSvcs);
		componentAccessUtil=new ConcurrentComponentAccessUtil(getContainerServices());
		assertNotNull(componentAccessUtil);
		containerUtil = new ContainerUtil(contSvcs);
		assertNotNull(containerUtil);
		containerUtil.loginToManager();
		logger=contSvcs.getLogger();
		componentAccessUtil.start();
	}

	@Override
	protected void tearDown() throws Exception {
		componentAccessUtil.stop();
		containerUtil.logoutFromManager();
		super.tearDown();
	}
	
	/**
	 * Log a message with the timing
	 * 
	 * @param msg The message to log
	 * @param time Execution time 
	 */
	private void logTime(String msg, long time) {
		Calendar cal = Calendar.getInstance();
		cal.setTimeInMillis(time);
		logger.info(msg+cal.get(Calendar.HOUR_OF_DAY)+':'+cal.get(Calendar.MINUTE)+':'+cal.get(Calendar.SECOND)+'.'+cal.get(Calendar.MILLISECOND)+" in thread "+Thread.currentThread().getName());
	}
	
	/**
	 * Start the containers
	 */
	private void startContainers() throws Exception {
		long startTime=System.currentTimeMillis();
		for (int t=0; t<totContainersToStart; t++) {
			String name = containerNameTrailing+t;
			logger.info("Starting container "+name);
			String hostName;
			if (containerHostNames==null || containerHostNames.length==0) {
				hostName=null;
			} else {
				hostName=containerHostNames[t%containerHostNames.length];
			}
			try {
				containerUtil.startContainer(hostName, ContainerImplLangType.CPP, name, "", true);
				startedContainers.add(new ContainerData(name, hostName));
			} catch (Throwable e) {
				logger.log(AcsLogLevel.ERROR, "Error starting container "+name+" in host "+hostName, e);
				e.printStackTrace();
				throw new Exception(e);
			}
		}
		long endTime=System.currentTimeMillis();
		logTime(totContainersToStart+" containers activated in ", endTime-startTime);
	}
	
	/**
	 * Stop the containers
	 * 
	 * @throws Exception
	 */
	private void stopContainers()  {
		long startTime=System.currentTimeMillis();
		for (ContainerData container: startedContainers) {
			logger.info("Stopping container "+container.name+" in "+container.host);
			try {
				long startTimeContainer=System.currentTimeMillis();
				containerUtil.stopContainer(container.host, container.name);
				long endTimeContainer=System.currentTimeMillis();
				logTime("Container "+container.name+" stopped in ", endTimeContainer-startTimeContainer);
			} catch (Throwable t) {
				logger.severe("Error stopping container "+container.name+": "+t.getMessage());
				t.printStackTrace();
			}
		}
		long endTime=System.currentTimeMillis();
		logTime(totContainersToStart+" containers stopped in ", endTime-startTime);
	}
	
	/**
	 * The test concurrently starts {@link StressStartComponents#totComponentsToStart} components
	 * in {@link StressStartComponents#totThreads} threads.
	 * 
	 * @throws Exception
	 */
	public void testStartComponents() throws Exception {
		startContainers();
		ContainerData[] containers = new ContainerData[startedContainers.size()];
		startedContainers.toArray(containers);
		
		// Start the components
		Vector<Future<MountOperations>> tasks = new Vector<Future<MountOperations>>();
		int c=0; // to cycle through containers in the array
		for (int t=0; t<totComponentsToStart; t++) {
			String name = componentNameTrailing+t;
			String contName = containers[c].name;
			c =(c+1)%containers.length;
			logger.info("Starting "+name);
			
			ComponentSpec spec = new ComponentSpec(
					name, 
					"IDL:alma/testManager/Mount:1.0", 
					"mount", 
					contName);
			tasks.add(componentAccessUtil.getDynamicComponentConcurrent(spec, MountOperations.class));
		}
		
		// wait for the termination of all the tasks
		int n=1;
		logger.info("Waiting termination of "+tasks.size()+" tasks");
		for (Future<MountOperations> ft: tasks) {
			try {
				ft.get(5, TimeUnit.MINUTES);
				if (ft instanceof InstrumentedFutureTask<?>) {
					InstrumentedFutureTask ift=(InstrumentedFutureTask)ft;
					logTime("Component loaded in ", ift.getExecutionTime());
				}
				logger.info("A task got the component. Number of terminated tasks: "+n);
			} catch (ExecutionException ee) {
				logger.severe("Exception returned from task while getting component: "+ee.getMessage());
			} catch (CancellationException ce) {
				logger.warning("Task cancelled while getting component: "+ce.getMessage());
			} catch (TimeoutException te) {
				logger.severe("Timeout from task while getting component: "+te.getMessage());
			} finally {
				n++;
			}
		}
		logger.info("The threads got all the components");
		
		try {
			Thread.sleep(1000);
		} catch (InterruptedException ie) {}
		
		logger.info("Releasing components");
		// release all the components in parallel
		long startTime=System.currentTimeMillis();
		componentAccessUtil.releaseAllComponents(true);
		long endTime=System.currentTimeMillis();
		logTime("Components released in ", endTime-startTime);
		
		try {
			Thread.sleep(1000);
		} catch (InterruptedException ie) {}
		
		logger.info("Stopping containers");
		// Stop containers
		startTime=System.currentTimeMillis();
		stopContainers();
		endTime=System.currentTimeMillis();
		logTime("Containers stopped in ", endTime-startTime);
		
	}
	

}
