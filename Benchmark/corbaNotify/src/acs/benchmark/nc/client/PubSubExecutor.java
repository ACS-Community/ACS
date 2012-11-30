/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2012
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

import java.io.File;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import acs.benchmark.util.ContainerUtil;
import acs.benchmark.util.ContainerUtil.ContainerLogLevelSpec;

import alma.acs.component.client.ComponentClient;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.pubsubtest.config.ContainerSpecT;
import alma.acs.pubsubtest.config.PubSubInfrastructureSpec;
import alma.acs.pubsubtest.config.PublisherSpecT;
import alma.acs.pubsubtest.config.SimpleLoggingSpecT;
import alma.acs.pubsubtest.config.SubscriberSpecT;
import alma.acs.pubsubtest.config.TerminationSpecT;
import alma.acs.util.AcsLocations;
import alma.maci.containerconfig.types.ContainerImplLangType;

/**
 * Creates and runs publisher and subscriber components as specified 
 * in an XML configuration file that must comply with schema pubSubConfig.xsd.
 * <p>
 * About termination of pub-sub runs:
 * <ol>
 *   <li>The optional xml attribute <code>numberOfEvents</code> can control 
 *       how many events a publisher sends or a subscriber receives.
 *       This is useful only when these times are short, since this PubSubExecutor will 
 *       keep open calls to these components.
 *   <li>If attribute <code>numberOfEvents</code> is missing or has a non-positive value, 
 *       then the PubSubExecutor commands all publisher and subscriber
 *       components to do their work, and the calls to these components return.
 *       This makes sense for longer times.
 *       Therefore, if at least one pub/sub component is configured for an unbounded number of events, 
 *       the method {@link #execute(PubSubScenario, long, TimeUnit)}
 *       must be called with a timeout, so that the pub/sub components can be stopped
 *       and their containers as well.
 * </ol>
 * 
 * 
 * @author hsommer
 */
public class PubSubExecutor extends ComponentClient
{
	private static final String localhostName = "alma-head"; //= null;

	private final ContainerUtil containerUtil;
	
	private final PubSubComponentAccessUtil componentAccessUtil;

	private final SimpleLoggingSpecT defaultLoggingSpec;

	/**
	 * Constructor, which is independent of a concrete pub-sub scenario.
	 * This allows one <code>PubSubExecutor</code> to execute more than one scenario.
	 * 
	 * @throws Exception
	 */
	public PubSubExecutor() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), PubSubExecutor.class.getSimpleName());
		
		containerUtil = new ContainerUtil(getContainerServices());
		containerUtil.loginToManager();
		
		componentAccessUtil = new PubSubComponentAccessUtil(getContainerServices());
		
		defaultLoggingSpec = new SimpleLoggingSpecT();
		defaultLoggingSpec.setDefaultLevelMin(4);
		defaultLoggingSpec.setDefaultLevelMinLocal(4);
		defaultLoggingSpec.setJacorbLevelMin(4);
	}

	/**
	 * This method must be called when done, so that this <code>PubSubExecutor</code>
	 * can release resources.
	 * @see alma.acs.component.client.ComponentClient#tearDown()
	 */
	public void tearDown() throws Exception {
		if (componentAccessUtil != null) {
			componentAccessUtil.releaseAllComponents(true);
		}
		
		if (containerUtil != null) {
			// TODO: create and call a new method containerUtil.stopAllContainers
			containerUtil.logoutFromManager();
		}
		super.tearDown();
	}

	/**
	 * Run the specified pub/sub container and components.
	 * Since no timeout is passed to this method, one of the following must be true:
	 * <ul>
	 *   <li>The XML spec contains the (optional) <code>Termination</code> element
	 *       that defines a timeout, or
	 *   <li>No pub/sub component has an unbounded number of events, which means 
	 *       that all component definitions must contain the <code>numberOfEvents</code> attribute.
	 * </ul>
	 * @param scenario
	 * @throws Throwable
	 */
	public void execute(PubSubScenario scenario) throws Throwable {
		PubSubInfrastructureSpec pubSubSpec = scenario.getSpec();

		TerminationSpecT teminationSpec = pubSubSpec.getTermination();
		if (teminationSpec == null) {
			for (PublisherSpecT pubSpec : pubSubSpec.getPublisher()) {
				if (!pubSpec.hasNumberOfEvents() || pubSpec.getNumberOfEvents() <= 0) {
					throw new IllegalArgumentException("Publisher " + pubSpec.getComponentName() 
							+ " needs numberOfEvents >= 0, or call execute with a timeout.");
				}
			}
			for (SubscriberSpecT subSpec : pubSubSpec.getSubscriber()) {
				if (!subSpec.hasNumberOfEvents() || subSpec.getNumberOfEvents() <= 0) {
					throw new IllegalArgumentException("Subscriber " + subSpec.getComponentName() 
							+ " needs numberOfEvents >= 0, or call execute with a timeout.");
				}
			}
			// timeout is implied by the finite number of events
			execute(scenario, -1, null);
		}
		else {
			// use timeout from XML
			long timeout = teminationSpec.getTimeout();
			TimeUnit timeUnit = TimeUnit.valueOf(teminationSpec.getTimeUnit().toString());
			m_logger.fine("Will use timeout from the XML spec: timeout=" + timeout + ", timeUnit=" + timeUnit);
			execute(scenario, timeout, timeUnit);
		}
	}

	/**
	 * Run the specified pub/sub container and components.
	 * If this method gets called directly (and not by {@link #execute(PubSubScenario)}, 
	 * the executionTime parameter can overwrite the timeout setting from the XML, or can 
	 * set a timeout complementary to a finite <code>numberOfEvents</code>.
	 */
	public void execute(PubSubScenario scenario, long executionTime, TimeUnit timeUnit) throws Throwable {
		if (executionTime <= 0) {
			throw new IllegalArgumentException("executionTime must be >= 0");
		}
		
		m_logger.info("execute!");
		
		PubSubInfrastructureSpec pubSubSpec = scenario.getSpec();
		
		SimpleLoggingSpecT loggingSpec = ( pubSubSpec.getLogging() != null ? pubSubSpec.getLogging() : defaultLoggingSpec );
		AcsLogLevelDefinition levelDefaultLocal = AcsLogLevelDefinition.fromInteger(loggingSpec.getDefaultLevelMinLocal());
		AcsLogLevelDefinition levelDefault = AcsLogLevelDefinition.fromInteger(loggingSpec.getDefaultLevelMin());
		AcsLogLevelDefinition levelJacORB = AcsLogLevelDefinition.fromInteger(loggingSpec.getJacorbLevelMin());

		List<Throwable> errors = new ArrayList<Throwable>();
		
		try {
			for (ContainerSpecT containerSpec : pubSubSpec.getContainer()) {
				// start containers sequentially (TODO: with thread pool)
				String host = ( containerSpec.getHostName() != null ? containerSpec.getHostName() : localhostName );
				String containerName = containerSpec.getContainerName();
				m_logger.fine("about to start container " + containerName + " on host " + (host==null ? "localhost" : host));
				ContainerImplLangType implLang = ContainerImplLangType.valueOf(containerSpec.getImplLang().toString());
				containerUtil.startContainer(host, implLang, containerName, null, true);
				
				// configure container log levels
				ContainerLogLevelSpec contLogLevelSpec = new ContainerLogLevelSpec(levelDefault, levelDefaultLocal);
				contLogLevelSpec.addNamedLoggerSpec("jacorb@"+containerName, levelJacORB, levelJacORB);
				containerUtil.setContainerLogLevels(containerName, contLogLevelSpec);
				
				m_logger.info("started container " + containerName + " on host " + (host==null ? "localhost" : host));
			}
		} catch (Throwable thr) {
			errors.add(thr);
		}
		
		// TODO: components
		if (errors.isEmpty()) {
			
		}
		
		// wait
		if (errors.isEmpty() && executionTime > 0) {
			try {
				m_logger.info("Will sleep for " + executionTime + " " + timeUnit.toString() + "...");
				timeUnit.sleep(executionTime);
			} catch (Exception ex) {
				errors.add(ex);
			}
		}
		
		// cleanup
		m_logger.info("Will clean up the pub/sub components...");
		
		// todo: interrupt and release components
		
		for (ContainerSpecT containerSpec : pubSubSpec.getContainer()) {
			try {
				String host = ( containerSpec.getHostName() != null ? containerSpec.getHostName() : localhostName );
				String containerName = containerSpec.getContainerName();
				m_logger.fine("about to stop container " + containerName + " on host " + (host==null ? "localhost" : host));
				containerUtil.stopContainer(host, containerName);
				m_logger.info("stopped container " + containerName + " on host " + (host==null ? "localhost" : host));
			} catch (Throwable thr) {
				errors.add(thr);
			}
		}
		
		if (!errors.isEmpty()) {
			throw errors.get(0);
		}
	}

	
	/**
	 * Parameters:
	 * <ol>
	 *   <li>XML config file name.	
	 *   <li>Optional container-host mapping in the form <code>container1=host1:container2=host2</code>.
	 * </ol>
	 * @param args 
	 */
	public static void main(String[] args) {
		if (args.length < 1 || args.length > 2) {
			throw new IllegalArgumentException("Expecting 1 or 2 arguments: <XML config file> [<container1=host1:>]");
		}
		
		PubSubExecutor exec = null;
		try {
			exec = new PubSubExecutor();
			Logger logger = exec.getContainerServices().getLogger();
			
			File xmlFile = new File(args[0]);
			PubSubScenario scenario = new PubSubScenario(logger, xmlFile, true);
			if (args.length == 2) {
				scenario.setHostNames(args[1]);
			}
			else {
				logger.fine("No container-host mapping specified, will use localhost for all containers.");
			}
			
			exec.execute(scenario);
			
		} catch (Throwable thr) {
			thr.printStackTrace();
		} finally {
			if (exec != null) {
				try {
					exec.tearDown();
				} catch (Exception ex) {
					ex.printStackTrace();
				}
			}
		}
	}

}
