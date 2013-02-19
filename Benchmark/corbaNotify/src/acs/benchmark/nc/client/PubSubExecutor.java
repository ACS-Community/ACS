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
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import acs.benchmark.util.ContainerUtil;
import acs.benchmark.util.ContainerUtil.ContainerLogLevelSpec;

import alma.ACS.ACSComponentOperations;
import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.pubsubtest.config.ContainerSpecT;
import alma.acs.pubsubtest.config.PubSubInfrastructureSpec;
import alma.acs.pubsubtest.config.PubSubSpecCommonT;
import alma.acs.pubsubtest.config.PublisherSpecT;
import alma.acs.pubsubtest.config.SimpleLoggingSpecT;
import alma.acs.pubsubtest.config.SubscriberSpecT;
import alma.acs.pubsubtest.config.TerminationSpecT;
import alma.acs.pubsubtest.config.types.EventNameT;
import alma.acs.pubsubtest.config.types.ImplLangT;
import alma.acs.util.AcsLocations;
import alma.benchmark.CorbaNotifyCompBaseOperations;
import alma.benchmark.CorbaNotifyConsumerOperations;
import alma.benchmark.CorbaNotifySupplierOperations;
import alma.benchmark.NcEventSpec;
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
 * @author hsommer
 */
public class PubSubExecutor extends ComponentClient
{
	/**
	 * Use values != null to cheat, e.g. when running a test from Eclipse on Windows 
	 * but the containers should be somewhere else.
	 */
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
							+ " needs numberOfEvents >= 0, or call 'execute' with a timeout.");
				}
			}
			for (SubscriberSpecT subSpec : pubSubSpec.getSubscriber()) {
				if (!subSpec.hasNumberOfEvents() || subSpec.getNumberOfEvents() <= 0) {
					throw new IllegalArgumentException("Subscriber " + subSpec.getComponentName() 
							+ " needs numberOfEvents >= 0, or call 'execute' with a timeout.");
				}
			}
			// timeout is implied by the finite number of events
			execute(scenario, -1, null);
		}
		else {
			// use timeout from XML, complementary to any finite number of events if those are specified.
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
	 * set a timeout complementary to a finite <code>PubSubSpecCommonT#numberOfEvents</code>.
	 */
	public void execute(PubSubScenario scenario, long executionTimeMax, TimeUnit executionTimeMaxUnit) throws Throwable {
		PubSubInfrastructureSpec pubSubSpec = scenario.getSpec();
		
		m_logger.info("Will execute test with description:\n" + pubSubSpec.getTestDescription());
		
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
		
		boolean allPubSubHaveFiniteEvents = true;
		ExecutorService pubSubExec = Executors.newCachedThreadPool(getContainerServices().getThreadFactory());
		
		if (errors.isEmpty()) {
			for (final SubscriberSpecT subSpec : pubSubSpec.getSubscriber()) {
				boolean subIsBlocking = isBlocking(subSpec);
				if (!subIsBlocking) {
					allPubSubHaveFiniteEvents = false;
				}
				ImplLangT implLang = deriveComponentImplLang(subSpec, pubSubSpec);
				
				// Get the subscriber component. This may retrieve the same component more than once if it should use more than one NC
				// (which will result in multiple NCSubscriber instances created by that component).
				CorbaNotifyConsumerOperations subscriberComp = 
						componentAccessUtil.getDynamicSubscriberComponent(subSpec.getComponentName(), subSpec.getContainerName(), implLang);
				
				PubSubRunner runner = new PubSubRunner(subSpec, subscriberComp, pubSubExec, m_logger) {
					@Override
					protected Integer callSpecific(NcEventSpec eventSpec, int numEvents) throws CouldntPerformActionEx {
						logger.info("About to call subscriber#receiveEvents...");
						int processingDelay = (subSpec.hasProcessingDelayMillis() ? subSpec.getProcessingDelayMillis() : -1);
						return ((CorbaNotifyConsumerOperations)pubSubComp).receiveEvents(new NcEventSpec[] {eventSpec}, processingDelay, numEvents);
					}
				};
				try {
					runner.runPubSub();
				} catch (Exception ex) {
					errors.add(ex);
				}
			}

			Thread.sleep(100); // to "ensure" that even the asynchronously called subscribers are ready before we publish events
			
			for (final PublisherSpecT pubSpec : pubSubSpec.getPublisher()) {
				boolean pubIsBlocking = isBlocking(pubSpec);
				if (!pubIsBlocking) {
					allPubSubHaveFiniteEvents = false;
				}
				ImplLangT implLang = deriveComponentImplLang(pubSpec, pubSubSpec);
				
				// Get the publisher component. This may retrieve the same component more than once if it should use more than one NC
				// (which will result in multiple NCPublisher instances created by that component).
				CorbaNotifySupplierOperations publisherComp = 
						componentAccessUtil.getDynamicSupplierComponent(pubSpec.getComponentName(), pubSpec.getContainerName(), implLang);
				
				PubSubRunner runner = new PubSubRunner(pubSpec, publisherComp, pubSubExec, m_logger) {
					@Override
					protected Integer callSpecific(NcEventSpec eventSpec, int numEvents) throws CouldntPerformActionEx {
						logger.info("About to call publisher#sendEvents...");
						int eventPeriodMillis = (pubSpec.hasEventPeriodMillis() ? pubSpec.getEventPeriodMillis() : -1);
						return ((CorbaNotifySupplierOperations)pubSubComp).sendEvents(new NcEventSpec[] {eventSpec}, eventPeriodMillis, numEvents);
					}
				};
				try {
					runner.runPubSub();
				} catch (Exception ex) {
					errors.add(ex);
				}
			}
		} // end of starting all pubs/subs
		
		
		// wait for NC scenario to execute
		if (errors.isEmpty()) {

			if (allPubSubHaveFiniteEvents) {
				// wait for all suppliers and subscribers to finish, enforcing the timeout if applicable
				// TODO: More options would be available if we stored the PubSubRunner instances in a list...
				pubSubExec.shutdown();
				if (executionTimeMax > 0) {
					pubSubExec.awaitTermination(executionTimeMax, executionTimeMaxUnit);
				}
				else {
					pubSubExec.awaitTermination(100*365, TimeUnit.DAYS); // like Dornroeschen
				}
			}
			else {
				// executionTime should be used to let the "indefinite" suppliers and subscribers do their work, 
				// even if some others should terminate by themselves
				if (executionTimeMax > 0) {
					try {
						m_logger.info("Will sleep for " + executionTimeMax + " " + executionTimeMaxUnit.toString().toLowerCase() + "...");
						executionTimeMaxUnit.sleep(executionTimeMax);
					} catch (Exception ex) {
						errors.add(ex);
					}
					// now also the finite suppliers/subscribers should be finished, but even if not they will get interrupted
					// along with the infinite ones in the cleanup.
				}
				else {
					errors.add(new IllegalArgumentException("An execution time must be specified if some publisher or subscriber is configured with an infinite number of events. Terminating right away..."));
				}
			}
		}
		
		// cleanup
		m_logger.info("Will clean up the pub/sub components...");
		
		// interrupt and release all components
		for (ACSComponentOperations comp : componentAccessUtil.getCachedComponents()) {
			// we could avoid the casting if we keep a separate list of subscriber and supplier comps...
			if (comp instanceof CorbaNotifyCompBaseOperations) {
				CorbaNotifyCompBaseOperations pubSubComp = (CorbaNotifyCompBaseOperations) comp;
				pubSubComp.ncDisconnect();
			}
		}
		componentAccessUtil.releaseAllComponents(true);
		
		// stop containers
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
			m_logger.severe("There were " + errors.size() + " errors!");
			throw errors.get(0);
		}
	}

	
	private ImplLangT deriveComponentImplLang(PubSubSpecCommonT spec, PubSubInfrastructureSpec infraSpec) {
		ImplLangT implLang = null; // we derive the component implementation language from the associated container
		for (ContainerSpecT containerSpec : infraSpec.getContainer()) {
			if (containerSpec.getContainerName().equals(spec.getContainerName())) {
				implLang = containerSpec.getImplLang();
			}
		}
		return implLang;
	}
	
	private static boolean isBlocking(PubSubSpecCommonT spec) {
		return ( spec.hasNumberOfEvents() && spec.getNumberOfEvents() > 0 );
	}

	
	/**
	 * Code shared between starting a single supplier or a subscriber for a given NC and component.
	 * The user should call {@link #runPubSub()}. 
	 * Also encapsulates the decision whether to run a non-blocking call in the same thread
	 * or to run in a different thread using the provided ExecutorService.
	 */
	private static abstract class PubSubRunner implements Callable<Integer> {
		
		protected final Logger logger;
		private final PubSubSpecCommonT spec;
		protected final CorbaNotifyCompBaseOperations pubSubComp;
		private final ExecutorService pubSubExec;
		private Future<Integer> callFuture; // could be useful in the future...

		PubSubRunner(PubSubSpecCommonT spec, CorbaNotifyCompBaseOperations pubSubComp, ExecutorService pubSubExec, Logger logger) {
			this.logger = logger;
			this.spec = spec;
			this.pubSubComp = pubSubComp;
			this.pubSubExec = pubSubExec;
		}
		
		void runPubSub() throws Exception {
			if (isBlocking(spec)) {
				// the publisher / subscriber will return only when the specified number of events have been published / received, 
				// for which we need to run it from a separate thread and later sync on the returned Future.
				callFuture = pubSubExec.submit(this);
			}
			else {
				// just call "sendEvents"/ "receiveEvents" from the current thread, as the call will return immediately
				call();
			}
		}
		
		@Override
		public Integer call() throws Exception {
		
			try {
				pubSubComp.ncConnect(new String[] {spec.getNC()} );
			} catch (CouldntPerformActionEx ex) {
				AcsJCouldntPerformActionEx ex2 = AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
				logger.log(Level.WARNING, "Failure invoking " + pubSubComp.name() + "#ncConnect: ", ex2);
				throw ex2;
			}
			logger.info("Connected '" + spec.getComponentName() + "' to NC " + spec.getNC());
			
			List<String> eventNamesForIdl = new ArrayList<String>();
			for (EventNameT eventName : spec.getEventName()) {
				eventNamesForIdl.add(eventName.toString());
			}
			final NcEventSpec eventSpec = new NcEventSpec(spec.getNC(), eventNamesForIdl.toArray(new String[0]), "");
			int numEvents = (spec.hasNumberOfEvents() ? spec.getNumberOfEvents() : -1);
			return callSpecific(eventSpec, numEvents);
		}
		
		/**
		 * Subclasses must contribute pub / sub specific code here, to make the actual component call. 
		 */
		abstract Integer callSpecific(NcEventSpec eventSpec, int numEvents) throws CouldntPerformActionEx;
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
