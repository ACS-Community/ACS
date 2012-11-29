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

import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.lessThanOrEqualTo;
import static org.junit.Assert.assertThat;

import java.util.concurrent.Callable;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import acs.benchmark.util.ContainerUtil;
import acs.benchmark.util.ContainerUtil.ContainerLogLevelSpec;

import alma.ACSErrTypeCommon.CouldntPerformActionEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCouldntPerformActionEx;
import alma.acs.component.client.ComponentClient;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.AcsLocations;
import alma.benchmark.CHANNELNAME_CONTROL_REALTIME;
import alma.benchmark.CorbaNotifyConsumerOperations;
import alma.benchmark.CorbaNotifySupplierOperations;
import alma.benchmark.NcEventSpec;
import alma.maci.containerconfig.types.ContainerImplLangType;

/**
 * A simple publisher-subscriber test that runs publisher and subscriber components
 * on the local host. 
 * It uses the daemon / component test framework.
 * 
 * @author hsommer
 */
public class LocalPubSubTest extends ComponentClient
{
	/**
	 * Should normally be 'null' for localhost, 
	 * but can be changed when using this test as a template for manual non-local tests.
	 */
	private static final String localhostName = null; //"alma-head";

	private ContainerUtil containerUtil;
	
	private PubSubComponentAccessUtil componentAccessUtil;
	
	private final String supplierContainerName = "localSupplierContainer1";
	
	/**
	 * TODO: Check if this rule and the getMethodName() call in setUp() can be moved up to ComponentClient,
	 *      if that adds a runtime dependency on junit, and how bad that would be.
	 *      Probably we should add a class ComponentClientTestCaseJUnit4 that extends ComponentClient
	 *      and only adds this testname business.
	 */
	@Rule 
	public TestName testName = new TestName();
	
	/**
	 * For compatibility with JUnit3 based TATJUnitRunner
	 */
	public static junit.framework.Test suite() {
		return new JUnit4TestAdapter(LocalPubSubTest.class);
	}

	public LocalPubSubTest() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), LocalPubSubTest.class.getSimpleName());
	}


	@Before
	public void setUp() throws Exception {
		String testMethodName = testName.getMethodName();
		m_logger.info("----------------- " + testMethodName + " ----------------- ");

		// run a local container. Logs are stored under $ACS_TMP/logs/
		containerUtil = new ContainerUtil(getContainerServices());
		containerUtil.loginToManager();
		containerUtil.startContainer(localhostName, ContainerImplLangType.JAVA, supplierContainerName, null, true);
		m_logger.info("Container '" + supplierContainerName + "' is ready.");
		
		// configure container log levels
		ContainerLogLevelSpec contLogLevelSpec = new ContainerLogLevelSpec(AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.DEBUG);
		contLogLevelSpec.addNamedLoggerSpec("jacorb@"+supplierContainerName, AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.WARNING);
		containerUtil.setContainerLogLevels(supplierContainerName, contLogLevelSpec);
		assertThat(containerUtil.isContainerLoggedIn(supplierContainerName), is(true));
		
		componentAccessUtil = new PubSubComponentAccessUtil(getContainerServices());
	}

	@After
	public void tearDown() throws Exception {
		if (componentAccessUtil != null) {
			componentAccessUtil.releaseAllComponents(true);
		}
		
		if (containerUtil != null) {
			containerUtil.stopContainer(localhostName, supplierContainerName);
			containerUtil.logoutFromManager();
		}
		super.tearDown();
	}

	/**
	 * Publishes 10.000 "MountStatusData" events on the NC "CONTROL_REALTIME", 
	 * using a single supplier component "JavaSupplier-1" for sequential publishing at maximum rate.
	 * The call to the supplier component is synchronous, returning when all events are published. 
	 * This allows us to measure the the publishing performance without sync'ing up through a subscriber.
	 */
	@Test
	public void testOneSupplierNoSubscriber() throws Exception {
		final int numEvents = 10000;
		CorbaNotifySupplierOperations supplierComp = null;
		try {
			// Create dynamic supplier component
			String componentName = "JavaSupplier-1";
			supplierComp = componentAccessUtil.getDynamicJavaSupplierComponent(componentName, supplierContainerName);
			
			// supplier setup
			String[] ncNames = new String[] {CHANNELNAME_CONTROL_REALTIME.value};
			supplierComp.ncConnect(ncNames);
			m_logger.info("Connected to NC " + ncNames[0]);
			
			NcEventSpec[] ncEventSpecs = new NcEventSpec[] {
					new NcEventSpec(ncNames[0], 
							new String[] {"MountStatusData"}, 
							"" // don't care about antenna name here
						) }; 
			
			// Let publisher component publish these events
			int callTimeInternalMillis = supplierComp.sendEvents(ncEventSpecs, -1, numEvents);
			
			m_logger.info("Single supplier comp '" + componentName + "' sent " + numEvents 
					+ " MountStatusData events at max speed to NC '" 
					+ ncNames[0] + "' in " + callTimeInternalMillis + " ms.");
			assertThat("Expecting at most 4 ms for publishing an event to a NotifyService.", 
					callTimeInternalMillis, lessThanOrEqualTo(numEvents * 4));
		} finally {
			// clean up
			if (supplierComp != null) supplierComp.ncDisconnect();
			if (componentAccessUtil != null) componentAccessUtil.releaseAllComponents(true);
		}
	}

	/**
	 * Publishes >= 10.000 "MountStatusData" or "LightweightMountStatusData" events on the NC "CONTROL_REALTIME", 
	 * using a single supplier component "JavaSupplier-1" for sequential publishing at a rate of 10 events/s,
	 * and a single subscriber component "" for counting the events.
	 * The call to the supplier component is asynchronous, returning while the publisher runs in "indefinite publishing" mode. 
	 * Once the subscriber has received enough data, we terminate the supplier. 
	 */
	@Test
	public void testOneSupplierOneSubscriberMixedEvents() throws Throwable {

		final int numEvents = 200;
		final int eventPeriodMillis = 48;
		
		String[] ncNames = new String[] {CHANNELNAME_CONTROL_REALTIME.value};
		
		// mixed events spec
		final NcEventSpec[] ncEventSpecs = new NcEventSpec[] {
				new NcEventSpec(ncNames[0], 
						new String[] {"MountStatusData", "LightweightMountStatusData"}, 
						"" // don't care about antenna name here
					) };
		
		// Create, configure and activate dynamic subscriber component.
		// It must be running before we publish events, to make sure we don't lose any.
		String subscriberContainerName = "localSubscriberContainer1";
		String subscriberComponentName = "JavaSubscriber-1";
		try {
			containerUtil.startContainer(localhostName, ContainerImplLangType.JAVA, subscriberContainerName, null, true);
			final CorbaNotifyConsumerOperations subscriberComp = componentAccessUtil.getDynamicJavaSubscriberComponent(subscriberComponentName, subscriberContainerName);
			subscriberComp.ncConnect(ncNames);
			m_logger.info("Connected subscriber to NC " + ncNames[0]);
			Callable<Integer> runSubscriber = new Callable<Integer>() {
				@Override
				public Integer call() throws Exception {
					m_logger.info("About to call subscriber#receiveEvents in a separate thread...");
					return subscriberComp.receiveEvents(ncEventSpecs, 0, numEvents);
				}
			};
			Future<Integer> subscriberCallFuture = Executors.newSingleThreadExecutor().submit(runSubscriber);
			Thread.sleep(100); // to "ensure" that the subscriber is ready before we publish events 
	
			// Create dynamic supplier component
			String componentName = "JavaSupplier-1";
			CorbaNotifySupplierOperations supplierComp = componentAccessUtil.getDynamicJavaSupplierComponent(
															componentName, supplierContainerName);
			
			// supplier setup
			supplierComp.ncConnect(ncNames);
			m_logger.info("Connected supplier to NC " + ncNames[0] + ". Will now send ~" + numEvents + " events, one every " + eventPeriodMillis + " ms.");
			
			// Let publisher component publish events as long as it takes for the subscriber to get enough of them 
			supplierComp.sendEvents(ncEventSpecs, eventPeriodMillis, -1);
			
			int subscriberReceptionTimeMillis = subscriberCallFuture.get(60, TimeUnit.SECONDS);
			m_logger.info("Subscriber component done. It received " + numEvents + " events in " + subscriberReceptionTimeMillis + " ms.");
			int expectedReceptionTimeMillis = numEvents * eventPeriodMillis;
			// TODO: In Eclipse with JDK 1.7 it works more compact like this: 
			// is(both(greaterThan((int)(0.90 * expectedReceptionTimeMillis))).and(lessThan((int)(1.1 * expectedReceptionTimeMillis))))
			assertThat("It should have taken around " + expectedReceptionTimeMillis + " ms to receive the events.",
					subscriberReceptionTimeMillis, 
					is(greaterThan((int)(0.80 * expectedReceptionTimeMillis))));
			assertThat("It should have taken around " + expectedReceptionTimeMillis + " ms to receive the events.",
					subscriberReceptionTimeMillis, 
					is(lessThan((int)(1.1 * expectedReceptionTimeMillis))));
		} catch (CouldntPerformActionEx ex) {
			throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx(ex);
		}
		catch (ExecutionException ex) {
			Throwable ex2 = ex.getCause();
			if (ex2 instanceof CouldntPerformActionEx) {
				throw AcsJCouldntPerformActionEx.fromCouldntPerformActionEx((CouldntPerformActionEx)ex2);
			}
			else {
				throw ex2;
			}
		}
		finally {
			componentAccessUtil.releaseComponent(subscriberComponentName, true);
			containerUtil.stopContainer(localhostName, subscriberContainerName);
			// for supplier component and container we trust the tearDown method..
		}
	}
}
