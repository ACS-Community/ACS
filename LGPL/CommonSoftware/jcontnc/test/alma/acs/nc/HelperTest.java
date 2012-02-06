package alma.acs.nc;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.concurrent.TimeUnit;

import junit.framework.AssertionFailedError;

import org.omg.CORBA.IntHolder;
import org.omg.CosNaming.NameComponent;
import org.omg.CosNaming.NamingContext;
import org.omg.CosNaming.NamingContextPackage.NotFound;
import org.omg.CosNotification.Property;
import org.omg.CosNotification.UnsupportedAdmin;
import org.omg.CosNotification.UnsupportedQoS;

import Monitor.Data;
import Monitor.DataType;
import gov.sandia.CosNotification.NotificationServiceMonitorControl;
import gov.sandia.CosNotification.NotificationServiceMonitorControlHelper;
import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;
import gov.sandia.NotifyMonitoringExt.ActiveEventChannelCount;
import gov.sandia.NotifyMonitoringExt.ActiveEventChannelNames;
import gov.sandia.NotifyMonitoringExt.EventChannel;
import gov.sandia.NotifyMonitoringExt.EventChannelCreationTime;
import gov.sandia.NotifyMonitoringExt.EventChannelFactory;
import gov.sandia.NotifyMonitoringExt.EventChannelFactoryNames;
import gov.sandia.NotifyMonitoringExt.NameAlreadyUsed;
import gov.sandia.NotifyMonitoringExt.NameMapError;

import alma.ACSErrTypeCORBA.wrappers.AcsJNarrowFailedEx;
import alma.ACSErrTypeCommon.wrappers.AcsJCORBAProblemEx;
import alma.acs.component.client.ComponentClientTestCase;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.util.IsoDateFormat;
import alma.acscommon.NC_KIND;


/**
 * Tests creation of notification channels and usage of the TAO notification extensions 
 * that were introduced to fix the race conditions described in COMP-2808.
 * 
 * @author hsommer
 */
public class HelperTest extends ComponentClientTestCase
{
	private HelperWithChannelCreationSynch helper;
	private NotificationServiceMonitorControl nsmc;

	public HelperTest() throws Exception {
		super("HelperTest");
	}

	protected void setUp() throws Exception {
		System.out.println("------------------------- " + getName() + " -------------------------");
		super.setUp();
		helper = new HelperWithChannelCreationSynch(getContainerServices(), Helper.getNamingServiceInitial(getContainerServices()));

		String factoryMonitorRegName = "MC_NotifyEventChannelFactory";
		try {
			nsmc = NotificationServiceMonitorControlHelper.narrow(
					helper.getNamingService().resolve(new NameComponent[]{new NameComponent("MC_NotifyEventChannelFactory", "")}));
		} catch (NotFound ex) {
			fail("Failed to resolve factory's MC extension object in the naming service: " + factoryMonitorRegName);
		}
		assertNotNull(nsmc);
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	
//	public void testNamingServiceBindings() {
//		BindingListHolder blh = new BindingListHolder();
//		helper.getNamingService().list(100, blh, new BindingIteratorHolder());
//		for (Binding binding : blh.value) {
//			System.out.println(binding.binding_name[0].id + "  " + binding.binding_type.value());
//		}
//	}
	
	/**
	 * 
	 */
	public void testTaoMonitorAndControlService() throws Exception {
		
		String[] nsmcNames = nsmc.get_statistic_names();
		String nsmcNamesMsg = "NotificationServiceMonitorControl service offers statistcs for ";
		for (String nsmcName : nsmcNames) {
			nsmcNamesMsg += nsmcName + ", ";
		}
		m_logger.info("The following strings can be used as args to getStatistics: " + nsmcNamesMsg);
		m_logger.info("EventChannelFactoryNames: " + getStatistic(EventChannelFactoryNames.value));
		m_logger.info("ActiveEventChannelCount: " + getStatistic("NotifyEventChannelFactory/" + ActiveEventChannelCount.value));
		m_logger.info("ActiveEventChannelNames: " + getStatistic("NotifyEventChannelFactory/" + ActiveEventChannelNames.value));
		
		Date t1 = getStatisticTime("NotifyEventChannelFactory/" + EventChannelCreationTime.value);
		m_logger.info("NotifyEventChannelFactory/EventChannelCreationTime: " + IsoDateFormat.formatDate(t1));		
	}
	
	/**
	 * Creates and destroys a test channel.
	 * Also tests creating a second instance of that channel after the first one has been created, 
	 * to check for the NameAlreadyUsed ex.
	 */
	public void testCreateChannel() throws Exception {
		String channelName = "singleChannel";
		String factoryName = helper.getNotificationFactoryNameForChannel(channelName);
		
		EventChannel myChannel = null;
		try {
			//precondition: channel not there (e.g. from previous run)
			assertChannel(false, channelName);
			
			// The call to "getNotificationChannel" should create the channel, because reuse will not be possible.
			myChannel = helper.getNotificationChannel(channelName, NC_KIND.value, factoryName);
			assertChannel(true, channelName);
			
			// Now we try to create that channel again, without allowing reuse. Should fail. 
			try {
				helper.createNotificationChannel(channelName, NC_KIND.value, factoryName);
				fail("Expected NameAlreadyUsed exception for creating the channel twice.");
			} catch (Exception ex) {
				m_logger.info("Got a NameAlreadyUsed exception as expected.");
			}
			
			// But with reuse it should work
			EventChannel myChannel2 = helper.getNotificationChannel(channelName, NC_KIND.value, factoryName);
			assertTrue(myChannel._is_equivalent(myChannel2));
			
		} finally {
			// Destroy the channel
			if (myChannel != null) {
				helper.destroyNotificationChannel(channelName, NC_KIND.value, myChannel);
			}
			assertChannel(false, channelName);
		}
	}

	
	/**
	 * Tests the collision case where many threads create the same channel concurrently.
	 * <p>
	 * Note that we would need to hack TAO to slow down (or otherwise synchronize with) channel creation,
	 * so that we can be sure that the second request comes in before the first request has finished.
	 * We optimize this by synchronizing the test threads right before they make the call to the channel factory, 
	 * for which we overload the method {@link HelperWithChannelCreationSynch#createNotifyChannel_internal(EventChannelFactory, Property[], Property[], String, IntHolder)}.
	 * This eliminates jitter from thread creation, thread starting, and contact with the naming service, all happening before actual channel creation.
	 */
	public void testConcurrentChannelCreation() throws Exception {
		String channelName = "testChannelForConcurrentCreation"; // one channel tried to be created concurrently
		
		assertChannel(false, channelName);

		// @TODO Refactor the following code to use alma.acs.concurrent.ThreadBurstExecutorService now that we have it
		class ChannelCreator implements Callable<EventChannel> {
			private final String channelName;
			private final CountDownLatch synchStart;
			ChannelCreator(String channelName, CountDownLatch synchStart) {
				this.channelName = channelName;
				this.synchStart = synchStart;
			}
			public EventChannel call() throws Exception {
				String factoryName = helper.getNotificationFactoryNameForChannel(channelName);
				return helper.createNotificationChannel(channelName, NC_KIND.value, factoryName, synchStart);
			}
		}
		// we need at least two threads, but more threads may improve collision chances
		final int numCreators = 4;
		assertTrue(numCreators >= 2);
		
		ExecutorService pool = Executors.newFixedThreadPool(numCreators, getContainerServices().getThreadFactory());
		CountDownLatch synchCreationStart = new CountDownLatch(numCreators);
		
		List<Future<EventChannel>> results = new ArrayList<Future<EventChannel>>();
		
		
		// check the results
		EventChannel uniqueChannel = null;
		try {
			// Run the threads that create the same channel
			for (int i = 0; i < numCreators; i++) {
				results.add(pool.submit(new ChannelCreator(channelName, synchCreationStart)));
			}
			// wait for all threads to finish. Waiting here instead of waiting on the future.get() calls
			// has the advantage that we can exit this method with a fail() without leaving an ongoing channel creation behind.
			pool.shutdown();
			assertTrue(pool.awaitTermination(30, TimeUnit.SECONDS));
			
			for (Future<EventChannel> future : results) {
				try {
					EventChannel threadResult = future.get();
					// we only get here if threadResult != null, otherwise ex
					if (uniqueChannel != null) {
						fail("Only one thread should have managed to create the channel without exception!");
					}
					else {
						uniqueChannel = threadResult;
					}
				} 
				catch (ExecutionException ex) {
					if (ex.getCause() instanceof NameAlreadyUsed) {
						m_logger.info("Got a NameAlreadyUsed exception");
					}
					else {
						fail("Unexpected exception "+ ex.getCause().toString());
					}
				}
				catch (AssertionFailedError ex) {
					throw ex;
				}
				catch (Throwable thr) {
					fail("Unexpected exception "+ thr.toString());
				}
			}
			assertNotNull("One thread should have succeeded", uniqueChannel);
		} 
		finally {
			if (uniqueChannel != null) {
				helper.destroyNotificationChannel(channelName, NC_KIND.value, uniqueChannel);
			}
		}
	}
	
	/**
	 * One step up from {@link #testConcurrentChannelCreation()}, here we test concurrent calls to 
	 * {@link Helper#getNotificationChannel(String, String, String)} which are supposed to handle the 
	 * <code>NameAlreadyUsed</code> exception by making those later threads wait until the channel has 
	 * been created for the first thread, then sharing the channel object.
	 */
	public void testConcurrentChannelRetrieval() throws Throwable {
		final String channelName = "testChannelForConcurrentRetrieval"; // one channel to be retrieved concurrently
		assertChannel(false, channelName);

		class ChannelRetriever implements Callable<EventChannel> {
			private final String channelName;
			private final CountDownLatch synchStart;
			ChannelRetriever(String channelName, CountDownLatch synchStart) {
				this.channelName = channelName;
				this.synchStart = synchStart;
			}
			public EventChannel call() throws Exception {
				String factoryName = helper.getNotificationFactoryNameForChannel(channelName);
				return helper.getNotificationChannel(channelName, NC_KIND.value, factoryName, synchStart);
			}
		}
		// we need at least two threads, but more threads may improve collision chances
		final int numCreators = 3;
		assertTrue(numCreators >= 2);
		
		ExecutorService pool = Executors.newFixedThreadPool(numCreators, getContainerServices().getThreadFactory());
		CountDownLatch synchCreationStart = new CountDownLatch(numCreators);
		
		List<Future<EventChannel>> results = new ArrayList<Future<EventChannel>>();

		// check the results
		EventChannel uniqueChannel = null;
		try {
			// Run the threads that request the same channel
			for (int i = 0; i < numCreators; i++) {
				results.add(pool.submit(new ChannelRetriever(channelName, synchCreationStart)));
			}
			// wait for all threads to finish. Waiting here instead of waiting on the future.get() calls
			// has the advantage that we can exit this method with a fail() without leaving an ongoing channel creation behind.
			pool.shutdown();
			assertTrue(pool.awaitTermination(30, TimeUnit.SECONDS));
			
			for (Future<EventChannel> future : results) {
				try {
					EventChannel threadResult = future.get();
					// we only get here if threadResult != null, otherwise ex
					if (uniqueChannel != null) {
						assertTrue(uniqueChannel._is_equivalent(threadResult));
					}
					uniqueChannel = threadResult;
				} 
				catch (ExecutionException ex) {
					throw ex.getCause();
				}
				catch (AssertionFailedError ex) {
					throw ex;
				}
				catch (Throwable thr) {
					fail("Unexpected exception "+ thr.toString());
				}
			}
			m_logger.info("All concurrent calls to getNotificationChannel got the same channel object.");
		} 
		finally {
			if (uniqueChannel != null) {
				helper.destroyNotificationChannel(channelName, NC_KIND.value, uniqueChannel);
			}
		}
		
	}

	
	/////////////////////////////
	//// Test helper methods
	/////////////////////////////

	/**
	 * Helper method that checks (non-/)existence of a channel
	 * @param existing
	 */
	private void assertChannel(boolean existing, String channelName) {
		// Check notify service factory
		
		// Check naming service binding
		NameComponent[] t_NameSequence = { new NameComponent(channelName, NC_KIND.value) };
		try {
			helper.getNamingService().resolve(t_NameSequence);
			if (existing) {
				m_logger.info("Verified that channel " + channelName + " is registered with the naming service.");
			}
			else {
				fail("Channel " + channelName + " is erroneously registered with the naming service.");
			}
		} catch (NotFound ex) {
			if (existing) {
				fail("Channel " + channelName + " is NOT registered with the naming service.");
			}
			else {
				m_logger.info("Verified that channel " + channelName + " is NOT registered with the naming service.");
			}
		} catch (Throwable thr) {
			fail("Unexpected exception " + thr.toString());
		}
	}

	private String getStatistic(String statName) throws InvalidName {
		Data data = nsmc.get_statistic(statName);
		if (data.data_union.discriminator() == DataType.DATA_TEXT) {
			String[] list = data.data_union.list();
			return Arrays.toString(list);
		}
		else if (data.data_union.discriminator() == DataType.DATA_NUMERIC) {
			return Double.toString(data.data_union.num().last);
		}
		return null;
	}
	
	private Date getStatisticTime(String statName) throws InvalidName {
		Data data = nsmc.get_statistic(statName);
		if (data.data_union.discriminator() == DataType.DATA_NUMERIC) {
			long t = (long) data.data_union.num().last;
			return new Date(t*1000);
		}
		throw new org.omg.CORBA.BAD_OPERATION();
	}

	/**
	 * We extend the tested Helper class so that this test can synchronize on the actual channel creation
	 */
	private static class HelperWithChannelCreationSynch extends Helper {

		private volatile CountDownLatch synch;

		public HelperWithChannelCreationSynch(ContainerServicesBase services, NamingContext namingService) throws AcsJException {
			super(services, namingService);
		}
		
		/**
		 * Counts down the optional CountDownLatch, then delegates to parent method
		 * @throws AcsJCORBAProblemEx 
		 */
		protected EventChannel createNotifyChannel_internal(EventChannelFactory notifyFactory, Property[] initial_qos,
				Property[] initial_admin, String channelName, IntHolder channelIdHolder) 
				throws UnsupportedAdmin, NameAlreadyUsed, UnsupportedQoS, NameMapError, AcsJNarrowFailedEx, AcsJCORBAProblemEx {
			if (synch != null) {
				// count down and wait for other threads to reach the same state before going on.
				synch.countDown();
				m_logger.info("Thread "+ Thread.currentThread().getName() + " is ready to proceed with channel creation. Count is " + synch.getCount());
				try {
					synch.await(10, TimeUnit.SECONDS);
				} catch (InterruptedException ex) {
					ex.printStackTrace();
				}
			}
			return super.createNotifyChannel_internal(initial_qos, initial_admin, channelName, channelIdHolder);
		}
		
		protected EventChannel createNotificationChannel(String channelName, String channelKind, String notifyFactoryName, CountDownLatch synch) 
				throws AcsJException, NameAlreadyUsed, NameMapError {
			this.synch = synch;
			return super.createNotificationChannel(channelName, channelKind, notifyFactoryName);
		}
		
		protected EventChannel getNotificationChannel(String channelName, String channelKind, String notifyFactoryName, CountDownLatch synch)
				throws AcsJException {
			this.synch = synch;
			return super.getNotificationChannel(channelName, channelKind, notifyFactoryName);
		}

	}
	
}
