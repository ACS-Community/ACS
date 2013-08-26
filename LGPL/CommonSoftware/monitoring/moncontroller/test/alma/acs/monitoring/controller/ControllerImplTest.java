package alma.acs.monitoring.controller;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.ThreadFactory;
import java.util.logging.Logger;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.ACS.ComponentStates;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;
import alma.MonitorArchiver.BlobberHelper;
import alma.MonitorArchiver.BlobberOperations;
import alma.MonitorArchiver.CollectorListStatus;
import alma.acs.container.CleaningDaemonThreadFactory;
import alma.acs.container.testsupport.DummyContainerServices;
import alma.acs.logging.ClientLogManager;

/**
 * Stand-alone test for the Controller component impl class {@link ControllerImpl}.
 * Dependencies on ContainerServices and other components are satisfied with mocks.
 */
public class ControllerImplTest
{
	private Logger logger;

	private TestControllerImpl controllerImpl;
	
	private MockContainerServices cs;
	
	@Rule 
	public TestName name = new TestName();

	
	@Before
	public void setUp() throws Exception {
		String testMethodName = name.getMethodName();
		String testFullName = ControllerImplTest.class.getSimpleName() + "#" + testMethodName;
		
		// this allows individual log level control
		System.setProperty("ACS.log.minlevel.namedloggers", 
				"ControllerImpl=2,99" +
				":MockBlobber=2,99" +
				":alma.acs.logging=5,99" +
				":alma.acs.logging.config.LogConfig=5,99");
		logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(testFullName, false);
		logger.info("----------- setUp for '" + name.getMethodName() + "' -----------");
		
		String compName = ControllerImpl.class.getSimpleName();
		cs = new MockContainerServices(compName);
		
		controllerImpl = new TestControllerImpl();
	}
	
	@After
	public void tearDown() throws Exception {
		logger.info("----------- tearDown for '" + name.getMethodName() + "' -----------");
		controllerImpl.cleanUp();
	}

	/**
	 * @throws Exception 
	 * 
	 */
	@Test
	public void testSimpleRegistration() throws Exception {
		// configured blobbers
		String blobberName1 = "blobber1";
		String blobberName2 = "blobber2";
		cs.setConfiguredBlobberCompNames(new String[] {blobberName1, blobberName2});
		
		// running blobbers 
		controllerImpl.addMockBlobber(new MockBlobber(blobberName1));
		
		controllerImpl.initialize(cs);
		
		String collectorName1 = "collector1";
		controllerImpl.registerCollector(collectorName1);
	}

	
	/**
	 * Component impl subclass that allows test / mock control, 
	 * in conjunction with {@link MockContainerServices}. 
	 */
	private static class TestControllerImpl extends ControllerImpl {
		private final Map<String, MockBlobber> mockBlobberInstances = new HashMap<String, MockBlobber>();
		
		@Override
		protected BlobberOperations getBlobberRefFromContainerServices(String blobberName) {
			return mockBlobberInstances.get(blobberName);
		}

		void addMockBlobber(MockBlobber mockBlobber) {
			mockBlobberInstances.put(mockBlobber.name(), mockBlobber);
		}
	}
	
	private static class MockBlobber implements BlobberOperations {
		private final String name;
		private final Logger logger;

		MockBlobber(String name) {
			this.name = name;
			this.logger = ClientLogManager.getAcsLogManager().getLoggerForApplication(MockBlobber.class.getSimpleName(), false);
		}
		
		@Override
		public CollectorListStatus addCollector(String inComponentName) {
			logger.info("addCollector called with collectorName=" + inComponentName);
			return CollectorListStatus.ADDED;
		}

		@Override
		public CollectorListStatus containsCollector(String inComponentName) {
			return null;
		}

		@Override
		public CollectorListStatus removeCollector(String inComponentName) {
			return null;
		}

		@Override
		public ComponentStates componentState() {
			return ComponentStates.COMPSTATE_OPERATIONAL;
		}

		@Override
		public String name() {
			return name;
		}
	}
	
	
	/**
	 *
	 */
	private static class MockContainerServices extends DummyContainerServices {

		private String[] blobberCompNames;
		private final ThreadFactory tf;
		
		public MockContainerServices(String name) {
			super(name, ClientLogManager.getAcsLogManager().getLoggerForApplication(name, false));
			tf = new CleaningDaemonThreadFactory(name, logger);
		}
	
		void setConfiguredBlobberCompNames(String[] blobberCompNames) {
			this.blobberCompNames = blobberCompNames;
		}

		@Override
		public String[] findComponents(String curlWildcard, String typeWildcard) throws AcsJContainerServicesEx {
			if (curlWildcard == null && typeWildcard.equals(BlobberHelper.id())) {
				return blobberCompNames;
			}
			else {
				return super.findComponents(curlWildcard, typeWildcard);
			}
		}

		@Override
		public void releaseComponent(String componentUrl, ComponentReleaseCallback callback) {
			if (callback == null) {
				logger.info("CS: Async release of component '" + componentUrl + "'.");
			}
			else {
				super.releaseComponent(componentUrl, callback);
			}
		}
		
		@Override
		public ThreadFactory getThreadFactory() {
			logger.finer("CS: getThreadFactory() called");
			return tf;
		}
	}
}
