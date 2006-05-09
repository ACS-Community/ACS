package alma.acs.logging.config;

import junit.framework.TestCase;

import alma.acs.logging.ACSCoreLevel;

public class LogConfigTest extends TestCase {

	private LogConfig logConfig;
	
	public LogConfigTest() {
		super("LogConfigTest");
		logConfig = new LogConfig();
	}

	protected void setUp() throws Exception {
		super.setUp();
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	
	
	private static class CountingLogConfigSubscriber implements LogConfigSubscriber {
		volatile int count = 0;
		public void configureLogging(LogConfig logConfig) {
			assertNotNull(logConfig);
			count++;
		}
	}
	
	/**
	 * Tests adding and removing of subscribers, and whether {@link LogConfig#initialize()}
	 * triggers correctly the notification of these subscribers. 
	 */
	public void testSubscriberNotification() throws LogConfigException {
		int numSubscribers = 10;
		CountingLogConfigSubscriber[] configSubscribers = new CountingLogConfigSubscriber[numSubscribers];
		for (int i = 0; i < numSubscribers; i++) {
			configSubscribers[i] = new CountingLogConfigSubscriber();
			logConfig.addSubscriber(configSubscribers[i]);
			// as a stupid client we add some subscribers again but expect this to have no effect
			if (i%2 == 0) {
				logConfig.addSubscriber(configSubscribers[i]);
			}
		}
		logConfig.initialize(); // should call "configureLogging" on all subscribers
		for (int i = 0; i < numSubscribers; i++) {
			assertEquals(1, configSubscribers[i].count);
		}
		
		// now remove a few subscribers and assert they don't get called again, and that the remaining ones get called twice
		int numRemainingSubscribers = 3;
		assertTrue(numSubscribers > numRemainingSubscribers);
		for (int i = 0; i < numSubscribers - numRemainingSubscribers; i++) {
			logConfig.removeSubscriber(configSubscribers[i]);
		}
		logConfig.initialize(); 
		for (int i = 0; i < numSubscribers - numRemainingSubscribers; i++) {
			assertEquals(1, configSubscribers[i].count);
		}
		for (int i = numSubscribers - numRemainingSubscribers; i < numSubscribers; i++) {
			assertEquals(2, configSubscribers[i].count);
		}
	}

	
	/**
	 * Tests the config values returned from {@link LogConfig#getLogConfigData()}
	 * and {@link LogConfig#getLogConfigData(String)()} in the pristine state of our LogConfig object,
	 * that is, without CDB or other property information being considered.
	 * <p>
	 * Also asserts that none of these calls return the original object, 
	 * but instead a copy of it. This indirectly exercises the equals method.
	 */
	public void testDefaultValues() throws Exception {
		LogConfigData defaultLogConfig = logConfig.getLogConfigData();
		assertEquals("Log", defaultLogConfig.getCentralizedLoggerName());
		assertEquals(30, defaultLogConfig.getDispatchPacketSize());
		assertEquals(ACSCoreLevel.ACS_LEVEL_UNKNOWN, defaultLogConfig.getMinLogLevel());
		assertEquals(ACSCoreLevel.ACS_LEVEL_DEBUG, defaultLogConfig.getMinLogLevelLocal());
		assertEquals(ACSCoreLevel.ACS_LEVEL_ALERT, defaultLogConfig.getExpeditedDispatchLevel());

		LogConfigData defaultLogConfig2 = logConfig.getLogConfigData();
		assertNotSame(defaultLogConfig, defaultLogConfig2);
		assertEquals(defaultLogConfig, defaultLogConfig2);

		LogConfigData namedLogConfig1 = logConfig.getLogConfigData(null);
		assertNotSame(defaultLogConfig, namedLogConfig1);
		assertEquals(defaultLogConfig, namedLogConfig1);
		
		LogConfigData namedLogConfig2 = logConfig.getLogConfigData("nonExistingLogger");
		assertNotSame(defaultLogConfig, namedLogConfig2);
		assertEquals(defaultLogConfig, namedLogConfig2);
	}
	
	
	/**
	 * 
	 */
	public void testCDBValues() throws Exception {
		String cdbContainerPath = "MACI/Containers/frodoContainer";
        String frodoContainerXml = 
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?> " +  
                    "<Container xmlns=\"urn:schemas-cosylab-com:Container:1.0\" " + 
                               LogConfigData.CDBNAME_DISPATCH_PACKETSIZE + "=\"33\" " + 
                               LogConfigData.CDBNAME_MIN_LOG_LEVEL + "=\"2\" " +
                               LogConfigData.CDBNAME_MIN_LOG_LEVEL_LOCAL + "=\"3\" " +
                               LogConfigData.CDBNAME_EXPEDITED_DISPATCH_LEVEL + "=\"7\" " + 
                               LogConfigData.CDBNAME_CENTRALIZED_LOGGER + "=\"LogForFrodo\"> " + 
                    "</Container>";
		
		TestCDB testCDB = new TestCDB();
		testCDB.addCurlToXmlMapping(cdbContainerPath, frodoContainerXml);
		
		logConfig.setCDBContainerPath(cdbContainerPath);
		logConfig.setCDB(testCDB);
		
		logConfig.initialize();
		
		// TODO: test logConfig.setCDBComponentPath() with some real XML added to the test CDB, once this is defined and implemented.
	}
	
	
	/**
	 * Checks the response to bad CDB data and CDB access problems. 
	 */
	public void testInvalidCDB() throws Exception {
		String cdbContainerPath = "MACI/Containers/frodoContainer";
        String frodoContainerXml = 
            "<?xml version=\"1.0\" encoding=\"UTF-8\"?> " +  
                    "<Container xmlns=\"urn:schemas-cosylab-com:Container:1.0\" " + 
                               LogConfigData.CDBNAME_MIN_LOG_LEVEL + "=\"NotANumber\" " +
                               LogConfigData.CDBNAME_EXPEDITED_DISPATCH_LEVEL + "=\"\" " + // empty attr should be ignored w/o message
                               "myMisfittingAttribute=\"ShouldNotMatterThough\" " + // again no message expected for this
                               LogConfigData.CDBNAME_CENTRALIZED_LOGGER + "=\"LogForFrodo\"> " + 
                    "</Container>";
		
		TestCDB testCDB = new TestCDB();
		testCDB.addCurlToXmlMapping(cdbContainerPath, frodoContainerXml);
		
		logConfig.setCDBContainerPath(cdbContainerPath);
		logConfig.setCDB(testCDB);
		
		try {
			logConfig.initialize();
			fail("LogConfigException was expected.");
		}
		catch (LogConfigException ex) {
			assertEquals("Log config initialization at least partially failed. Problems during configuration: " + 
					"Attribute MinCachePriority: ignored invalid value NotANumber. ", ex.getMessage());
		}
		
		testCDB.setThrowException(true);
		try {
			logConfig.initialize();
		}
		catch (LogConfigException ex) {
			assertEquals("Log config initialization at least partially failed. " + 
					"Failed to read node MACI/Containers/frodoContainer from the CDB (msg='This is a test exception.'). ", ex.getMessage());
		}
		
	}
	
}
