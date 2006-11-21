package alma.acs.logging.config;

import junit.framework.TestCase;

import alma.acs.logging.ACSCoreLevel;
import alma.acs.testsupport.TestLogger;
import alma.maci.loggingconfig.LoggingConfig;
import alma.maci.loggingconfig.NamedLogger;

public class LogConfigTest extends TestCase {

	private LogConfig logConfig;
	
	/**
	 * 
	 */
	public LogConfigTest() {
		super("LogConfigTest");
		logConfig = new LogConfig();
		logConfig.setInternalLogger(TestLogger.getLogger("LogConfigTest"));
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
	 * Tests the config values returned from {@link LogConfig#getLoggingConfig()}
	 * and {@link LogConfig#getSpecialLoggerConfig(String)} in the pristine state of our LogConfig object,
	 * that is, without CDB or other property information being considered.
	 * <p>
	 * Also asserts that none of these calls return the original object, 
	 * but instead a copy of it. This indirectly exercises the equals method.
	 */
	public void testDefaultValues() throws Exception {
		LoggingConfig defaultLogConfig = logConfig.getLoggingConfig();
		assertEquals("Log", defaultLogConfig.getCentralizedLogger());
		assertEquals(10, defaultLogConfig.getDispatchPacketSize());
		assertEquals(ACSCoreLevel.ACS_LEVEL_TRACE, defaultLogConfig.getMinLogLevel());

		int expectedMinLogLevelLocal = ACSCoreLevel.ACS_LEVEL_TRACE;
		// TAT defines ACS_LOG_STDOUT, so the test must take this into account
		Integer ACS_LOG_STDOUT = Integer.getInteger(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL_LOCAL);
    	if (ACS_LOG_STDOUT != null) {
    		expectedMinLogLevelLocal = ACS_LOG_STDOUT.intValue();
    	}
		assertEquals(expectedMinLogLevelLocal, defaultLogConfig.getMinLogLevelLocal());
		
		assertEquals(ACSCoreLevel.ACS_LEVEL_ALERT, defaultLogConfig.getImmediateDispatchLevel());

		LoggingConfig defaultLogConfig2 = logConfig.getLoggingConfig();
//		assertNotSame(defaultLogConfig, defaultLogConfig2); // commented out because currently we don't copy/hide the instance
		assertEquals(defaultLogConfig, defaultLogConfig2);

		NamedLogger namedLogConfig1 = logConfig.getSpecialLoggerConfig(null);
		assertEquals(defaultLogConfig.getMinLogLevel(), namedLogConfig1.getMinLogLevel());
		assertEquals(defaultLogConfig.getMinLogLevelLocal(), namedLogConfig1.getMinLogLevelLocal());
		
		NamedLogger namedLogConfig2 = logConfig.getSpecialLoggerConfig("nonExistingLogger");
		assertNotSame(namedLogConfig1, namedLogConfig2); 
		assertEquals(namedLogConfig1.getMinLogLevel(), namedLogConfig2.getMinLogLevel());
		assertEquals(namedLogConfig1.getMinLogLevelLocal(), namedLogConfig2.getMinLogLevelLocal());
	}
	
	
	/**
	 * 
	 */
	public void testCDBValues() throws Exception {
		String cdbContainerPath = "MACI/Containers/frodoContainer";
        String frodoContainerXml = 
            "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> " +  
                    "<Container xmlns=\"urn:schemas-cosylab-com:Container:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:log=\"urn:schemas-cosylab-com:LoggingConfig:1.0\">" + 
                          "<LoggingConfig " +
                          " minLogLevel=\"2\" " + 
                          " minLogLevelLocal=\"3\" " +
                          " centralizedLogger=\"LogForFrodo\" " +
                          " maxLogQueueSize=\"200\" " +
                          " immediateDispatchLevel=\"7\" " +
                          " dispatchPacketSize=\"33\" " +
                          " >" +
                               //<log:_ Name="MyMuteComponent" minLogLevel="5" minLogLevelLocal="5" />
                          "</LoggingConfig>" +
                    "</Container>";
        		
		TestCDB testCDB = new TestCDB();
		testCDB.addCurlToXmlMapping(cdbContainerPath, frodoContainerXml);
		
		logConfig.setCDBContainerPath(cdbContainerPath);
		logConfig.setCDB(testCDB);
		
		logConfig.initialize();

		LoggingConfig updatedConfig = logConfig.getLoggingConfig();
		assertEquals("LogForFrodo", updatedConfig.getCentralizedLogger());
		assertEquals(7, updatedConfig.getImmediateDispatchLevel());
		assertEquals(33, updatedConfig.getDispatchPacketSize());		
		assertEquals(10, updatedConfig.getFlushPeriodSeconds()); // was not in CDB, thus default should be used		
		
		// TODO: test logConfig.setCDBComponentPath() with some real XML added to the test CDB, once this is implemented
	}
	
	
	/**
	 * Checks the response to bad CDB data and CDB access problems. 
	 */
	public void testInvalidCDB() throws Exception {
		String cdbContainerPath = "MACI/Containers/frodoContainer";
        String frodoContainerXml = 
            "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> " +  
            "<Container xmlns=\"urn:schemas-cosylab-com:Container:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:log=\"urn:schemas-cosylab-com:LoggingConfig:1.0\">" + 
                  "<LoggingConfig " +
                  " minLogLevel=\"NotANumber\" " + // should cause an error 
                  " minLogLevelLocal=\"3\" " +
                  " centralizedLogger=\"LogForFrodo\" " +
                  " maxLogQueueSize=\"200\" " +
                  " immediateDispatchLevel=\"\" " + // empty attr should be ignored w/o message
                  " myMisfittingAttribute=\"ShouldNotMatterThough\" " + // again no err message expected for this
                  " dispatchPacketSize=\"33\" " +
                  " flushPeriodSeconds=\"5\" " +
                  " />" +
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
			assertEquals("Log config initialization at least partially failed. Failed to parse XML for CDB node MACI/Containers/frodoContainer into binding classes " + 
					"(ex=org.exolab.castor.xml.MarshalException, msg='The following error occured while trying to unmarshal field _minLogLevel of class alma.maci.loggingconfig.LoggingConfig\n" +
					"For input string: \"NotANumber\"'). ", ex.getMessage());
		}		
		
		testCDB.setThrowException(true);
		try {
			logConfig.initialize();
			fail("LogConfigException was expected.");
		}
		catch (LogConfigException ex) {
			assertEquals("Log config initialization at least partially failed. Node MACI/Containers/frodoContainer does not exist in the CDB (msg='CDB record does not exist'). ", ex.getMessage());
		}
		
	}
	
}

