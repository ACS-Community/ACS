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
package alma.acs.logging.config;

import java.io.File;
import java.io.FileReader;
import java.io.StringReader;
import java.util.Set;
import java.util.logging.Logger;

import junit.framework.TestCase;

import si.ijs.maci.LoggingConfigurable;

import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.testsupport.TestLogger;
import alma.acs.util.ReaderExtractor;
import alma.maci.loggingconfig.LoggingConfig;
import alma.maci.loggingconfig.UnnamedLogger;
import alma.maci.loggingconfig.types.LogLevel;

/**
 * Test for {@linkplain LogConfig}. 
 * Does not require a running ACS environment.
 * 
 * @author hsommer
 */
public class LogConfigTest extends TestCase {

	private LogConfig logConfig;
	private Logger logger;
	
	public LogConfigTest() {
		super("LogConfigTest");
	}

	protected void setUp() throws Exception {
		super.setUp();
		logConfig = new LogConfig();
		logger = TestLogger.getLogger("LogConfigTest");
		logConfig.setInternalLogger(logger);
	}

	protected void tearDown() throws Exception {
		super.tearDown();
	}

	
	
	/**
	 * Mock LogConfigSubscriber to trace notification.
	 */
	private static class CountingLogConfigSubscriber implements LogConfigSubscriber {
		volatile int count = 0;
		public void configureLogging(LogConfig logConfig) {
			assertNotNull(logConfig);
			count++;
		}
	}
	
	
	/**
	 * Tests adding and removing of subscribers, and whether {@link LogConfig#initialize(boolean)}
	 * triggers correctly the notification of these subscribers. 
	 */
	public void testSubscriberNotification() throws LogConfigException {
		logger.info("============ Running testSubscriberNotification ============");

		int numSubscribers = 10;
		CountingLogConfigSubscriber[] configSubscribers = new CountingLogConfigSubscriber[numSubscribers];
		for (int i = 0; i < numSubscribers; i++) {
			configSubscribers[i] = new CountingLogConfigSubscriber();
			logConfig.addSubscriber(configSubscribers[i]);
			// as a stupid client we add some subscribers again but expect this to have no side effect
			if (i%2 == 0) {
				logConfig.addSubscriber(configSubscribers[i]);
			}
		}
		logConfig.initialize(false); // should call "configureLogging" on all subscribers
		for (int i = 0; i < numSubscribers; i++) {
			assertEquals(1, configSubscribers[i].count);
		}
		
		// now remove a few subscribers and assert they don't get called again, and that the remaining ones get called again
		int numRemainingSubscribers = 3;
		assertTrue(numSubscribers > numRemainingSubscribers);
		for (int i = 0; i < numSubscribers - numRemainingSubscribers; i++) {
			logConfig.removeSubscriber(configSubscribers[i]);
		}
		logConfig.initialize(false); 
		for (int i = 0; i < numSubscribers - numRemainingSubscribers; i++) {
			assertEquals(1, configSubscribers[i].count);
		}
		for (int i = numSubscribers - numRemainingSubscribers; i < numSubscribers; i++) {
			assertEquals(2, configSubscribers[i].count);
		}
	}

	
	/**
	 * Tests the config values returned from {@link LogConfig} 
	 * with at most env vars for default levels being set (can be enforced by TAT!),
	 * but without CDB or other information being considered.
	 * <p>
	 * Also asserts that none of these calls return the original object, 
	 * but instead a copy of it. This indirectly exercises the equals method.
	 */
	public void testDefaultValues() throws Exception {
		logger.info("============ Running testDefaultValues ============");

		assertEquals("Log", logConfig.getCentralizedLogger());
		assertEquals(100, logConfig.getDispatchPacketSize());

		LoggingConfig schemaDefaults = new LoggingConfig(); 
		// We verify that the castor-generated class actually has the current schema defaults.
		// These values must be adjusted when the schema is changed.
		// In that case also the values in the simulated CDB's xml might have to be changed
		// in order to still be different from the default values.
		assertEquals(0, schemaDefaults.getCount()); // 0 named loggers
		assertEquals(100, schemaDefaults.getDispatchPacketSize());
		assertEquals(10, schemaDefaults.getFlushPeriodSeconds());
		assertEquals(LogLevel.VALUE_10, schemaDefaults.getImmediateDispatchLevel());
		assertEquals(1000, schemaDefaults.getMaxLogQueueSize());
		assertEquals(LogLevel.VALUE_2, schemaDefaults.getMinLogLevelLocal());
		assertEquals(LogLevel.VALUE_2, schemaDefaults.getMinLogLevel());
		assertEquals("Log", schemaDefaults.getCentralizedLogger());

		AcsLogLevelDefinition defaultMinLogLevelLocal = AcsLogLevelDefinition.fromXsdLogLevel(schemaDefaults.getMinLogLevelLocal());
		// but if env vars are set, we may have different default levels
		Integer PROP_ACS_LOG_STDOUT = Integer.getInteger(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL_LOCAL);
    	if (PROP_ACS_LOG_STDOUT != null) {
    		defaultMinLogLevelLocal = AcsLogLevelDefinition.fromInteger(PROP_ACS_LOG_STDOUT.intValue());
    		logger.info("Using default stdout level from env var: " + defaultMinLogLevelLocal);
    	}
    	else {
    		logger.info("No env var setting found for " + LogConfig.PROPERTYNAME_MIN_LOG_LEVEL_LOCAL);
    	}
    	AcsLogLevelDefinition defaultMinLogLevel = AcsLogLevelDefinition.fromXsdLogLevel(schemaDefaults.getMinLogLevel());
		Integer PROP_ACS_LOG_REMOTE = Integer.getInteger(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL);
    	if (PROP_ACS_LOG_REMOTE != null) {
    		defaultMinLogLevel = AcsLogLevelDefinition.fromInteger(PROP_ACS_LOG_REMOTE.intValue());
    		logger.info("Using default remote level from env var: " + defaultMinLogLevelLocal);
    	}
    	else {
    		logger.info("No env var setting found for " + LogConfig.PROPERTYNAME_MIN_LOG_LEVEL);
    	}
    	
    	// our logConfig should give the correct default values, coming from schema or env var
		assertEquals(defaultMinLogLevelLocal, logConfig.getDefaultMinLogLevelLocal());
		assertEquals(defaultMinLogLevel, logConfig.getDefaultMinLogLevel());
		
		// Check default data other than log levels
		assertEquals(schemaDefaults.getCentralizedLogger(), logConfig.getCentralizedLogger());
		assertEquals(schemaDefaults.getImmediateDispatchLevel(), logConfig.getImmediateDispatchLevel().toXsdLevel());
		assertEquals(schemaDefaults.getDispatchPacketSize(), logConfig.getDispatchPacketSize());
		assertEquals(schemaDefaults.getFlushPeriodSeconds(), logConfig.getFlushPeriodSeconds());

		// Get log levels for not existing named loggers, which should result in the default log levels being used
		UnnamedLogger namedLogConfig1 = logConfig.getNamedLoggerConfig(null); 
		assertEquals(defaultMinLogLevel, AcsLogLevelDefinition.fromXsdLogLevel(namedLogConfig1.getMinLogLevel()));
		assertEquals(defaultMinLogLevelLocal, AcsLogLevelDefinition.fromXsdLogLevel(namedLogConfig1.getMinLogLevelLocal()));
		
		UnnamedLogger namedLogConfig2 = logConfig.getNamedLoggerConfig("nonExistingLogger");
		assertNotSame(namedLogConfig1, namedLogConfig2); 
		assertEquals(defaultMinLogLevel, AcsLogLevelDefinition.fromXsdLogLevel(namedLogConfig2.getMinLogLevel()));
		assertEquals(defaultMinLogLevelLocal, AcsLogLevelDefinition.fromXsdLogLevel(namedLogConfig2.getMinLogLevelLocal()));
	}
	
		
	/**
	 * Test for the XPath based {@link LogConfig#getLogConfigXml(String, String)} which extracts log info from the XML 
	 * that we get from the CDB.
	 */
	public void testGetLogConfigXml() throws Exception {
		logger.info("============ Running testGetLogConfigXml ============");

		File containerConfigFile = new File("frodoContainer.xml");
		assertTrue("Cannot find file frodoContainer.xml. Check that file exists and test is run with working dir acsjlog/test.", containerConfigFile.exists());
		String containerConfigXml = (new ReaderExtractor((new FileReader(containerConfigFile)))).extract();		
		logger.info("containerConfigXml = " + containerConfigXml);
		assertNotNull(containerConfigXml);
		assertFalse(containerConfigXml.isEmpty());
		
		TestCDB testCDB = new TestCDB();
		logConfig.setCDB(testCDB);
		String cdbContainerConfigPath = "frodoContainer";
		testCDB.addCurlToXmlMapping(cdbContainerConfigPath, containerConfigXml);
		logConfig.setCDBLoggingConfigPath(cdbContainerConfigPath);

		String xml = logConfig.getLogConfigXml(cdbContainerConfigPath, "//" + LogConfig.CDBNAME_LoggingConfig);
		assertNotNull(xml);
		logger.info("Got container logging config xml: " + xml);
		LoggingConfig loggingConfig = LoggingConfig.unmarshalLoggingConfig(new StringReader(xml));
		assertNotNull(loggingConfig);		
		
		String separateConfigComponent1 = "testComp1";
		String separateConfigComponent2 = "testComp2";
		String componentsXml =
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> " +
			"<Components xmlns=\"urn:schemas-cosylab-com:Components:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> " +
			createComponentsCdbXml(separateConfigComponent1, "IDL_TYPE_1", "some.class1", "zampaione", true, 2, 8) +
			createComponentsCdbXml(separateConfigComponent2, "IDL_TYPE_2", "some.class2", "zampaione", true, 5, 6) +
			"</Components>";
		logger.info("componentsXml = " + componentsXml);
		String cdbComponentsPath = "MACI/Components";
		logConfig.setCDB(testCDB);
		testCDB.addCurlToXmlMapping(cdbComponentsPath, componentsXml);
		logConfig.setCDBComponentPath(separateConfigComponent1, cdbComponentsPath);
		logConfig.setCDBComponentPath(separateConfigComponent2, cdbComponentsPath);

		String expr = "//_[@Name='" + separateConfigComponent2 + "']/ComponentLogger";
		xml = logConfig.getLogConfigXml("MACI/Components", expr);
		assertNotNull(xml);
		logger.info("Got component config xml: " + xml);
		UnnamedLogger compLoggerConfig = UnnamedLogger.unmarshalUnnamedLogger(new StringReader(xml));
		assertNotNull(compLoggerConfig);
	}
	
	
	/**
	 * Tests logging config from the CDB, for both cases
	 * (a) that env vars beat CDB settings e.g. for normal CDB reading,
	 * (b) that CDB beats env vars e.g. during a refresh from CDB triggered via LoggingConfigurable API. 
	 */
	public void testCDBValues() throws Exception {
		logger.info("============ Running testCDBValues ============");

		// we simulate an ACS_LOG_STDOUT env var setting
		String ACS_LOG_STDOUT_ORIGINAL = System.getProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL_LOCAL);
		String ACS_LOG_REMOTE_ORIGINAL = System.getProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL);
		String ACS_LOG_STDOUT = "" + AcsLogLevelDefinition.EMERGENCY.value;
		assertFalse("Fix this test to chose a different env var than the default", ACS_LOG_STDOUT.equals(ACS_LOG_STDOUT_ORIGINAL));
		System.setProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL_LOCAL, ACS_LOG_STDOUT);
		logger.info("Set property (env var) for local default level to " + ACS_LOG_STDOUT);
		
		// and remove any possibly present property from env var ACS_LOG_CENTRAL
		System.clearProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL);
		logger.info("Removed property (env var) for remote default level");
		
		// the schema defaults as reference
		LoggingConfig schemaDefaults = new LoggingConfig(); 
		AcsLogLevelDefinition defaultMinLogLevel = AcsLogLevelDefinition.fromXsdLogLevel(schemaDefaults.getMinLogLevel());
		AcsLogLevelDefinition defaultMinLogLevelLocal = AcsLogLevelDefinition.fromXsdLogLevel(schemaDefaults.getMinLogLevelLocal());

		// before we read the CDB, let's verify that the env var and default log levels are correct
		logConfig.initialize(false);
		assertEquals(defaultMinLogLevel, logConfig.getDefaultMinLogLevel());
		assertEquals(AcsLogLevelDefinition.EMERGENCY, logConfig.getDefaultMinLogLevelLocal());
		
		// the simulated test CDB to configure our loggers from
		String cdbContainerPath = "MACI/Containers/frodoContainer";
        String frodoContainerXml = 
            "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> " +  
                    "<Container xmlns=\"urn:schemas-cosylab-com:Container:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:log=\"urn:schemas-cosylab-com:LoggingConfig:1.0\">" + 
                          "<LoggingConfig " +
                          " minLogLevel=\"4\" " + 
                          " minLogLevelLocal=\"3\" " +
                          " centralizedLogger=\"LogForFrodo\" " +
                          " maxLogQueueSize=\"200\" " +
                          " immediateDispatchLevel=\"8\" " +
                          " dispatchPacketSize=\"33\" " +
                          " >" +
                               "<log:_ Name=\"MyMuteComponent\" minLogLevel=\"5\" minLogLevelLocal=\"6\" />" +
                          "</LoggingConfig>" +
                    "</Container>";
        
		TestCDB testCDB = new TestCDB();
		testCDB.addCurlToXmlMapping(cdbContainerPath, frodoContainerXml);		
		logConfig.setCDBLoggingConfigPath(cdbContainerPath);
		logConfig.setCDB(testCDB);
		
		// first the normal case where the env var default level beats the CDB default level
		logConfig.initialize(false);
		assertEquals("CDB must beat schema default", AcsLogLevelDefinition.INFO, logConfig.getDefaultMinLogLevel());
		assertEquals("Env var must beat CDB", AcsLogLevelDefinition.EMERGENCY, logConfig.getDefaultMinLogLevelLocal());		
		assertEquals("LogForFrodo", logConfig.getCentralizedLogger());
		assertSame(AcsLogLevelDefinition.ERROR, logConfig.getImmediateDispatchLevel());
		assertEquals(33, logConfig.getDispatchPacketSize());		
		assertEquals(200, logConfig.getMaxLogQueueSize());
		Set<String> loggerNames = logConfig.getLoggerNames();		
		assertEquals(1, loggerNames.size());
		assertTrue(loggerNames.contains("MyMuteComponent"));
		assertEquals(schemaDefaults.getFlushPeriodSeconds(), logConfig.getFlushPeriodSeconds()); // was not in CDB, thus default should be used
		
		// next the special case of CDB refresh via dynamic API, where the CDB beats the env var default levels
		logConfig.initialize(true);
		assertSame("CDB must beat schema default", AcsLogLevelDefinition.INFO, logConfig.getDefaultMinLogLevel());
		assertSame("CDB must beat env var", AcsLogLevelDefinition.DEBUG, logConfig.getDefaultMinLogLevelLocal());		
		assertEquals("LogForFrodo", logConfig.getCentralizedLogger());
		assertSame(AcsLogLevelDefinition.ERROR, logConfig.getImmediateDispatchLevel());
		assertEquals(33, logConfig.getDispatchPacketSize());		
		assertEquals(200, logConfig.getMaxLogQueueSize());
		loggerNames = logConfig.getLoggerNames();		
		assertEquals(1, loggerNames.size());
		assertTrue(loggerNames.contains("MyMuteComponent"));
		assertEquals(schemaDefaults.getFlushPeriodSeconds(), logConfig.getFlushPeriodSeconds()); // was not in CDB, thus default should be used
				
		UnnamedLogger myMuteloggerConfig = logConfig.getNamedLoggerConfig("MyMuteComponent");
		assertEquals(LogLevel.VALUE_5, myMuteloggerConfig.getMinLogLevel());
		assertEquals(LogLevel.VALUE_6, myMuteloggerConfig.getMinLogLevelLocal());

		// Test logger configuration given in the CDB separately for a component in the Components.xml file, not with the rest of LoggingConfig in the Container xml.
		String separateConfigComponent1 = "testComp1";
		String separateConfigComponent2 = "testComp2";
		String componentsXml =
			"<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> " +
			"<Components xmlns=\"urn:schemas-cosylab-com:Components:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\"> " +
			createComponentsCdbXml(separateConfigComponent1, "IDL_TYPE_1", "some.class1", "zampaione", true, 2, 8) +
			createComponentsCdbXml(separateConfigComponent2, "IDL_TYPE_2", "some.class2", "zampaione", true, 5, 6) +
			"</Components>";
		logger.info("componentsXml = " + componentsXml);
		String cdbComponentsPath = "MACI/Components";
		testCDB.addCurlToXmlMapping(cdbComponentsPath, componentsXml);
		logConfig.setCDBComponentPath(separateConfigComponent1, cdbComponentsPath);
		logConfig.setCDBComponentPath(separateConfigComponent2, cdbComponentsPath);
		logConfig.initialize(false);
		loggerNames = logConfig.getLoggerNames();		
//		assertEquals(2, loggerNames.size());
		assertTrue(loggerNames.contains("MyMuteComponent"));		
//		assertTrue(loggerNames.contains(separateConfigComponent1));
//		assertTrue(loggerNames.contains(separateConfigComponent2));
		UnnamedLogger separateConfig1 = logConfig.getNamedLoggerConfig(separateConfigComponent1);
		assertEquals(LogLevel.VALUE_2, separateConfig1.getMinLogLevel());
		assertEquals(LogLevel.VALUE_8, separateConfig1.getMinLogLevelLocal());
		UnnamedLogger separateConfig2 = logConfig.getNamedLoggerConfig(separateConfigComponent2);
		assertEquals(LogLevel.VALUE_5, separateConfig2.getMinLogLevel());
		assertEquals(LogLevel.VALUE_6, separateConfig2.getMinLogLevelLocal());
		
		// restore env vars (probably not necessary)
		if (ACS_LOG_STDOUT_ORIGINAL != null) {
			System.setProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL_LOCAL, ACS_LOG_STDOUT_ORIGINAL);
		}
		if (ACS_LOG_REMOTE_ORIGINAL != null) {
			System.setProperty(LogConfig.PROPERTYNAME_MIN_LOG_LEVEL, ACS_LOG_REMOTE_ORIGINAL);
		}
	}

	
	
	
	
	
	/**
	 * Checks the response to bad CDB data and CDB access problems. 
	 */
	public void testInvalidCDB() throws Exception {
		logger.info("============ Running testInvalidCDB ============");
		
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
		
		logConfig.setCDBLoggingConfigPath(cdbContainerPath);
		logConfig.setCDB(testCDB);
		
		try {
			logConfig.initialize(true);
			fail("LogConfigException was expected.");
		}
		catch (LogConfigException ex) {
			assertEquals("Log config initialization at least partially failed. Failed to parse XML for CDB node MACI/Containers/frodoContainer into binding classes " +
					"(ex=org.exolab.castor.xml.MarshalException, msg='unable to add attribute \"minLogLevel\" to 'alma.maci.loggingconfig.LoggingConfig' due to the following error: " +
					"java.lang.IllegalStateException: java.lang.IllegalArgumentException: 'NotANumber' is not a valid LogLevel'). ", 
					ex.getMessage());
		}
		
		testCDB.setThrowException(true);
		try {
			logConfig.initialize(true);
			fail("LogConfigException was expected.");
		}
		catch (LogConfigException ex) {
			assertEquals("Log config initialization at least partially failed. Node MACI/Containers/frodoContainer does not exist in the CDB (msg='CDB record does not exist'). ", ex.getMessage());
		}
		
	}
	
	
	/**
	 * Tests runtime changes as they could happen via the {@linkplain LoggingConfigurable} API. 
	 */
	public void testDynamicChanges() throws Exception {
		logger.info("============ Running testDynamicChanges ============");

		// the default log levels from schema defaults and optional env var setting
		AcsLogLevelDefinition defaultMinLogLevel = logConfig.getDefaultMinLogLevel();
		AcsLogLevelDefinition defaultMinLogLevelLocal = logConfig.getDefaultMinLogLevelLocal();

		// change the default log levels
		AcsLogLevelDefinition newDefaultLevel = defaultMinLogLevel.getNextHigherLevel();
		assertNotNull(newDefaultLevel);
		logConfig.setDefaultMinLogLevel(newDefaultLevel);
		assertSame(newDefaultLevel, logConfig.getDefaultMinLogLevel());
		assertSame(defaultMinLogLevelLocal, logConfig.getDefaultMinLogLevelLocal());
		newDefaultLevel = newDefaultLevel.getNextHigherLevel();
		assertNotNull(newDefaultLevel);
		logConfig.setDefaultMinLogLevel(newDefaultLevel);
		logConfig.setDefaultMinLogLevelLocal(newDefaultLevel);
		assertSame(newDefaultLevel, logConfig.getDefaultMinLogLevel());
		assertSame(newDefaultLevel, logConfig.getDefaultMinLogLevelLocal());
		logConfig.setDefaultMinLogLevel(defaultMinLogLevel); // restore initial values
		logConfig.setDefaultMinLogLevelLocal(defaultMinLogLevelLocal);
		
		// named logger levels 
		String knownLoggerName = "knownLogger";
		UnnamedLogger knownLoggerConfig = logConfig.getNamedLoggerConfig(knownLoggerName); // now the logger is known, even though it has default values 
		assertEquals(defaultMinLogLevel, AcsLogLevelDefinition.fromXsdLogLevel(knownLoggerConfig.getMinLogLevel()));
		assertEquals(defaultMinLogLevelLocal, AcsLogLevelDefinition.fromXsdLogLevel(knownLoggerConfig.getMinLogLevelLocal()));
		Set<String> loggerNames = logConfig.getLoggerNames();
		assertEquals(1, loggerNames.size());
		assertTrue(loggerNames.contains(knownLoggerName));

		String unknownLoggerName = "unknownLogger";
		logConfig.setMinLogLevel(AcsLogLevelDefinition.DEBUG, unknownLoggerName); // first encounter with this logger when setting its levels
		loggerNames = logConfig.getLoggerNames();		
		assertEquals(2, loggerNames.size());
		assertTrue(loggerNames.contains(knownLoggerName));
		assertTrue(loggerNames.contains(unknownLoggerName));
		
		// make sure that returned objects are not "live" for the logConfig data
		knownLoggerConfig = logConfig.getNamedLoggerConfig(knownLoggerName);
		UnnamedLogger knownLoggerConfig2 = logConfig.getNamedLoggerConfig(knownLoggerName);
		assertNotSame(knownLoggerConfig, knownLoggerConfig2);
		newDefaultLevel = newDefaultLevel.getNextHigherLevel();
		assertNotNull(newDefaultLevel);
		knownLoggerConfig.setMinLogLevel(newDefaultLevel.toXsdLevel());
		assertSame(defaultMinLogLevel, logConfig.getDefaultMinLogLevel());
		newDefaultLevel = newDefaultLevel.getNextHigherLevel();
		assertNotNull(newDefaultLevel);
		logConfig.setDefaultMinLogLevel(newDefaultLevel);
		assertEquals(defaultMinLogLevel, AcsLogLevelDefinition.fromXsdLogLevel(knownLoggerConfig2.getMinLogLevel()));
	}
	
	
	public void testLockingRemoteLevel() throws Exception {
		logger.info("============ Running testLockingLevels ============");

		// named logger levels 
		String loggerName = "jacorb@archiveContainer";
		logConfig.getNamedLoggerConfig(loggerName); // now the logger is known, even though it has default values
		logConfig.setAndLockMinLogLevel(AcsLogLevelDefinition.OFF, loggerName);
		
		// once a level is locked, it must not be changed any more:
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(loggerName).getMinLogLevel()));
		logConfig.setMinLogLevel(AcsLogLevelDefinition.TRACE, loggerName); 
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(loggerName).getMinLogLevel()));
		logConfig.setAndLockMinLogLevel(AcsLogLevelDefinition.TRACE, loggerName);
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(loggerName).getMinLogLevel()));
		logConfig.clearNamedLoggerConfig(loggerName);
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevelDefinition.fromXsdLogLevel(logConfig.getNamedLoggerConfig(loggerName).getMinLogLevel()));
		
		logConfig.initialize(false);
		logConfig.initialize(true);
		
		String cdbContainerPath = "MACI/Containers/archiveContainer";
        String archiveContainerXml = 
            "<?xml version=\"1.0\" encoding=\"ISO-8859-1\"?> " +  
                    "<Container xmlns=\"urn:schemas-cosylab-com:Container:1.0\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:log=\"urn:schemas-cosylab-com:LoggingConfig:1.0\">" + 
                          "<LoggingConfig>" +
                               "<log:_ Name=\"" + loggerName + "\" minLogLevel=\"2\" minLogLevelLocal=\"2\" />" +
                               "<log:_ Name=\"unlockedLogger\" minLogLevel=\"6\" minLogLevelLocal=\"6\" />" +
                          "</LoggingConfig>" +
                    "</Container>";        
		TestCDB testCDB = new TestCDB();
		testCDB.addCurlToXmlMapping(cdbContainerPath, archiveContainerXml);		
		logConfig.setCDBLoggingConfigPath(cdbContainerPath);
		logConfig.setCDB(testCDB);
		logConfig.initialize(true);
		assertEquals(LogLevel.VALUE_6, logConfig.getNamedLoggerConfig("unlockedLogger").getMinLogLevel()); // to make sure the CDB entry was considered
		assertSame(LogLevel.VALUE_99, logConfig.getNamedLoggerConfig(loggerName).getMinLogLevel());
		assertTrue(logConfig.getNamedLoggerConfig(loggerName).isLockedRemote());
	}
	
	
	////////////////////////////////////////////////
	//      helper methods 
	////////////////////////////////////////////////
	
	private String createComponentsCdbXml(String compName, String type, String code, String container, boolean configureLogger, int minLogLevel, int minLogLevelLocal) {
		String xml = 
			"<_ Name=\"" + compName + "\"" +
			"   Type=\"" + type + "\"" +
			"   Code=\"" + code + "\"" +
			"   Container=\"" + container + "\">";
		if (configureLogger) {
			xml += 
			"<ComponentLogger minLogLevel=\"" + minLogLevel + "\" minLogLevelLocal=\"" + minLogLevelLocal + "\" />";
		}
		xml += "</_>";
		return xml;
	}
}

