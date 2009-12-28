/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.logging;

import java.util.Set;
import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.formatters.ConsoleLogFormatter;
import alma.acs.logging.level.AcsLogLevelDefinition;

/**
 *
 * Tests the class ClientLogManager, which creates and configures all ACS loggers.
 * @author hsommer
 */
public class ClientLogManagerTest extends junit.framework.TestCase
{
	ClientLogManager clientLogManager;

	Logger m_logger = null;

	public ClientLogManagerTest(String name)
	{
		super(name);
	}

	protected void setUp() throws Exception {
		clientLogManager = ClientLogManager.getAcsLogManager();
	}

	protected void tearDown() throws Exception {
		clientLogManager.shutdown(true);
	}

	public void testLoggerStructure() {
		Logger containerLogger = clientLogManager.getLoggerForContainer("test");
		assertNotNull(containerLogger);
		Logger acsRemoteLogger = containerLogger.getParent();
		assertNotNull(acsRemoteLogger);
		assertFalse(acsRemoteLogger.getUseParentHandlers());
		Logger rootLogger = acsRemoteLogger.getParent();
		assertNotNull(rootLogger);
		assertNull(rootLogger.getParent());

		Handler[] handlers = containerLogger.getHandlers();
		assertTrue(handlers.length == 2);
		StdOutConsoleHandler localHandler = null;
		AcsLoggingHandler remoteHandler = null;
		for (Handler handler : handlers) {
			if (handler instanceof StdOutConsoleHandler) {
				localHandler = (StdOutConsoleHandler) handler;
			} else if (handler instanceof AcsLoggingHandler) {
				remoteHandler = (AcsLoggingHandler) handler;
			} else {
				fail("Unexpected handler type " + handler.getClass().getName() + " encountered.");
			}
		}
		assertNotNull(localHandler);
		assertNotNull(remoteHandler);
		Formatter localFormatter = localHandler.getFormatter();
		assertTrue("localFormatter should not be of type " + localFormatter.getClass().getName(),
				localFormatter instanceof ConsoleLogFormatter);

		Handler[] parentHandlers = acsRemoteLogger.getHandlers();
		assertTrue(parentHandlers.length == 0);
		assertEquals(AcsLogLevel.DELOUSE, remoteHandler.getLevel());

		containerLogger.info("I'm a good pedigree logger.");
	}

    
    public void testLoggerNameUniqueness() {
		AcsLogger compLogger1 = clientLogManager.getLoggerForComponent("component");
		assertEquals("component", compLogger1.getLoggerName());
		AcsLogger compLogger2 = clientLogManager.getLoggerForComponent("component_1");
		assertEquals("component_1", compLogger2.getLoggerName());
		AcsLogger compLogger3 = clientLogManager.getLoggerForComponent("component");
		assertSame(compLogger1, compLogger3); // reuse for same name and same logger type
		AcsLogger orbLogger1 = clientLogManager.getLoggerForCorba("component", false); // what a stupid name for an orb logger!
		assertEquals("component_2", orbLogger1.getLoggerName());
		AcsLogger containerLogger1 = clientLogManager.getLoggerForContainer("component"); // what a stupid name for a container logger!
		assertEquals("component_3", containerLogger1.getLoggerName());
	}

	public void testShutdown() {
		Logger containerLogger = clientLogManager.getLoggerForContainer("test");
		containerLogger.info("---------- testShutdown -----------");
		clientLogManager.shutdown(true);
		// should still log locally
		containerLogger.info("testShutdown: now with local handler only.");

		// must survive another call to shutdown after this in tearDown()
	}

	/**
	 * <ol>
	 * <li>Logs stupid messages to a container logger, while
	 * {@link ClientLogManager#initRemoteLogging(org.omg.CORBA.ORB, Manager, int, boolean)} has not been called.
	 * <li>Calls {@link ClientLogManager#suppressRemoteLogging()}
	 * <li>Forces garbage collection, and verifies that the previously filled log record queue gets finalized within a
	 * reasonable time.
	 * <li>While or after waiting for queue finalization, we log more messages to the same container logger, just to see
	 * that it doesn't fail (should be local-only).
	 * </ol>
	 * The ClientLogManager shutdown called in method tearDown is an important part of this test.
	 * 
	 * @throws Exception
	 */
	public void testSuppressRemoteLoggingAndCheckGC() throws Exception {
		final CyclicBarrier sync = new CyclicBarrier(2);

		// use a special ClientLogManager that has a queue with smart finalize method,
		// so that we can detect GC
		ClientLogManager specialClientLogManager = new ClientLogManager() {
			protected void prepareRemoteLogging() {
				logQueue = new DispatchingLogQueue() {
					protected void finalize() throws Exception {
						System.out.println("*************** DispatchingLogQueue getting finalized. ************");
						if (sync == null) {
							System.err.println("The final local variable 'sync' appeared as 'null' in the finalizer thread. "
											+ "So far this only happened in certain Eclipse JVMs. "
											+ "Turning it into a member variable would solve the problem.");
						} else {
							sync.await();
						}
					}
				};
				super.prepareRemoteLogging();
			}
		};
		Logger contLog1 = specialClientLogManager.getLoggerForContainer("contLog1");
		for (int i = 0; i < 400; i++) {
			contLog1.info("stupid queued message number " + i);
		}
		specialClientLogManager.suppressRemoteLogging();
		System.gc();
		// System.runFinalization();

		for (int i = 0; i < 50; i++) {
			contLog1.info("stupid local message number " + i);
		}
		try {
			if (sync.getNumberWaiting() == 0) {
				System.out
						.println("Local logging finished before LogQueue#finalize was called, despite an earlier call to System.gc(). "
								+ "Now waiting for at most 20 seconds...");
			}
			sync.await(20, TimeUnit.SECONDS);
		} catch (TimeoutException e) {
			fail("The disabled log queue was not garbage collected. Probably some bad code changes have created an immortal reference to it, which should be removed.");
		}
	}

	/**
	 */
	public void testSuppressCorbaRemoteLogging() {
		LogConfig config = clientLogManager.getLogConfig();
		
		assertEquals(1, config.getLoggerNames().size()); // the internal logger of ClientLogManager "alma.acs.logging"
		AcsLogger orbLog1 = clientLogManager.getLoggerForCorba("jacorb_1", true);
		
		assertEquals(2, config.getLoggerNames().size());
		AcsLogger orbLog1b = clientLogManager.getLoggerForCorba("jacorb_1", true);
		assertSame(orbLog1, orbLog1b);
		assertEquals(2, config.getLoggerNames().size());

		AcsLogger orbLog2 = clientLogManager.getLoggerForCorba("jacorb_2", true);
		assertNotSame(orbLog1, orbLog2);
		assertEquals(3, config.getLoggerNames().size());

		// From the container logger, the process name will be derived, and the corba loggers will get it appended
		AcsLogger contLog = clientLogManager.getLoggerForContainer("myContainer");
			
		Set<String> loggerNames = config.getLoggerNames();
		assertEquals(4, loggerNames.size());
		assertTrue(loggerNames.contains("alma.acs.logging"));
		assertTrue(loggerNames.contains("jacorb_1@myContainer"));
		assertTrue(loggerNames.contains("jacorb_2@myContainer"));
		assertTrue(loggerNames.contains("myContainer"));
		
		config.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.INFO);
		config.setDefaultMinLogLevel(AcsLogLevelDefinition.DEBUG);
		// the log level must be the smaller one of stdout and remote level
		assertEquals(Level.FINE, orbLog1.getLevel());
		assertEquals(Level.FINE, orbLog2.getLevel());
		assertEquals(Level.FINE, contLog.getLevel());
		
		clientLogManager.suppressCorbaRemoteLogging();
		
		// now for corba loggers the remote handler's level must be infinite, and the local level should determine the logger level. 
		assertEquals(AcsLogLevel.INFO, orbLog1.getLevel()); // todo: check levels directly on the handlers
		assertEquals(AcsLogLevel.INFO, orbLog2.getLevel());
		assertEquals(AcsLogLevel.FINE, contLog.getLevel());
		
		assertEquals(4, loggerNames.size()); 
	}
	
	/**
	 * As of ACS 8.1.0, it is not possible to configure application loggers via the CDB.
	 * This test demonstrates how to do it programmatically, as a workaround.
	 */
	public void testConfigureApplicationLogger() {
		
		LogConfig sharedLogConfig = clientLogManager.getLogConfig();

		// Set default log levels before creating the application logger
		AcsLogLevelDefinition localLevel = AcsLogLevelDefinition.WARNING;
		AcsLogLevelDefinition remoteLevel = AcsLogLevelDefinition.CRITICAL;
		sharedLogConfig.setDefaultMinLogLevelLocal(localLevel);
		sharedLogConfig.setDefaultMinLogLevel(remoteLevel);
		
		// get the logger
		AcsLogger applLogger = clientLogManager.getLoggerForApplication("testApplicationLogger", true);
		
		// verify levels set on handlers
		assertLogLevels(applLogger, localLevel, remoteLevel);
		
		// change the default log levels
		localLevel = AcsLogLevelDefinition.INFO;
		remoteLevel = AcsLogLevelDefinition.TRACE;
		sharedLogConfig.setDefaultMinLogLevelLocal(localLevel);
		sharedLogConfig.setDefaultMinLogLevel(remoteLevel);
		assertLogLevels(applLogger, localLevel, remoteLevel);

		assertLogLevels(applLogger, localLevel, remoteLevel);
	}
	
	public void testHibernateLoggerConfig() throws Exception {
		LogConfig sharedLogConfig = clientLogManager.getLogConfig();
		
		// default levels < WARNING for first hibernate logger, should result in custom levels WARNING
		sharedLogConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.TRACE);
		sharedLogConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.DEBUG);
		AcsLogger hibernateLogger1 = clientLogManager.getLoggerForCorba("hibernate", true);
		assertLogLevels(hibernateLogger1, AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.WARNING);
		assertTrue(sharedLogConfig.hasCustomConfig(hibernateLogger1.getLoggerName()));
		
		// default levels >= WARNING for second hibernate logger, should result in custom levels equal to default levels
		sharedLogConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.ERROR);
		sharedLogConfig.setDefaultMinLogLevel(AcsLogLevelDefinition.WARNING);
		AcsLogger hibernateLogger2 = clientLogManager.getLoggerForCorba("hibernateSql", true);
		assertLogLevels(hibernateLogger2, AcsLogLevelDefinition.ERROR, AcsLogLevelDefinition.WARNING);
		assertTrue(sharedLogConfig.hasCustomConfig(hibernateLogger2.getLoggerName()));
		
		// ClientLogManager should create new hibernate loggers ,no reuse
		assertNotSame("Hibernate logger reuse should happen inside org.slf4j.impl.ACSLoggerFactory, but not in ClientLogManager.", hibernateLogger1, hibernateLogger2);
	}
	
	/**
	 * Checks that an AcsLogger has local and remote handlers configured with the correct levels.
	 */
	private void assertLogLevels(AcsLogger logger, AcsLogLevelDefinition expectedLocalLevel, AcsLogLevelDefinition expectedRemoteLevel) {
		Handler[] handlers = logger.getHandlers();
		assertTrue(handlers.length == 2);
		StdOutConsoleHandler localHandler = null;
		AcsLoggingHandler remoteHandler = null;
		for (Handler handler : handlers) {
			if (handler instanceof StdOutConsoleHandler) {
				localHandler = (StdOutConsoleHandler) handler;
			} else if (handler instanceof AcsLoggingHandler) {
				remoteHandler = (AcsLoggingHandler) handler;
			} else {
				fail("Unexpected handler type " + handler.getClass().getName() + " encountered.");
			}
		}
		assertNotNull(localHandler);
		assertNotNull(remoteHandler);

		assertEquals(AcsLogLevel.getLowestMatchingJdkLevel(expectedLocalLevel), localHandler.getLevel());
		assertEquals(AcsLogLevel.getLowestMatchingJdkLevel(expectedRemoteLevel), remoteHandler.getLevel());
	}
}