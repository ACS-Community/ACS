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

import java.util.logging.Formatter;
import java.util.logging.Handler;
import java.util.logging.Logger;

import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.TimeoutException;

import alma.acs.logging.formatters.ConsoleLogFormatter;

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
        assertTrue(handlers.length == 1);
        Handler localHandler = handlers[0];
        assertTrue(localHandler instanceof StdOutConsoleHandler);
        Formatter localFormatter = localHandler.getFormatter();
        assertTrue(localFormatter instanceof ConsoleLogFormatter);

        Handler[] parentHandlers = acsRemoteLogger.getHandlers();
        assertTrue(parentHandlers.length == 1);
        Handler parentHandler = parentHandlers[0];
        assertTrue(parentHandler instanceof AcsLoggingHandler);        
        AcsLoggingHandler remoteHandler = (AcsLoggingHandler) parentHandler;
        assertEquals(AcsLogLevel.TRACE, remoteHandler.getLevel());
        
        containerLogger.info("I'm a good pedigree logger.");
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
     *     {@link ClientLogManager#initRemoteLogging(org.omg.CORBA.ORB, Manager, int, boolean)} has not been called.
     * <li>Calls {@link ClientLogManager#suppressRemoteLogging()}
     * <li>Forces garbage collection, and verifies that the previously filled log record queue 
     *     gets finalized within a reasonable time.
     * <li>While or after waiting for queue finalization, we log more messages to the same container logger, 
     *     just to see that it doesn't fail (should be local-only).
     * </ol>
     * The ClientLogManager shutdown called in method tearDown is an important part of this test.
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
						}
						sync.await();
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
//        System.runFinalization();
        
        for (int i = 0; i < 50; i++) {
            contLog1.info("stupid local message number " + i);
        }
        try {
        	if (sync.getNumberWaiting() == 0) {
        		System.out.println("Local logging finished before LogQueue#finalize was called, despite an earlier call to System.gc(). "  
        				+ "Now waiting for at most 20 seconds...");
        	}
            sync.await(20, TimeUnit.SECONDS);
        } catch (TimeoutException e) {
            fail("The disabled log queue was not garbage collected. Probably some bad code changes have created an immortal reference to it, which should be removed.");
        }
    }

// For the time being we do not support logging configuration from properties, because values can be read from the CDB now.    
//    /**
//     * Assumes that the file acsjlog/test/test_userdef_logging.properties exists and defines the expected properties.
//     */
//    public void testUserdefinedLoglevels() {
//    	String propertyFileName = "test_userdef_logging.properties";
//    	System.setProperty("java.util.logging.config.file", propertyFileName);
//    	
//    	clientLogManager.setUserLogConfiguration();
//    	
//    	Level stdoutLevel = clientLogManager.getLoggerLevel("alma.acs.logging.StdOutConsoleHandler");
//    	assertEquals(Level.FINE, stdoutLevel);
//    	
//    	Level containerLevel = clientLogManager.getLoggerLevel("alma.acs.container.frodoContainer");
//    	assertEquals(Level.INFO, containerLevel);
//    	
//    	System.setProperty("java.util.logging.config.file", "");
//    }    
	
}