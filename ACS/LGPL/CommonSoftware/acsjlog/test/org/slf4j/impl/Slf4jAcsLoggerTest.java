package org.slf4j.impl;

import java.util.logging.Logger;

import alma.acs.logging.AcsLogger;

import junit.framework.TestCase;

public class Slf4jAcsLoggerTest extends TestCase
{
	/**
	 * Checks that slf4j is configured to use a single instance of AcsLogger.
	 * @TODO: check log levels etc
	 */
	public void testAcsLoggerFromSlf4j() {
		org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger("name to be ignored");
		assertTrue(logger instanceof JDK14LoggerAdapter);
		
		org.slf4j.Logger secondLogger = org.slf4j.LoggerFactory.getLogger("some other logger");
		assertSame(logger, secondLogger);
		
		JDK14LoggerAdapter adapter = (JDK14LoggerAdapter) logger;
		Logger delegateLogger = adapter.logger;
		assertNotNull(delegateLogger);
		assertTrue(delegateLogger instanceof AcsLogger);
		assertEquals("hibernate", delegateLogger.getName());
	}
}
