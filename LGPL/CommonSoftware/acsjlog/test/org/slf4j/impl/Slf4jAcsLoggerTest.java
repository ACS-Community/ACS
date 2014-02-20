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
		
		// hibernate SQL logger 1
		org.slf4j.Logger logger = org.slf4j.LoggerFactory.getLogger("org.hibernate.SQL");
		validateSlf4jLogger(logger, HibernateLoggerHelper.HIBERNATE_SQL_LOGGER_NAME_PREFIX);
		
		// hibernate SQL logger 2
		org.slf4j.Logger logger2 = org.slf4j.LoggerFactory.getLogger("org.hibernate.type.XYZ");
		validateSlf4jLogger(logger2, HibernateLoggerHelper.HIBERNATE_SQL_LOGGER_NAME_PREFIX);
		assertSame(logger, logger2);
		
		// hibernate standard logger
		org.slf4j.Logger logger3 = org.slf4j.LoggerFactory.getLogger("org.hibernate.cfg.Ejb3Column");
		validateSlf4jLogger(logger3, HibernateLoggerHelper.HIBERNATE_LOGGER_NAME_PREFIX);
		assertNotSame(logger, logger3);
		
		// jacorb logger
		org.slf4j.Logger logger4 = org.slf4j.LoggerFactory.getLogger("org.jacorb.whatever");
		validateSlf4jLogger(logger4, JacorbLoggerHelper.JACORB_LOGGER_NAME);
		
		// unrecognized logger
		org.slf4j.Logger logger5 = org.slf4j.LoggerFactory.getLogger("myNewFramework.logger3736");
		validateSlf4jLogger(logger5, "UnknownSlf4j");
	}
	
	private void validateSlf4jLogger(org.slf4j.Logger logger, String expectedName) {
		assertNotNull(logger);
		
		JDK14LoggerAdapter adapter = (JDK14LoggerAdapter) logger;
		Logger delegateLogger = adapter.logger;
		assertNotNull(delegateLogger);
		assertTrue(delegateLogger instanceof AcsLogger);
		assertEquals(expectedName, delegateLogger.getName());
	}

}
