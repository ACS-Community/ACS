/*
 * @@COPYRIGHT@@
 */
 
package abeans.pluggable.acs.logging.test;

import java.util.logging.Level;

import abeans.pluggable.acs.logging.LoggingLevel;

import junit.framework.TestCase;
import junit.framework.TestSuite;


/**
 * Test for LoggingLevel.
 * 
 * @author		Matej Sekoranja (matej.sekoranja@cosylab.com)
 * @version	@@VERSION@@
 */
public class LoggingLevelTest extends TestCase
{

	public LoggingLevelTest(String name)
	{
		super(name);
	}

	/**
	 */
	public static TestSuite suite()
	{
		return new TestSuite(LoggingLevelTest.class);
	}

	// 	Available:
	//	SEVERE (highest value)  -- EMERGENCY 						1000
	//	WARNING 				-- WARNING (ERROR CRITICAL ALERT)	 900
	//	INFO 					-- INFO	(NOTICE)					 800
	//	CONFIG 					-- DEBUG							 700
	//	FINE 					-- DEBUG							 500
	//	FINER 					-- TRACE							 400
	//	FINEST (lowest value) 	-- TRACE							 300
	//
	//  ALL Integer.MIN_VALUE   -- TRACE and above					Integer.MIN_VALUE
	//  OFF Integer.MAX_VALUE   -- null 							Integer.MAX_VALUE


	/**
	 * Test LoggingLevel to native mapping
	 */
	public void testLevelMapping()
	{ 
		// native to native
		assertEquals(LoggingLevel.TRACE, LoggingLevel.getNativeLevel(LoggingLevel.TRACE));
		assertEquals(LoggingLevel.WARNING, LoggingLevel.getNativeLevel(LoggingLevel.WARNING));
		assertTrue(LoggingLevel.getNativeLevel(LoggingLevel.WARNING) != LoggingLevel.TRACE);
		
		// Level to native
		assertEquals(LoggingLevel.EMERGENCY, LoggingLevel.getNativeLevel(Level.SEVERE));
		assertEquals(LoggingLevel.WARNING, LoggingLevel.getNativeLevel(Level.WARNING));
		assertEquals(LoggingLevel.INFO, LoggingLevel.getNativeLevel(Level.INFO));
		assertEquals(LoggingLevel.DEBUG, LoggingLevel.getNativeLevel(Level.CONFIG));
		assertEquals(LoggingLevel.DEBUG, LoggingLevel.getNativeLevel(Level.FINE));
		assertEquals(LoggingLevel.TRACE, LoggingLevel.getNativeLevel(Level.FINER));
		assertEquals(LoggingLevel.TRACE, LoggingLevel.getNativeLevel(Level.FINEST));
		assertEquals(LoggingLevel.TRACE, LoggingLevel.getNativeLevel(Level.ALL));
		assertEquals(null, LoggingLevel.getNativeLevel(Level.OFF));

		// some repetitions to test lookup
		assertEquals(LoggingLevel.TRACE, LoggingLevel.getNativeLevel(LoggingLevel.TRACE));
		assertEquals(LoggingLevel.WARNING, LoggingLevel.getNativeLevel(LoggingLevel.WARNING));
		assertTrue(LoggingLevel.getNativeLevel(LoggingLevel.WARNING) != LoggingLevel.TRACE);
		assertEquals(LoggingLevel.EMERGENCY, LoggingLevel.getNativeLevel(Level.SEVERE));
		assertEquals(LoggingLevel.WARNING, LoggingLevel.getNativeLevel(Level.WARNING));
		assertEquals(LoggingLevel.INFO, LoggingLevel.getNativeLevel(Level.INFO));
	}

	/**
	 * Test LoggingLevel Entry names.
	 */
	public void testLevelEntryNames()
	{ 
		// test some
		assertEquals("Trace", LoggingLevel.TRACE.getEntryName());
		assertEquals("Debug", LoggingLevel.DEBUG.getEntryName());
		assertEquals("Warning", LoggingLevel.WARNING.getEntryName());
		assertEquals("Info", LoggingLevel.INFO.getEntryName());
		assertEquals("Emergency", LoggingLevel.EMERGENCY.getEntryName());

	}

	/**
	 * Test LoggingLevel to ACS core levels.
	 */
	public void testACSCoreLevels()
	{ 
		// test some
		assertEquals(2, LoggingLevel.TRACE.getAcsLevel());
		assertEquals(3, LoggingLevel.DEBUG.getAcsLevel());
		assertEquals(6, LoggingLevel.WARNING.getAcsLevel());
		assertEquals(4, LoggingLevel.INFO.getAcsLevel());
		assertEquals(11, LoggingLevel.EMERGENCY.getAcsLevel());

	}
}