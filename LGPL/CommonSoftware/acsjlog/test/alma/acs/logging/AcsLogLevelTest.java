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

import java.util.logging.Level;

/**
 * 
 * @author  Matej Sekoranja (matej.sekoranja@cosylab.com)
 *
 *Tests the AcsLogLevel
 */
public class AcsLogLevelTest extends junit.framework.TestCase
{

	public AcsLogLevelTest(String name)
	{
		super(name);
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
	//  OFF Integer.MAX_VALUE   -- level not set 					Integer.MAX_VALUE
	//							   (as if no contLogger.setLevel defined)

	/**
	 * Test AcsLogLevel Mapping of levels.
	 */
	public void testLevelMapping()
	{
		// native to native
		assertEquals(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE));
		assertEquals(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING));
		assertTrue(AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING) != AcsLogLevel.TRACE);

		// Level to native
		assertEquals(AcsLogLevel.EMERGENCY, AcsLogLevel.getNativeLevel(Level.SEVERE));
		assertEquals(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(Level.WARNING));
		assertEquals(AcsLogLevel.INFO, AcsLogLevel.getNativeLevel(Level.INFO));
		assertEquals(AcsLogLevel.DEBUG, AcsLogLevel.getNativeLevel(Level.CONFIG));
		assertEquals(AcsLogLevel.DEBUG, AcsLogLevel.getNativeLevel(Level.FINE));
		assertEquals(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(Level.FINER));
		assertEquals(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(Level.FINEST));
		assertEquals(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(Level.ALL));
		assertEquals(null, AcsLogLevel.getNativeLevel(Level.OFF));

		// some repetitions to test lookup
		assertEquals(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE));
		assertEquals(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING));
		assertTrue(AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING) != AcsLogLevel.TRACE);
		assertEquals(AcsLogLevel.EMERGENCY, AcsLogLevel.getNativeLevel(Level.SEVERE));
		assertEquals(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(Level.WARNING));
		assertEquals(AcsLogLevel.INFO, AcsLogLevel.getNativeLevel(Level.INFO));

		// compare expected and actual levels
		assertEquals(400, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE).intValue());
		assertEquals(700, AcsLogLevel.getNativeLevel(AcsLogLevel.DEBUG).intValue());
		assertEquals(800, AcsLogLevel.getNativeLevel(AcsLogLevel.INFO).intValue());
		assertEquals(801, AcsLogLevel.getNativeLevel(AcsLogLevel.NOTICE).intValue());
		assertEquals(900, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING).intValue());
		assertEquals(901, AcsLogLevel.getNativeLevel(AcsLogLevel.ERROR).intValue());
		assertEquals(902, AcsLogLevel.getNativeLevel(AcsLogLevel.CRITICAL).intValue());
		assertEquals(903, AcsLogLevel.getNativeLevel(AcsLogLevel.ALERT).intValue());
		assertEquals(1000, AcsLogLevel.getNativeLevel(AcsLogLevel.EMERGENCY).intValue());
		assertEquals(400, AcsLogLevel.getNativeLevel(AcsLogLevel.ALL).intValue());
	}

	/**
	 * Test AcsLogLevel Entry names.
	 */
	public void testLevelEntryNames()
	{
		// test some
		assertEquals("Trace", AcsLogLevel.TRACE.getEntryName());
		assertEquals("Debug", AcsLogLevel.DEBUG.getEntryName());
		assertEquals("Warning", AcsLogLevel.WARNING.getEntryName());
		assertEquals("Info", AcsLogLevel.INFO.getEntryName());
		assertEquals("Notice", AcsLogLevel.NOTICE.getEntryName());
		assertEquals("Error", AcsLogLevel.ERROR.getEntryName());
		assertEquals("Critical", AcsLogLevel.CRITICAL.getEntryName());
		assertEquals("Alert", AcsLogLevel.ALERT.getEntryName());
		assertEquals("Emergency", AcsLogLevel.EMERGENCY.getEntryName());

		assertEquals("TRACE", AcsLogLevel.TRACE.getName());
		assertEquals("DEBUG", AcsLogLevel.DEBUG.getName());
		assertEquals("INFO", AcsLogLevel.INFO.getName());
		assertEquals("NOTICE", AcsLogLevel.NOTICE.getName());
		assertEquals("WARNING", AcsLogLevel.WARNING.getName());
		assertEquals("ERROR", AcsLogLevel.ERROR.getName());
		assertEquals("CRITICAL", AcsLogLevel.CRITICAL.getName());
		assertEquals("ALERT", AcsLogLevel.ALERT.getName());
		assertEquals("EMERGENCY", AcsLogLevel.EMERGENCY.getName());
	}

	/**
	 * Test AcsLogLevel to ACS core levels.
	 */
	public void testACSCoreLevels()
	{
		// test some
		assertEquals(2, AcsLogLevel.TRACE.getAcsLevel());
		assertEquals(3, AcsLogLevel.DEBUG.getAcsLevel());
		assertEquals(4, AcsLogLevel.INFO.getAcsLevel());
		assertEquals(5, AcsLogLevel.NOTICE.getAcsLevel());
		assertEquals(6, AcsLogLevel.WARNING.getAcsLevel());
		assertEquals(8, AcsLogLevel.ERROR.getAcsLevel());
		assertEquals(9, AcsLogLevel.CRITICAL.getAcsLevel());
		assertEquals(10, AcsLogLevel.ALERT.getAcsLevel());
		assertEquals(11, AcsLogLevel.EMERGENCY.getAcsLevel());
	}
}