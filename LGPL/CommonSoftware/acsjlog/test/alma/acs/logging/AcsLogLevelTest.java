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

	public AcsLogLevelTest()
	{
		super("AcsLogLevelTest");
	}

	/**
	 * Test AcsLogLevel Mapping of levels.
	 * {@link AcsLogLevel} stands between the JDK-defined {@link Level} which it inherits from, 
	 * and the "core" level integer constants defined in the ACS logging system, given in {@link ACSCoreLevel}.
	 * Conversion from and to core levels is done separately in test {@link #testACSCoreLevels()}. 
	 */
	public void testLevelMapping()
	{
		// ACS-level to ACS-level
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING));
		assertNotSame(AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING), AcsLogLevel.TRACE);

		// JDK-Level to ACS-level
		assertSame(AcsLogLevel.EMERGENCY, AcsLogLevel.getNativeLevel(Level.SEVERE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(Level.WARNING));
		assertSame(AcsLogLevel.INFO, AcsLogLevel.getNativeLevel(Level.INFO));
		assertSame(AcsLogLevel.DEBUG, AcsLogLevel.getNativeLevel(Level.CONFIG));
		assertSame(AcsLogLevel.DEBUG, AcsLogLevel.getNativeLevel(Level.FINE));
		assertSame(AcsLogLevel.DEBUG, AcsLogLevel.getNativeLevel(Level.FINER));
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(Level.FINEST));
		assertSame(AcsLogLevel.ALL, AcsLogLevel.getNativeLevel(Level.ALL));
		assertSame(null, AcsLogLevel.getNativeLevel(Level.OFF));

		// some repetitions to test lookup
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING));
		assertNotSame(AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING), AcsLogLevel.TRACE);
		assertSame(AcsLogLevel.EMERGENCY, AcsLogLevel.getNativeLevel(Level.SEVERE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(Level.WARNING));
		assertSame(AcsLogLevel.INFO, AcsLogLevel.getNativeLevel(Level.INFO));

		// compare expected and actual levels
		assertEquals(Integer.MIN_VALUE, AcsLogLevel.getNativeLevel(AcsLogLevel.ALL).intValue());
		assertEquals(300, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE).intValue());
		assertEquals(700, AcsLogLevel.getNativeLevel(AcsLogLevel.DEBUG).intValue());
		assertEquals(800, AcsLogLevel.getNativeLevel(AcsLogLevel.INFO).intValue());
		assertEquals(801, AcsLogLevel.getNativeLevel(AcsLogLevel.NOTICE).intValue());
		assertEquals(900, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING).intValue());
		assertEquals(901, AcsLogLevel.getNativeLevel(AcsLogLevel.ERROR).intValue());
		assertEquals(902, AcsLogLevel.getNativeLevel(AcsLogLevel.CRITICAL).intValue());
		assertEquals(903, AcsLogLevel.getNativeLevel(AcsLogLevel.ALERT).intValue());
		assertEquals(1000, AcsLogLevel.getNativeLevel(AcsLogLevel.EMERGENCY).intValue());		
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
		assertEquals(0, AcsLogLevel.ALL.getAcsLevel());
		assertEquals(2, AcsLogLevel.TRACE.getAcsLevel());
		assertEquals(3, AcsLogLevel.DEBUG.getAcsLevel());
		assertEquals(4, AcsLogLevel.INFO.getAcsLevel());
		assertEquals(5, AcsLogLevel.NOTICE.getAcsLevel());
		assertEquals(6, AcsLogLevel.WARNING.getAcsLevel());
		assertEquals(8, AcsLogLevel.ERROR.getAcsLevel());
		assertEquals(9, AcsLogLevel.CRITICAL.getAcsLevel());
		assertEquals(10, AcsLogLevel.ALERT.getAcsLevel());
		assertEquals(11, AcsLogLevel.EMERGENCY.getAcsLevel());
		
		// ACS-core-level to ACS-Level
		assertSame(AcsLogLevel.ALL, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_ALL));
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.fromAcsCoreLevel(1)); // undefined core level, should round up
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_TRACE));
		assertSame(AcsLogLevel.DEBUG, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_DEBUG));
		assertSame(AcsLogLevel.INFO, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_INFO));
		assertSame(AcsLogLevel.NOTICE, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_NOTICE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_WARNING));
		assertSame(AcsLogLevel.ERROR, AcsLogLevel.fromAcsCoreLevel(7)); // undefined core level, should round up
		assertSame(AcsLogLevel.ERROR, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_ERROR));
		assertSame(AcsLogLevel.CRITICAL, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_CRITICAL));
		assertSame(AcsLogLevel.ALERT, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_ALERT));
		assertSame(AcsLogLevel.EMERGENCY, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_EMERGENCY));
		assertSame(AcsLogLevel.EMERGENCY, AcsLogLevel.fromAcsCoreLevel(ACSCoreLevel.ACS_LEVEL_EMERGENCY + 10)); // undefined core level, should round down
	}
	
	public void testPrintMappings() {
		AcsLogLevel.printMappings(System.out);
	}
}