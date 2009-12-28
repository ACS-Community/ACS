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

import java.io.PrintStream;
import java.util.logging.Level;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalArgumentEx;
import alma.AcsLogLevels.ALERT_NAME;
import alma.AcsLogLevels.ALERT_VAL;
import alma.AcsLogLevels.CRITICAL_NAME;
import alma.AcsLogLevels.CRITICAL_VAL;
import alma.AcsLogLevels.DEBUG_NAME;
import alma.AcsLogLevels.DEBUG_VAL;
import alma.AcsLogLevels.DELOUSE_NAME;
import alma.AcsLogLevels.DELOUSE_VAL;
import alma.AcsLogLevels.EMERGENCY_NAME;
import alma.AcsLogLevels.EMERGENCY_VAL;
import alma.AcsLogLevels.ERROR_NAME;
import alma.AcsLogLevels.ERROR_VAL;
import alma.AcsLogLevels.INFO_NAME;
import alma.AcsLogLevels.INFO_VAL;
import alma.AcsLogLevels.NOTICE_NAME;
import alma.AcsLogLevels.NOTICE_VAL;
import alma.AcsLogLevels.OFF_NAME;
import alma.AcsLogLevels.OFF_VAL;
import alma.AcsLogLevels.TRACE_NAME;
import alma.AcsLogLevels.TRACE_VAL;
import alma.AcsLogLevels.WARNING_NAME;
import alma.AcsLogLevels.WARNING_VAL;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.StringOutputStream;

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
		assertSame(AcsLogLevel.DELOUSE, AcsLogLevel.getNativeLevel(Level.FINER));
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(Level.FINEST));
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(Level.ALL));
		assertSame(AcsLogLevel.OFF, AcsLogLevel.getNativeLevel(Level.OFF));

		// some repetitions to test lookup
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING));
		assertNotSame(AcsLogLevel.getNativeLevel(AcsLogLevel.WARNING), AcsLogLevel.TRACE);
		assertSame(AcsLogLevel.EMERGENCY, AcsLogLevel.getNativeLevel(Level.SEVERE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getNativeLevel(Level.WARNING));
		assertSame(AcsLogLevel.INFO, AcsLogLevel.getNativeLevel(Level.INFO));

		// compare expected and actual levels
		assertEquals(300, AcsLogLevel.getNativeLevel(AcsLogLevel.ALL).intValue());
		assertEquals(300, AcsLogLevel.getNativeLevel(AcsLogLevel.TRACE).intValue());
		assertEquals(400, AcsLogLevel.getNativeLevel(AcsLogLevel.DELOUSE).intValue());
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
		assertEquals("Delouse", AcsLogLevel.DELOUSE.getEntryName());
		assertEquals("Debug", AcsLogLevel.DEBUG.getEntryName());
		assertEquals("Warning", AcsLogLevel.WARNING.getEntryName());
		assertEquals("Info", AcsLogLevel.INFO.getEntryName());
		assertEquals("Notice", AcsLogLevel.NOTICE.getEntryName());
		assertEquals("Error", AcsLogLevel.ERROR.getEntryName());
		assertEquals("Critical", AcsLogLevel.CRITICAL.getEntryName());
		assertEquals("Alert", AcsLogLevel.ALERT.getEntryName());
		assertEquals("Emergency", AcsLogLevel.EMERGENCY.getEntryName());

		assertEquals("TRACE", AcsLogLevel.TRACE.getName());
		assertEquals("DELOUSE", AcsLogLevel.DELOUSE.getName());
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
	 * Tests the encapsulation and conversion between the JDK-Logger-like levels used inside Java,
	 * and the ACS-levels defined as IDL constants and enum {@link AcsLogLevelDefinition} 
	 * that are used for communication outside of the JVM.
	 */
	public void testAcsCoreLevels() throws Exception
	{
		// Checks that our loglevel-enum correctly encapsulates the IDL-defined constants  
		assertEquals(TRACE_VAL.value, AcsLogLevelDefinition.TRACE.value);
		assertEquals(DELOUSE_VAL.value, AcsLogLevelDefinition.DELOUSE.value);
		assertEquals(DEBUG_VAL.value, AcsLogLevelDefinition.DEBUG.value);
		assertEquals(INFO_VAL.value, AcsLogLevelDefinition.INFO.value);
		assertEquals(NOTICE_VAL.value, AcsLogLevelDefinition.NOTICE.value);
		assertEquals(WARNING_VAL.value, AcsLogLevelDefinition.WARNING.value);
		assertEquals(ERROR_VAL.value, AcsLogLevelDefinition.ERROR.value);
		assertEquals(CRITICAL_VAL.value, AcsLogLevelDefinition.CRITICAL.value);
		assertEquals(ALERT_VAL.value, AcsLogLevelDefinition.ALERT.value);
		assertEquals(EMERGENCY_VAL.value, AcsLogLevelDefinition.EMERGENCY.value);
		assertEquals(OFF_VAL.value, AcsLogLevelDefinition.OFF.value);
		assertEquals("Number of log levels has changed, need to update this test.", 11, AcsLogLevelDefinition.values().length);
		
		// check integer value to enum conversion
		assertSame(AcsLogLevelDefinition.TRACE, AcsLogLevelDefinition.fromInteger(TRACE_VAL.value));
		assertSame(AcsLogLevelDefinition.DELOUSE, AcsLogLevelDefinition.fromInteger(DELOUSE_VAL.value));
		assertSame(AcsLogLevelDefinition.DEBUG, AcsLogLevelDefinition.fromInteger(DEBUG_VAL.value));
		assertSame(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.fromInteger(INFO_VAL.value));
		assertSame(AcsLogLevelDefinition.NOTICE, AcsLogLevelDefinition.fromInteger(NOTICE_VAL.value));
		assertSame(AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.fromInteger(WARNING_VAL.value));
		assertSame(AcsLogLevelDefinition.ERROR, AcsLogLevelDefinition.fromInteger(ERROR_VAL.value));
		assertSame(AcsLogLevelDefinition.CRITICAL, AcsLogLevelDefinition.fromInteger(CRITICAL_VAL.value));
		assertSame(AcsLogLevelDefinition.ALERT, AcsLogLevelDefinition.fromInteger(ALERT_VAL.value));
		assertSame(AcsLogLevelDefinition.EMERGENCY, AcsLogLevelDefinition.fromInteger(EMERGENCY_VAL.value));
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevelDefinition.fromInteger(OFF_VAL.value));
		try {
			AcsLogLevelDefinition.fromInteger(0);
			fail("undefined log level not allowed.");
		} catch (AcsJIllegalArgumentEx ex) {
			// good
		}		
		try {
			AcsLogLevelDefinition.fromInteger(Integer.MAX_VALUE);
			fail("undefined log level not allowed.");
		} catch (AcsJIllegalArgumentEx ex) {
			// good
			assertEquals("val", ex.getVariable());
			assertEquals(String.valueOf(Integer.MAX_VALUE), ex.getValue());
		}
		
		// check name to enum conversion
		assertSame(AcsLogLevelDefinition.TRACE, AcsLogLevelDefinition.fromName(TRACE_NAME.value));
		assertSame(AcsLogLevelDefinition.DELOUSE, AcsLogLevelDefinition.fromName(DELOUSE_NAME.value));
		assertSame(AcsLogLevelDefinition.DEBUG, AcsLogLevelDefinition.fromName(DEBUG_NAME.value));
		assertSame(AcsLogLevelDefinition.INFO, AcsLogLevelDefinition.fromName(INFO_NAME.value));
		assertSame(AcsLogLevelDefinition.NOTICE, AcsLogLevelDefinition.fromName(NOTICE_NAME.value));
		assertSame(AcsLogLevelDefinition.WARNING, AcsLogLevelDefinition.fromName(WARNING_NAME.value));
		assertSame(AcsLogLevelDefinition.ERROR, AcsLogLevelDefinition.fromName(ERROR_NAME.value));
		assertSame(AcsLogLevelDefinition.CRITICAL, AcsLogLevelDefinition.fromName(CRITICAL_NAME.value));
		assertSame(AcsLogLevelDefinition.ALERT, AcsLogLevelDefinition.fromName(ALERT_NAME.value));
		assertSame(AcsLogLevelDefinition.EMERGENCY, AcsLogLevelDefinition.fromName(EMERGENCY_NAME.value));
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevelDefinition.fromName(OFF_NAME.value));
		try {
			AcsLogLevelDefinition.fromName(null);
			fail("null log level name not allowed.");
		} catch (AcsJIllegalArgumentEx ex) {
			// good
		}		
		try {
			AcsLogLevelDefinition.fromName("");
			fail("empty log level name not allowed.");
		} catch (AcsJIllegalArgumentEx ex) {
			// good
		}		
		try {
			AcsLogLevelDefinition.fromName(" " + TRACE_NAME.value);
			fail("whitespace in log level name not allowed.");
		} catch (AcsJIllegalArgumentEx ex) {
			// good
		}		
		try {
			AcsLogLevelDefinition.fromName("hmpfgrnzlsm");
			fail("illegal log level name not allowed.");
		} catch (AcsJIllegalArgumentEx ex) {
			// good
		}
		
		// test the next-level method that should only be used by other tests
		assertSame(AcsLogLevelDefinition.DELOUSE, AcsLogLevelDefinition.TRACE.getNextHigherLevel());
		assertSame(AcsLogLevelDefinition.ERROR, AcsLogLevelDefinition.WARNING.getNextHigherLevel()); // values 6 -> 8
		assertNull(AcsLogLevelDefinition.EMERGENCY.getNextHigherLevel());
		assertNull(AcsLogLevelDefinition.OFF.getNextHigherLevel());
		
		// AcsLogLevel (which extends/overwrites some of the JDK log Levels) each maps to exactly one AcsLogLevelDefinition enum
		assertSame(AcsLogLevelDefinition.TRACE, AcsLogLevel.TRACE.getAcsLevel());
		assertSame(AcsLogLevelDefinition.DELOUSE, AcsLogLevel.DELOUSE.getAcsLevel());
		assertSame(AcsLogLevelDefinition.DEBUG, AcsLogLevel.DEBUG.getAcsLevel());
		assertSame(AcsLogLevelDefinition.INFO, AcsLogLevel.INFO.getAcsLevel());
		assertSame(AcsLogLevelDefinition.NOTICE, AcsLogLevel.NOTICE.getAcsLevel());
		assertSame(AcsLogLevelDefinition.WARNING, AcsLogLevel.WARNING.getAcsLevel());
		assertSame(AcsLogLevelDefinition.ERROR, AcsLogLevel.ERROR.getAcsLevel());
		assertSame(AcsLogLevelDefinition.CRITICAL, AcsLogLevel.CRITICAL.getAcsLevel());
		assertSame(AcsLogLevelDefinition.ALERT, AcsLogLevel.ALERT.getAcsLevel());
		assertSame(AcsLogLevelDefinition.EMERGENCY, AcsLogLevel.EMERGENCY.getAcsLevel());
		assertSame(AcsLogLevelDefinition.OFF, AcsLogLevel.OFF.getAcsLevel());

		// ACS-core-level to ACS-Level 
		assertSame(AcsLogLevel.TRACE, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.TRACE));
		assertSame(AcsLogLevel.DELOUSE, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.DELOUSE));
		assertSame(Level.FINE, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.DEBUG));
		assertSame(AcsLogLevel.INFO, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.INFO));
		assertSame(AcsLogLevel.NOTICE, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.NOTICE));
		assertSame(AcsLogLevel.WARNING, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.WARNING));
		assertSame(AcsLogLevel.ERROR, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.ERROR));
		assertSame(AcsLogLevel.CRITICAL, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.CRITICAL));
		assertSame(AcsLogLevel.ALERT, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.ALERT));
		assertSame(AcsLogLevel.EMERGENCY, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.EMERGENCY));
		assertSame(AcsLogLevel.OFF, AcsLogLevel.getLowestMatchingJdkLevel(AcsLogLevelDefinition.OFF));
	}
	
	public void testPrintMappings() {
		StringOutputStream stringOut = new StringOutputStream();
		AcsLogLevel.printMappings(new PrintStream(stringOut));
		System.out.println("Printed level mappings:\n" + stringOut);
		
		String sep = System.getProperty("line.separator");
		String expected = 
			"Trace" +     '\t' +  "300" + '\t' +  "1" + sep + 
			"Delouse" +   '\t' +  "400" + '\t' +  "2" + sep + 
			"Debug" +     '\t' +  "700" + '\t' +  "3" + sep + 
			"Info" +      '\t' +  "800" + '\t' +  "4" + sep + 
			"Notice" +    '\t' +  "801" + '\t' +  "5" + sep + 
			"Warning" +   '\t' +  "900" + '\t' +  "6" + sep + 
			"Error" +     '\t' +  "901" + '\t' +  "8" + sep + 
			"Critical" +  '\t' +  "902" + '\t' +  "9" + sep + 
			"Alert" +     '\t' +  "903" + '\t' + "10" + sep + 
			"Emergency" + '\t' + "1000" + '\t' + "11" + sep +
			"Off" +       '\t' + "2147483647" + '\t' + "99" + sep;
		assertEquals(expected, stringOut.toString());
	}
}
