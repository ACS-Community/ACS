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
package alma.acs.util;

import junit.framework.TestCase;

/**
 */
public class DateConversionTest extends TestCase
{
	
	/**
	 * Tests the two methods
	 * {@link UTCUtility#utcJavaToOmg} and
	 * {@link UTCUtility#utcOmgToJava} by converting the current
	 * system time to OMG time and back, asserting it be equal. 
	 */
	public void testDateConversionRoundtrip()
	{
		long nowJ = System.currentTimeMillis();
		long nowGreg = UTCUtility.utcJavaToOmg(nowJ);
		long nowJ2 = UTCUtility.utcOmgToJava(nowGreg);
		assertEquals(nowJ, nowJ2);		
	}

	public void testGetDate()
	{
		String date1 = UTCUtility.getUTCDate(5L);
		assertEquals("1970-01-01T00:00:00.005", date1);
	}
	
}
