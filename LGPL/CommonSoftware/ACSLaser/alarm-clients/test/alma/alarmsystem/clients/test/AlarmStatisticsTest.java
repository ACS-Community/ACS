/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.clients.test;

import java.net.URL;
import java.sql.Timestamp;
import java.util.Collection;
import java.util.Properties;

import alma.alarmsystem.clients.alarm.AlarmStatistics;
import alma.alarmsystem.clients.alarm.AlarmStatistics.AlarmStatField;
import alma.alarmsystem.clients.test.utils.AlarmForTesting;
import junit.framework.TestCase;

public class AlarmStatisticsTest  extends TestCase {
	
	/**
	 * The {@link AlarmStatistics} object to test
	 */
	private AlarmStatistics stats;

	@Override
	protected void setUp() throws Exception {
		stats=new AlarmStatistics();
		assertNotNull(stats);
		
		super.setUp();
	}
	
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Thest the updating of a single field
	 * 
	 * @throws Exception
	 */
	public void testUpdateField() throws Exception {
		// The field to test: one is like the the others
		AlarmStatField field = AlarmStatField.PRI2;
		
		// Intial value... 
		assertTrue(stats.getStatValue(field)==0);
		
		// Lets update a few values as active
		final int activesUpdate=100;
		for (int t=0; t<activesUpdate; t++) {
			stats.updateField(field, true);
		}
		assertEquals("Statistics for active reported ",activesUpdate, stats.getStatValue(field).intValue());
		
		// Now test for inactive
		final int terminateesUpdate=50;
		for (int t=0; t<terminateesUpdate; t++) {
			stats.updateField(field, false);
		}
		assertEquals("Statistics for terminate reported ",activesUpdate-terminateesUpdate, stats.getStatValue(field).intValue());
		
		// Now check that a stats never goes below 0
		for (int t=0; t<1000; t++) {
			stats.updateField(field, false);
		}
		assertEquals("Statistics should never be less then 0 ",0, stats.getStatValue(field).intValue());
	}
	
	/**
	 * Now that the updating of a filed works as expected, we check
	 * if updating for an alarm is updating the right fields
	 * 
	 * @throws Exception
	 */
	public void testUpdateAlarm() throws Exception {
		
		AlarmForTesting alarm = new AlarmForTesting(
				"faultFamily", "faultMember", 0, 
				1,
				true,
				false,
				false);

		stats.update(alarm);
		assertEquals(1, stats.getStatValue(AlarmStatField.ACTIVE).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI1).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.PRI2).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.PRI3).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.PRI4).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MASKED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.REDUCED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_PARENT).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_PARENT).intValue());
		
		alarm.priority=2;
		stats.update(alarm);
		assertEquals(2, stats.getStatValue(AlarmStatField.ACTIVE).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI1).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI2).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.PRI3).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.PRI4).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MASKED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.REDUCED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_PARENT).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_PARENT).intValue());
		
		alarm.priority=3;
		stats.update(alarm);
		assertEquals(3, stats.getStatValue(AlarmStatField.ACTIVE).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI1).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI2).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI3).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.PRI4).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MASKED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.REDUCED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_PARENT).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_PARENT).intValue());
		
		alarm.priority=4;
		stats.update(alarm);
		assertEquals(4, stats.getStatValue(AlarmStatField.ACTIVE).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI1).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI2).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI3).intValue());
		assertEquals(1, stats.getStatValue(AlarmStatField.PRI4).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MASKED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.REDUCED).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.MULTIPLICITY_PARENT).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_CHILD).intValue());
		assertEquals(0, stats.getStatValue(AlarmStatField.NODE_PARENT).intValue());
	}
	
}
