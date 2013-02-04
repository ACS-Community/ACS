/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarm.tests;

import junit.framework.JUnit4TestAdapter;

import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.TestName;

import alma.acs.component.client.ComponentClient;
import alma.acs.util.AcsLocations;
import alma.alarmsystem.core.alarms.LaserCoreFaultState.LaserCoreFaultCodes;

/**
 * Test if the alarm server send/clear an alarm when the difference between the actual
 * time and the timestamp of the alarms received by the sources differs more then a threashold.
 * <BR>
 * The alarm this test want to check is {@link LaserCoreFaultCodes#ALARMS_TOO_OLD}.
 * 
 * @author acaproni
 * @since ACS 11.1
 */
public class AlarmsDelayCoreAlarmTest  extends ComponentClient {
	
	/**
	 * TODO: Check if this rule and the getMethodName() call in setUp() can be moved up to ComponentClient,
	 *      if that adds a runtime dependency on junit, and how bad that would be.
	 *      Probably we should add a class ComponentClientTestCaseJUnit4 that extends ComponentClient
	 *      and only adds this testname business.
	 */
	@Rule 
	public TestName testName = new TestName();
	
	/**
	 * For compatibility with JUnit3 based TATJUnitRunner
	 */
	public static junit.framework.Test suite() {
		return new JUnit4TestAdapter(AlarmsDelayCoreAlarmTest.class);
	}
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public AlarmsDelayCoreAlarmTest() throws Exception {
		super(null, AcsLocations.figureOutManagerLocation(), AlarmsDelayCoreAlarmTest.class.getSimpleName());
	}
	
	@After
	public void tearDown() throws Exception {
		super.tearDown();
	}
	
	@Before
	public void setUp() throws Exception {
		
	}
	
	/**
	 * Create a ASI message with a old timestamp and checks if the alarm server detects th error and publishes the alarm
	 * 
	 * @throws Exception
	 */
	@Test
	public void testAlarmsTooOld() throws Exception {
		
	}
}
