/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2015 
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

package alma.acs.lasercore.test;

import com.cosylab.acs.laser.AlarmSourcesListenerCached;

import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.statistics.StatsCalculator;

/**
 * A class to test the generation of statistics
 * <P>
 * The generation of statistics is performed by instantiating a
 * {@link StatsCalculator} object and invoking its method.
 * The object if working correctly will publish statistics
 * with a log that tat checks automatically.
 * <P>
 * The statistics are generated once per minute and using the Notice log level
 * (both set as java property in the script starting the test).
 * 
 * @author  acaproni
 * @since 2015.2
 */
public class StatisticsTest extends ComponentClientTestCase {
	
	/**
	 * The object to test
	 */
	private StatsCalculator stats;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public StatisticsTest() throws Exception {
		super(StatisticsTest.class.getName());
	}
	
	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		// Instantiate the object to test
		AlarmSourcesListenerCached queue = new AlarmSourcesListenerCached(getContainerServices(), getContainerServices().getLogger()); 
		assertNotNull(queue);
		stats = new StatsCalculator(this.getContainerServices().getLogger(), queue);
		assertNotNull(stats);
		// Enable the generation of statistics
		stats.start();
	}
	
	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		// Terminates the generation of statistics
		stats.shutdown();
		super.tearDown();
	}
	
	/**
	 * Call methods of {@link StatsCalculator} to trigger meaningful numbers
	 * in the logs produced once per minute.
	 * 
	 * @throws Exception
	 */
	public void testStatistics() throws Exception {
		System.out.println("testStatistics started");
		// Call few methods
		stats.processedFS("A:B:1", true);
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:C:1", true);
		stats.processedFS("B:B:1", false);
		stats.processedFS("B:B:2", false);
		stats.msgFromSourceNCReceived();
		stats.msgFromSourceNCReceived();
		stats.msgFromSourceNCReceived();
		// Wait one minute to leave time to log the figures
		try {
			Thread.sleep(70*1000);
		} catch (Throwable t) {}
		// Wait one more minute to see if in the next logged message the statistics have been reset
		try {
			Thread.sleep(70*1000);
		} catch (Throwable t) {}
		System.out.println("testStatistics done");
		
	}
	
}
