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

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import alma.acs.component.client.ComponentClientTestCase;
import alma.alarmsystem.statistics.StatHashMap;

/**
 * A class to test the generation of statistics on file
 * <P>
 * In general the tests of this class work all in the same way: 
 * a set of alarms is sent to the {@link StatsHashMap} then
 * the writing of the file is triggered by calling 
 * @link StatsHashMap#calcStatistics(int activations, int terminations)}.
 * <BR>The file is then dumped in the stdout to let tat make the comparison.
 * 
 * @author  acaproni
 * @since 2015.2
 */
public class StatsHashMapTest extends ComponentClientTestCase {
	
	/**
	 * The object to test
	 */
	private StatHashMap stats;
	
	/**
	 * The time interval is used by {@link #stats} to calculate the
	 * average number of alarms per minute.
	 */
	private final int TIMEINTERVAL=10;
	
	/**
	 * Constructor
	 * 
	 * @throws Exception
	 */
	public StatsHashMapTest() throws Exception {
		super(StatsHashMapTest.class.getName());
	}
	
	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#setUp()
	 */
	@Override
	protected void setUp() throws Exception {
		super.setUp();
		stats = new StatHashMap(this.m_logger,TIMEINTERVAL,getStatFolder());
		stats.start();
	}
	
	/**
	 * @see alma.acs.component.client.ComponentClientTestCase#tearDown()
	 */
	@Override
	protected void tearDown() throws Exception {
		stats.shutdown();
		super.tearDown();
	}
	
	/**
	 * Get the folder where the files with the statistics will be written
	 * <BR>The test uses ACS.tmp folder.
	 * 
	 * @return The folder to host file of statistics.
	 */
	private File getStatFolder() {
		// Try to create the file in $ACS_TMP
		String acstmp = System.getProperty("ACS.tmp",".");
		return  new File(acstmp);
	}
	
	/**
	 * Dump the content of the file of statistics on stdout
	 */
	private void dump() throws Exception {
		File folder = getStatFolder();
		String fName = folder.getAbsolutePath();
		if (!fName.endsWith(File.separator)) {
			fName=fName+File.separator;
		} 
		fName=fName+StatHashMap.fileNamePrefix+"0.xml";
		BufferedReader reader = new BufferedReader(new FileReader(fName));
		String line = null;
		while ((line = reader.readLine()) != null) {
			System.out.println(line);
		}
		reader.close();
	}
	
	/**
	 * Test the statistics with no alarms
	 */
	public void testEmptyStats() throws Exception {
		System.out.println("testEmptyStats...");
		// Trigger the writing of statistics
		stats.calcStatistics(0, 0);
		// The calculation and writing of stats is async: leave time to finish
		Thread.sleep(5000);
		dump();
		System.out.println("testEmptyStats DONE!");
	}
	
}
