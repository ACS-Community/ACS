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
import alma.alarmsystem.statistics.generated.Statistics;

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
		// Remove previously generated file of stats.
		removeOldFile();
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
	 * Remove the file of stats, if exists to ensure that each tests starts
	 * with no pre-existing file.
	 * In fact if a file of statistics already exists, new record are appended.
	 */
	private void removeOldFile() {
		String fName= buildFileName();
		File f= new File(fName);
		if (f.exists()) {
			f.delete();
		}
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
	
	private String buildFileName() {
		File folder = getStatFolder();
		String fName = folder.getAbsolutePath();
		if (!fName.endsWith(File.separator)) {
			fName=fName+File.separator;
		} 
		return fName+StatHashMap.fileNamePrefix+"0.xml";
	}
	
	/**
	 * Dump the content of the file of statistics on stdout
	 */
	private void dump() throws Exception {
		
		BufferedReader reader = new BufferedReader(new FileReader(buildFileName()));
		String line = null;
		while ((line = reader.readLine()) != null) {
			System.out.println(line);
		}
		reader.close();
		
		// Unmarshal the file to check if it is valid
		Statistics stats = Statistics.unmarshalStatistics(new FileReader(buildFileName()));
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
	
	/**
	 * This test issues a certain number of active alarms and checks the number
	 * appearing in the XML (as usual dump in the output).
	 * It also trigger the generation of a second record without setting alarms
	 * to check if all the numbers have been reset to 0.
	 * <BR>
	 * Apart of the numbers of activations and activations/terminations, the test 
	 * checks the list of the IDs of the most activated and operated alarms too.
	 * 
	 * @throws Exception
	 */
	public void testActiveAlarms() throws Exception {
		System.out.println("testActiveAlarms...");
		
		// 10 different type of alarms
		stats.processedFS("A:B:1", true);
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:4", true);
		stats.processedFS("A:B:5", true);
		stats.processedFS("A:B:6", true);
		stats.processedFS("A:B:7", true);
		stats.processedFS("A:B:8", true);
		stats.processedFS("A:B:9", true);
		stats.processedFS("A:B:10", true);
		
		// Now add more of those alarms to fill the
		// list of terminated and most operated alarms
		
		// "A:B:3" will be the most activated with 7 activations
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:3", true);
		
		// "A:B:5" and "A:B:7" in second position with 5
		stats.processedFS("A:B:5", true);
		stats.processedFS("A:B:7", true);
		stats.processedFS("A:B:5", true);
		stats.processedFS("A:B:7", true);
		stats.processedFS("A:B:5", true);
		stats.processedFS("A:B:7", true);
		stats.processedFS("A:B:5", true);
		stats.processedFS("A:B:7", true);
		
		// "A:B:2" with 4
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:2", true);
		
		// "A:B:10" with 3
		stats.processedFS("A:B:10", true);
		stats.processedFS("A:B:10", true);
		
		// "A:B:6" with 2
		stats.processedFS("A:B:6", true);
		
		stats.calcStatistics(0, 0);
		
		
		// Trigger again the writing of a record
		// to check if numbers have been reset
		stats.calcStatistics(0, 0);
		
		Thread.sleep(5000);
		dump();
		
		System.out.println("testActiveAlarms DONE!");
	}
	
	/**
	 * Same of {@link #testActiveAlarms()} for activations
	 * and terminations
	 * 
	 * @throws Exception
	 */
	public void testTerminatedAlarms() throws Exception {
		System.out.println("testTerminatedAlarms...");
		
		// 10 different type of alarms
		stats.processedFS("A:B:1", false);
		stats.processedFS("A:B:2", false);
		stats.processedFS("A:B:3", false);
		
		// Now add more of those alarms to fill the
		// list of activated and most operated alarms
		
		
		// "A:B:2" with 5
		stats.processedFS("A:B:2", false);
		stats.processedFS("A:B:2", false);
		stats.processedFS("A:B:2", false);
		stats.processedFS("A:B:2", false);
		
		
		// "A:B:1" and "A:B:3" with 3
		stats.processedFS("A:B:1", false);
		stats.processedFS("A:B:3", false);
		stats.processedFS("A:B:1", false);
		stats.processedFS("A:B:3", false);

		stats.calcStatistics(0, 0);
		
		// Trigger again the writing of a record
		// to check if numbers have been reset
		stats.calcStatistics(0, 0);
		
		Thread.sleep(5000);
		dump();
		
		System.out.println("testTerminatedAlarms DONE!");
	}
	
	/**
	 * Same of {@link #testActiveAlarms()} for activation and termination.
	 * 
	 * @throws Exception
	 */
	public void testActivatedTerminatedAlarms() throws Exception {
		System.out.println("testActivatedTerminatedAlarms...");
		
		// 10 different type of alarms
		stats.processedFS("A:B:1", true);
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:4", true);
		stats.processedFS("A:B:5", true);
		stats.processedFS("A:B:6", true);
		stats.processedFS("A:B:7", true);
		stats.processedFS("A:B:8", true);
		stats.processedFS("A:B:9", true);
		stats.processedFS("A:B:10", true);
		
		// Now add more of those alarms to fill the
		// list of activated and most operated alarms
		
		
		// "A:B:2" with 5 is the most activated
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:2", true);
		stats.processedFS("A:B:2", true);
		
		// "A:B:1" and "A:B:3" with 3 activation follow
		stats.processedFS("A:B:1", true);
		stats.processedFS("A:B:3", true);
		stats.processedFS("A:B:1", true);
		stats.processedFS("A:B:3", true);
		
		// "A:B:5" with 4 is the most terminated
		stats.processedFS("A:B:5", false);
		stats.processedFS("A:B:5", false);
		stats.processedFS("A:B:5", false);
		stats.processedFS("A:B:5", false);
		
		// "A:B:7" and "A:B:2" with 2 activation follow
		stats.processedFS("A:B:7", false);
		stats.processedFS("A:B:2", false);
		stats.processedFS("A:B:7", false);
		stats.processedFS("A:B:2", false);
		

		stats.calcStatistics(0, 0);
		
		// Trigger again the writing of a record
		// to check if numbers have been reset
		stats.calcStatistics(0, 0);
		
		Thread.sleep(5000);
		dump();
		
		System.out.println("testActivatedTerminatedAlarms DONE!");
	}
	
	/**
	 * Test the number of Avg alarms per minute and the
	 * number of activated and terminated alarms
	 * that are provided calling {@link StatHashMap#calcStatistics(int, int)}.
	 * <BR>
	 * The purpose of this test is to check that the fields are
	 * properly filled in the XML.
	 * 
	 * @throws Exception
	 */
	public void testAvgAlarms() throws Exception {
		System.out.println("testAvgAlarms...");
		stats.calcStatistics(100245, 8911);
		// Trigger again the writing of a record
		// to check if numbers have been reset
		stats.calcStatistics(0, 0);
		
		Thread.sleep(5000);
		dump();
		System.out.println("testAvgAlarms DONE!");
	}
	
}
