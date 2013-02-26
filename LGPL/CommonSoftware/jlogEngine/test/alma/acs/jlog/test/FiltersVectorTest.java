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
package alma.acs.jlog.test;

import java.io.File;
import java.util.Date;

import junit.framework.TestCase;

import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.engine.ExactFilter;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.MinMaxFilter;
import com.cosylab.logging.engine.RegExpFilter;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * Test the {@link FiltersVector} class.
 * @author  acaproni
 * @version $Id: FiltersVectorTest.java,v 1.2 2013/02/26 17:02:53 acaproni Exp $
 * @since ACS 11.2
 */
public class FiltersVectorTest extends TestCase {
	
	/**
	 * lethal is false for testing
	 */
	private final boolean lethal=false;
	
	/**
	 * ISO date formatter
	 */
	private final IsoDateFormat df = new IsoDateFormat();
	
	/**
	 * The {@link FiltersVector} to test
	 */
	private FiltersVector filters;
	
	/**
	 * Logs for testing the vector filters 
	 */
	private LogEntry log1,log2,log3; 
	
	@Override
	protected void setUp() throws Exception {
		
		filters= new FiltersVector();
		assertNotNull(filters);
		
		log1 = new LogEntry(
				df.parseIsoTimestamp("2013-08-04T15:10:10.512").getTime(), 
				LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.INFO).ordinal(), 
				"File.java", 
				100, 
				"File#routine()", 
				"alma.hq.eso.org", 
				"javaProcess", 
				"context", 
				"java thread", 
				"log id", 
				5, 
				"URI", 
				"stack id", 
				12, 
				"A message for a log", 
				"source object", 
				"Audience", 
				"array name", 
				"antenna name", 
				null); 
		
		log2 = new LogEntry(
				df.parseIsoTimestamp("2013-08-01T15:10:10.512").getTime(), 
				LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.TRACE).ordinal(), 
				"File.java", 
				95, 
				"File#routine()", 
				"alma.hq.eso.org", 
				"javaProcess", 
				"context", 
				"java thread", 
				"log id", 
				5, 
				"URI", 
				"stack id", 
				12, 
				"A message for log2", 
				"source object", 
				"Audience", 
				"array name", 
				"antenna name", 
				null); 
		
		log3 = new LogEntry(
				df.parseIsoTimestamp("2013-08-10T15:10:10.512").getTime(), 
				LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.WARNING).ordinal(), 
				"File.java", 
				130, 
				"File#routine()", 
				"alma.hq.eso.org", 
				"javaProcess", 
				"context", 
				"java thread", 
				"log id", 
				5, 
				"URI", 
				"stack id", 
				15, 
				"A message for log2", 
				"source object", 
				"Audience", 
				"array name", 
				"antenna name", 
				null); 
		
		super.setUp();
	}
	
	@Override
	protected void tearDown() throws Exception {
		super.tearDown();
	}
	
	/**
	 * Test {@link FiltersVector#hasActiveFilters()}
	 */
	public void testHasActiveFilters() throws Exception {
		//No active filters yet
		assertFalse("The vector should be empty at startup",filters.hasActiveFilters());
		Filter exactLogMessageFilter= new ExactFilter(LogField.LOGMESSAGE, lethal, "A message", false);
		filters.addFilter(exactLogMessageFilter, false);
		assertFalse("The vector should contain one filter but not active",filters.hasActiveFilters());
		Filter stackLvlFilter= new ExactFilter(LogField.STACKLEVEL, lethal, Integer.valueOf(5), false);
		filters.addFilter(stackLvlFilter, true);
		assertTrue("The vector should contain one active filter",filters.hasActiveFilters());
		filters.clear();
		assertFalse("The empty vector has no active filters",filters.hasActiveFilters());
	}
	
	/**
	 * Test {@link FiltersVector#applyFilters(com.cosylab.logging.engine.log.ILogEntry)}
	 */
	public void testApplyFilters() throws Exception {
		// All logs are accepted if the vector is empty
		assertTrue("All logs accepted if no filters", filters.applyFilters(log1));
		assertTrue("All logs accepted if no filters", filters.applyFilters(log2));
		assertTrue("All logs accepted if no filters", filters.applyFilters(log3));
		// All logs are accepted if the vector has filters but no one is active
		Filter exactLogMessageFilter= new ExactFilter(LogField.LOGMESSAGE, lethal, "A message for a log", false);
		filters.addFilter(exactLogMessageFilter, false);
		Filter stackLvlFilter= new ExactFilter(LogField.STACKLEVEL, lethal, Integer.valueOf(12), false);
		filters.addFilter(stackLvlFilter, false);
		assertTrue("All logs accepted if no filters", filters.applyFilters(log1));
		assertTrue("All logs accepted if no filters", filters.applyFilters(log2));
		assertTrue("All logs accepted if no filters", filters.applyFilters(log3));
		filters.clear();
		assertFalse("The empty vector has no active filters",filters.hasActiveFilters());
		// Only log1 should pass
		filters.addFilter(exactLogMessageFilter, true);
		assertTrue("The vector should contain one active filter",filters.hasActiveFilters());
		assertTrue("log1 should be accepted", filters.applyFilters(log1));
		assertFalse("log2 logs NOT accepted", filters.applyFilters(log2));
		assertFalse("log3 logs NOT accepted", filters.applyFilters(log3));
		filters.clear();
		assertFalse("The empty vector has no active filters",filters.hasActiveFilters());
		// Test with 2 filters
		Filter exactLogMessageFilter2= new ExactFilter(LogField.LOGMESSAGE, lethal, "A message for log2", false);
		Filter lineFilter= new ExactFilter(LogField.LINE, lethal, Integer.valueOf(95), false);
		filters.addFilter(exactLogMessageFilter2, true);
		filters.addFilter(lineFilter, true);
		assertFalse("log1 should NOT be accepted", filters.applyFilters(log1));
		assertTrue("log2 logs accepted", filters.applyFilters(log2));
		assertFalse("log3 logs NOT accepted", filters.applyFilters(log3));
	}
	
	/**
	 * Test the saving and loading of the filters.
	 * <P>The test is done by:
	 * <UL>
	 * 	<LI>Setting up some filters
	 *  <LI>Saving the filters in a temporary file
	 *  <LI>Clearing the vector of filters
	 *  <LI>Loading the filters
	 *  <LI>Checking if the loaded filters are the same filters that had been saved
	 * </UL>
	 * The checking of filters is done also printing {@link FiltersVector#getFilterString()} in the stdout:
	 * tat will check for correctness. Besides that, each filter will be checked 
	 * @throws Exception
	 */
	public void testLoadSave() throws Exception {
		// Define few interesting filters
		Filter exactLogMessageFilter= new ExactFilter(LogField.LOGMESSAGE, lethal, "One message", false);
		Filter regExpLogMessageFilter= new RegExpFilter(LogField.SOURCEOBJECT, lethal, "RegExpr.*", false);
		Date maxDate = df.parseIsoTimestamp("2013-08-07T15:10:10.512");
		Date minDate = df.parseIsoTimestamp("2013-08-02T15:10:10.512");
		Filter minMaxDateFilter= new MinMaxFilter(LogField.TIMESTAMP,lethal,minDate,maxDate,false);
		Integer maxType = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.NOTICE).ordinal();
		Integer minType = LogTypeHelper.fromAcsCoreLevel(AcsLogLevelDefinition.DEBUG).ordinal();
		Filter minMaxTypeFilter= new MinMaxFilter(LogField.ENTRYTYPE,lethal,minType,maxType,false);
		Date excatDate = df.parseIsoTimestamp("2011-09-02T10:21:18.333");
		Filter exactDateFilter= new ExactFilter(LogField.TIMESTAMP, lethal, excatDate, false);
		Filter exactStackLevelFilter= new ExactFilter(LogField.STACKLEVEL, lethal, Integer.valueOf(13), false);
		Filter minMaxLineFilter= new MinMaxFilter(LogField.LINE,lethal,Integer.valueOf(7),Integer.valueOf(320),false);
		Filter exactTypeFilter= new ExactFilter(LogField.ENTRYTYPE, lethal, LogTypeHelper.EMERGENCY, false);
		filters.addFilter(exactLogMessageFilter, true);
		filters.addFilter(regExpLogMessageFilter, false);
		filters.addFilter(minMaxDateFilter, true);
		filters.addFilter(minMaxTypeFilter, false);
		filters.addFilter(exactDateFilter,true);
		filters.addFilter(exactStackLevelFilter,true);
		filters.addFilter(minMaxLineFilter, false);
		filters.addFilter(exactTypeFilter, false);
		assertEquals("We added 8 filters!",8, filters.size());
		System.out.println("Filters to save: "+filters.getFilterString());
		// Save filters
		String dir = System.getProperty("user.dir");
		String fileName=dir+"/TestSaveLoad.xml";
		File outF = new File(fileName); 
		filters.saveFilters(outF);
		// Clear the vector of filters
		filters.clear();
		// Load filters
		filters.loadFilters(outF, true, fileName);
		System.out.println("Loaded filters: "+filters.getFilterString());
		// Check each filter
		assertEquals("Loaded filters differ from saved filters",8, filters.size());
		for (int t=0; t<filters.size(); t++) {
			Filter flt = filters.get(t);
			System.out.println(flt.toString());
		}
		// Check if the activation state has been saved correctly
		assertTrue("Invalid \"ENABLED\" state",filters.isActive(0));
		assertFalse("Invalid \"ENABLED\" state",filters.isActive(1));
		assertTrue("Invalid \"ENABLED\" state",filters.isActive(2));
		assertFalse("Invalid \"ENABLED\" state",filters.isActive(3));
		assertTrue("Invalid \"ENABLED\" state",filters.isActive(4));
		assertTrue("Invalid \"ENABLED\" state",filters.isActive(5));
		assertFalse("Invalid \"ENABLED\" state",filters.isActive(6));
		assertFalse("Invalid \"ENABLED\" state",filters.isActive(7));
		// Remove the file
		//outF.delete();
	}
}