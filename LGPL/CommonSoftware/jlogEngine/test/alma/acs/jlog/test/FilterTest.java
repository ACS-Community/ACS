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

import java.util.Date;

import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.LogEntryTest;
import com.cosylab.logging.engine.ExactFilter;
import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.InvalidFilterConstraintException;
import com.cosylab.logging.engine.MinMaxFilter;
import com.cosylab.logging.engine.RegExpFilter;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;

import junit.framework.TestCase;
/** 
 * @author  acaproni
 * @version $Id: FilterTest.java,v 1.3 2013/02/26 17:04:13 acaproni Exp $
 * @since   ACS 11.2
 */

/**
 * <code>FilterTest</code> tests {@link Filter}
 * 
 */
public class FilterTest extends TestCase {
	
	/**
	 * ISO date formatter
	 */
	private final IsoDateFormat df = new IsoDateFormat();
	
	/**
	 * Logs for testing filters
	 */
	private LogEntry log1,log2,log3; 
	
	/**
	 * Lethal passed in Filter constructor and in matching must be the same
	 * to correctly test filter behaviors
	 */
	private final boolean lethal=true;
	
	@Override
	protected void setUp() throws Exception {
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
				"A message for a log", 
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
				12, 
				"A message for a log", 
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
	 * Check if the filter works for exact value of log type.
	 */
	public void testExactLogType() throws Exception {
		// Test the LogMessgae
		String logMessageToTest = "A message for a log";
		Filter exactLogMessageFilter= new ExactFilter(LogField.LOGMESSAGE, lethal, logMessageToTest, false);
		assertTrue("Exact filetrs should have accepted this log msg",exactLogMessageFilter.applyTo(log1, lethal));
		String anotherLogMessage="Another message";
		Filter noExactLogMessageFilter= new ExactFilter(LogField.LOGMESSAGE, lethal, anotherLogMessage, false);
		assertFalse("Exact filetrs should have discarded this log msg",noExactLogMessageFilter.applyTo(log1, lethal));
		// Test the file name
		String fileName = "File.java";
		Filter exactfileNameFilter= new ExactFilter(LogField.FILE, lethal, fileName, false);
		assertTrue("Exact filetrs should have accepted this file name",exactfileNameFilter.applyTo(log1, lethal));
		String notFileName = "FileFile.java";
		Filter noExactfileNameFilter= new ExactFilter(LogField.FILE, lethal, notFileName, false);
		assertFalse("Exact filetrs should have discarded this file name",noExactfileNameFilter.applyTo(log1, lethal));
		// Test the stack level
		Integer stackLvl = Integer.valueOf(12);
		Filter stackLvlFilter= new ExactFilter(LogField.STACKLEVEL, lethal, stackLvl, false);
		assertTrue("Exact filetrs should have accepted this stack level",stackLvlFilter.applyTo(log1, lethal));
		Integer noStackLvl = Integer.valueOf(10);
		Filter nostackLvlFilter= new ExactFilter(LogField.STACKLEVEL, lethal, noStackLvl, false);
		assertFalse("Exact filetrs should have discarded this stack level",nostackLvlFilter.applyTo(log1, lethal));
		// Test the timestamp
		Date date = df.parseIsoTimestamp("2013-08-04T15:10:10.512");
		Filter dateFilter= new ExactFilter(LogField.TIMESTAMP, lethal, date, false);
		assertTrue("Exact filetrs should have accepted this timestamp",dateFilter.applyTo(log1, lethal));
		Date noDate = df.parseIsoTimestamp("2013-08-04T16:10:10.888");
		Filter noDateFilter= new ExactFilter(LogField.TIMESTAMP, lethal, noDate, false);
		assertFalse("Exact filetrs should have discarded this timestamp",noDateFilter.applyTo(log1, lethal));
		// Test the LogEntryType
		Filter logTypeFiletr= new ExactFilter(LogField.ENTRYTYPE, lethal, LogTypeHelper.INFO, false);
		assertTrue("This log type filter should have accepted this type",logTypeFiletr.applyTo(log1, lethal));
		assertFalse("This log type filter should have discarded this type",logTypeFiletr.applyTo(log2, lethal));
	}
	
	/**
	 * Check if the filter works for exact value of log type.
	 */
	public void testRegexpFilterType() throws Exception {
		// Test the LogMessgae against regexp
		String regExpToTest = "..message.for.*";
		Filter regExpLogMessageFilter= new RegExpFilter(LogField.LOGMESSAGE, lethal, regExpToTest, false);
		assertTrue("Regexp filter should have accepted this log msg",regExpLogMessageFilter.applyTo(log1, lethal));
		String noRegExpToTest="Another message";
		Filter noRegExpLogMessageFilter= new RegExpFilter(LogField.LOGMESSAGE, lethal, noRegExpToTest, false);
		assertFalse("Regexp filter should have discarded this log msg",noRegExpLogMessageFilter.applyTo(log1, lethal));
		// Test source object
		String regSrcExpToTest = "source.*";
		Filter regExpSrcFilter= new RegExpFilter(LogField.SOURCEOBJECT, lethal, regSrcExpToTest, false);
		assertTrue("Regexp filter should have accepted this source object",regExpSrcFilter.applyTo(log1, lethal));
		String noRegSrcExpToTest="Another message";
		Filter noRegExpSrcFilter= new RegExpFilter(LogField.SOURCEOBJECT, lethal, noRegSrcExpToTest, false);
		assertFalse("Regexp filter should have discarded this source object",noRegExpSrcFilter.applyTo(log1, lethal));
	}
	
	/**
	 * Check if the filter works for min value 
	 */
	public void testMinFilterType() throws Exception {
		// Check the line
		Integer minLine= Integer.valueOf(99);
		Filter minFilter= new MinMaxFilter(LogField.LINE,lethal,minLine,null,false);
		assertTrue("Min should have accepted this line value",minFilter.applyTo(log1, lethal));
		Integer noMinLine= Integer.valueOf(101);
		Filter noMinFilter= new MinMaxFilter(LogField.LINE,lethal,noMinLine,null,false);
		assertFalse("Min should have rejected this line value",noMinFilter.applyTo(log1, lethal));
		// Check the time
		Date date = df.parseIsoTimestamp("2013-08-01T15:10:10.512");
		Filter minDateFilter= new MinMaxFilter(LogField.TIMESTAMP,lethal,date,null,false);
		assertTrue("Min should have accepted this timestamp",minDateFilter.applyTo(log1, lethal));
		Date noDate = df.parseIsoTimestamp("2013-08-15T15:10:10.512");
		Filter noMinDateFilter= new MinMaxFilter(LogField.TIMESTAMP,lethal,noDate,null,false);
		assertFalse("Min should have accepted this timestamp",noMinDateFilter.applyTo(log1, lethal));
		// Log type
		LogTypeHelper minType = LogTypeHelper.DEBUG;
		Filter minTypeFilter= new MinMaxFilter(LogField.ENTRYTYPE,lethal,minType,null,false);
		assertTrue("Min should have accepted this log type",minTypeFilter.applyTo(log1, lethal));
		LogTypeHelper noMinType = LogTypeHelper.WARNING;
		Filter noMinTypeFilter= new MinMaxFilter(LogField.ENTRYTYPE,lethal,noMinType,null,false);
		assertFalse("Min should have accepted this log type",noMinTypeFilter.applyTo(log1, lethal));
	}
	
	/**
	 * Check if the filter works for max value 
	 */
	public void testMaxFilterType() throws Exception {
		// Check the line
		Integer maxLine= Integer.valueOf(120);
		Filter maxFilter= new MinMaxFilter(LogField.LINE,lethal,null,maxLine,false);
		assertTrue("Max should have accepted this line value",maxFilter.applyTo(log1, lethal));
		Integer noMaxLine= Integer.valueOf(98);
		Filter noMaxFilter= new MinMaxFilter(LogField.LINE,lethal,null,noMaxLine,false);
		assertFalse("Min should have rejected this line value",noMaxFilter.applyTo(log1, lethal));
		// Check the time
		Date date = df.parseIsoTimestamp("2013-09-01T15:10:10.512");
		Filter maxDateFilter= new MinMaxFilter(LogField.TIMESTAMP,lethal,null,date,false);
		assertTrue("Max should have accepted this timestamp",maxDateFilter.applyTo(log1, lethal));
		Date noDate = df.parseIsoTimestamp("2013-07-01T15:10:10.512");
		Filter noMaxDateFilter= new MinMaxFilter(LogField.TIMESTAMP,lethal,null,noDate,false);
		assertFalse("Max should have accepted this timestamp",noMaxDateFilter.applyTo(log1, lethal));
		// Log type
		LogTypeHelper maxType = LogTypeHelper.CRITICAL;
		Filter maxTypeFilter= new MinMaxFilter(LogField.ENTRYTYPE,lethal,null,maxType,false);
		assertTrue("Min should have accepted this log type",maxTypeFilter.applyTo(log1, lethal));
		LogTypeHelper noMaxType = LogTypeHelper.TRACE;
		Filter noMaxTypeFilter= new MinMaxFilter(LogField.ENTRYTYPE,lethal,null,noMaxType,false);
		assertFalse("Min should have accepted this log type",noMaxTypeFilter.applyTo(log1, lethal));
	}
	
	/**
	 * Check if the filter works for MinMax value 
	 */
	public void testMinMaxFilterType() throws Exception {
		// Check the line
		Integer maxLine= Integer.valueOf(120);
		Integer minLine= Integer.valueOf(98);
		Filter minMaxFilter= new MinMaxFilter(LogField.LINE,lethal,minLine,maxLine,false);
		assertTrue("MinMax should have accepted this line value",minMaxFilter.applyTo(log1, lethal));
		assertFalse("MinMax should have rejected this line value",minMaxFilter.applyTo(log2, lethal));
		assertFalse("MinMax should have rejected this line value",minMaxFilter.applyTo(log3, lethal));
		
		// Check the time
		Date maxDate = df.parseIsoTimestamp("2013-08-07T15:10:10.512");
		Date minDate = df.parseIsoTimestamp("2013-08-02T15:10:10.512");
		Filter minMaxDateFilter= new MinMaxFilter(LogField.TIMESTAMP,lethal,minDate,maxDate,false);
		assertTrue("MinMax should have accepted this timestamp",minMaxDateFilter.applyTo(log1, lethal));
		assertFalse("MinMax should have rejected this timestamp",minMaxDateFilter.applyTo(log2, lethal));
		assertFalse("MinMax should have rejected this timestamp",minMaxDateFilter.applyTo(log3, lethal));
		// Log type
		LogTypeHelper maxType = LogTypeHelper.NOTICE;
		LogTypeHelper minType = LogTypeHelper.DEBUG;
		Filter minMaxTypeFilter= new MinMaxFilter(LogField.ENTRYTYPE,lethal,minType,maxType,false);
		assertTrue("MinMax should have accepted this log type",minMaxTypeFilter.applyTo(log1, lethal));
		assertFalse("MinMax should have rejected this log type",minMaxTypeFilter.applyTo(log2, lethal));
		assertFalse("MinMax should have rejected this log type",minMaxTypeFilter.applyTo(log3, lethal));
	}
	
	/**
	 * Building a filter with a min>=max must return a error.
	 * This tests builds a filter passing a min value greater then a max value and catching the exception.
	 * 
	 * @throws Exception
	 */
	public void testWrongMinMaxValuesInFilters() throws Exception {
		Integer maxLine= Integer.valueOf(120);
		Integer minLine= Integer.valueOf(98);
		try {
			Filter minMaxFilter= new MinMaxFilter(LogField.LINE,lethal,maxLine,minLine,false);
			System.out.println("Error building a filter with min="+maxLine+"max="+minLine);
		} catch (InvalidFilterConstraintException ivc) {
			// This exception means that the test passed!
			System.out.println("Test passed: error received while building a filter with wrong values");
		}
	}
	
}
