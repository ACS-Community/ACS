/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2009, All rights reserved
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
package alma.acs.jlog.test;

import java.text.FieldPosition;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;

import com.cosylab.logging.engine.log.ILogEntry;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.logging.table.reduction.AntennaRule;
import alma.acs.logging.table.reduction.SourceAntennaRule;
import alma.acs.util.IsoDateFormat;

/**
 * The class for testing {@link AntennaRule}.
 * <P>
 * The test is based on string written in the stdout
 * and checked by tat.
 * 
 * @author acaproni
 *
 */
public class AntennaSourceReductionTest {
	
	/**
	 * Check {@link AntennaRule#isReducible()}
	 * @throws Exception
	 */
	public void testIsReducible() throws Exception {
		String msg="In position";
		
		// Reducible
		ILogEntry log1=createLog(msg, "CONTROL/DA41/WVR");
		SourceAntennaRule sar = new SourceAntennaRule(log1);
		if (!sar.isReducible()) {
			System.out.println("Error: log1 should be reducible!!!");
		}
		
		// NOT reducible!
		ILogEntry log2=createLog(msg, "maci");
		sar = new SourceAntennaRule(log2);
		if (sar.isReducible()) {
			System.out.println("Error: log2 should not be reducible!!!");
		}
		
		// NOT reducible!
		ILogEntry log3=createLog(msg, "");
		sar = new SourceAntennaRule(log3);
		if (sar.isReducible()) {
			System.out.println("Error: log3 should not be reducible!!!");
		}
		
		// Reducible!
		ILogEntry log4=createLog(msg, "PM02");
		sar = new SourceAntennaRule(log4);
		if (!sar.isReducible()) {
			System.out.println("Error: log4 should be reducible!!!");
		}
	}
	
	/**
	 * Test if the reduction works submitting several logs
	 * 
	 * @throws Exception
	 */
	public void testRedction() throws Exception {
		String msg="In position";
		// Reducible
		ILogEntry log1=createLog(msg, "CONTROL/DA41/WVR");
		SourceAntennaRule sar = new SourceAntennaRule(log1);
		
		ILogEntry log2=createLog(msg, "CONTROL/DV13/WVR");
		sar.applyRule(log2);
		
		if (!sar.isReducingLogs()) {
			System.out.println("isReducing should return TRUE at this point!");
		}
		ILogEntry log3=createLog(msg, "CONTROL/PM02/WVR");
		sar.applyRule(log3);
		
		ILogEntry log4=createLog(msg, "CONTROL/CM04/WVR");
		sar.applyRule(log4);
		
		// This log is not reduced even if the source contains an antenna name
		ILogEntry log5=createLog(msg, "CONTROL/CM03/ACD");
		sar.applyRule(log5);
		
		// This log is not reduced because the message is different
		ILogEntry log6=createLog("Different log message", "CONTROL/CM03/WVR");
		sar.applyRule(log6);
		
		ILogEntry log7=createLog(msg, "CONTROL/DA44/WVR");
		sar.applyRule(log7);
		
		
		System.out.println("Reduced log: "+sar.getReducedLog().toString());
	}

	/**
	 * Create a log entry with the passed message and source
	 * 
	 * @param logMsg The message
	 * @param logSrc The source
	 * @return The log entry
	 * @throws Exception
	 */
	private ILogEntry createLog(String logMsg, String logSrc) throws Exception {
		ACSLogParser parser= ACSLogParserFactory.getParser();
		
		long now = Calendar.getInstance().getTimeInMillis();
		SimpleDateFormat df = new IsoDateFormat();
		
		Date dt = new Date(now);
		StringBuffer dateSB = new StringBuffer();
		FieldPosition pos = new FieldPosition(0);
		df.format(dt,dateSB,pos);
			
		StringBuilder logStr = new StringBuilder("<Info TimeStamp=\"");
		logStr.append(dateSB.toString());
		logStr.append("\" Routine=\"CacheTest::testGet\" SourceObject=\"");
		logStr.append(logSrc);
		logStr.append("\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[");
		logStr.append(logMsg);
		logStr.append("]]></Info>");
		ILogEntry newLog = parser.parse(logStr.toString());
		return newLog;
	}
	
	public static void main(String[] args) {
		AntennaSourceReductionTest test=null;
		test=new AntennaSourceReductionTest();
		
		// Start the test
		try {
			test.testIsReducible();
			test.testRedction();
		} catch (Throwable t) {
			System.out.println("Error executing test!");
			t.printStackTrace();
		}
	}
}
