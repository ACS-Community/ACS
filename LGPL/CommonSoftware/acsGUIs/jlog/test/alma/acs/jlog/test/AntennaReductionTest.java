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

import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.Date;
import java.text.FieldPosition;

import alma.acs.logging.table.reduction.AntennaRule;
import alma.acs.logging.table.reduction.ReductionRule;

import alma.acs.logging.engine.parser.ACSLogParser;
import alma.acs.logging.engine.parser.ACSLogParserFactory;
import alma.acs.util.IsoDateFormat;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * Test the AntennaReduction class.
 * <P>
 * The test is based on string written in the stdout
 * and checked by tat
 * 
 * @author acaproni
 *
 */
public class AntennaReductionTest {
	
	/**
	 * The object to test
	 */
	public AntennaRule reduction;
	
	public AntennaReductionTest() throws Exception {
		ILogEntry firstLog=createLog("Antenna DA41 in position");
		reduction=new AntennaRule(firstLog);
		
		if (!reduction.isReducible()) {
			System.out.println("This rule SHOULD be reducible!");
		}
	}
	
	public void checkReductions() {
		// Create a log that matches
		ILogEntry log=null;
		try {
			log = createLog("Antenna PM02 in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		reduction.applyRule(log);
		
		if (!reduction.isReducingLogs()) {
			System.out.println("This rule SHOULD have reduced logs!");
		}
		
		// Create a log that does not match
		// It has an antenna but in a wrong position
		try {
			log = createLog("Antenna in position PM02");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		reduction.applyRule(log);
		
		// Create another log that does not match
		// No antenna
		try {
			log = createLog("Antenna in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		reduction.applyRule(log);
		
		// Create another log that matches
		try {
			log = createLog("Antenna DV07 in position");
		} catch (Exception e) {
			System.err.println(e.getMessage());
			e.printStackTrace();
			System.exit(-1);
		}
		reduction.applyRule(log);
		
		System.out.println("Matching antennae: "+reduction.getReducedItems());
	}
	
	public static void main(String[] args) {
		AntennaReductionTest test=null;
		try {
			test=new AntennaReductionTest();
		} catch (Throwable t) {
			System.err.println("Error building the AntennaReductionTest ");
			t.printStackTrace();
			System.exit(-1);
		}
		
		// Start the test
		test.checkReductions();
	}
	
	private ILogEntry createLog(String logMsg) throws Exception {
		ACSLogParser parser= ACSLogParserFactory.getParser();
		
		long now = Calendar.getInstance().getTimeInMillis();
		SimpleDateFormat df = new IsoDateFormat();
		
		Date dt = new Date(now);
		StringBuffer dateSB = new StringBuffer();
		FieldPosition pos = new FieldPosition(0);
		df.format(dt,dateSB,pos);
			
		StringBuilder logStr = new StringBuilder("<Info TimeStamp=\"");
		logStr.append(dateSB.toString());
		logStr.append("\" Routine=\"CacheTest::testGet\" Host=\"this\" Process=\"test\" Thread=\"main\" Context=\"\"><![CDATA[");
		logStr.append(logMsg);
		logStr.append("]]></Info>");
		ILogEntry newLog = parser.parse(logStr.toString());
		return newLog;
	}
	
}
