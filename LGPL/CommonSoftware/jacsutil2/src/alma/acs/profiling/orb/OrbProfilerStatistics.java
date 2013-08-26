/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.profiling.orb;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import alma.acs.algorithms.DataBinner.TimeValue;
import alma.acs.logging.ClientLogManager;
import alma.acs.profiling.orb.ProfilerMessage.Type;
import alma.acs.util.IsoDateFormat;

public class OrbProfilerStatistics
{

	private final List<ProfilerMessage> messages;
	private final Logger logger;

	public OrbProfilerStatistics(List<ProfilerMessage> messages, Logger logger) {
		this.messages = messages;
		this.logger = logger;
	}
	
	/**
	 * Returns the names of the operations that were called 
	 * (from REQUEST_STARTED logs), in the order in which they first appear.
	 */
	public List<String> getOperations() {
		Set<String> ops = new LinkedHashSet<String>();
		
		for (ProfilerMessage msg : messages) {
			if (msg.type == Type.REQUEST_STARTED) {
				ops.add(msg.operation);
			}
		}
		return new ArrayList<String>(ops);
	}
	
	
	public List<TimeValue<Integer>> getFinishedRequests(String operation) {
		List<TimeValue<Integer>> ret = new ArrayList<TimeValue<Integer>>();
		
		for (ProfilerMessage msg : messages) {
			if (msg.type == Type.REQUEST_FINISHED
				&& (operation == null || operation.equals(msg.operation))) {
					TimeValue<Integer> tv = new TimeValue<Integer>(msg.timestamp, (int)msg.timeElapsedMillis);
					ret.add(tv);
			}
		}
		return ret;
	}
	
	
	public List<TimeValue<Integer>> getConcurrentCalls() {
		int startedCount = 0;
		TimeValue<Integer> last = null;
		List<TimeValue<Integer>> ret = new ArrayList<TimeValue<Integer>>();
		
		for (ProfilerMessage msg : messages) {
			if (msg.type == ProfilerMessage.Type.REQUEST_STARTED ||
				msg.type == ProfilerMessage.Type.REQUEST_FINISHED) {
				
				if (msg.type == ProfilerMessage.Type.REQUEST_STARTED) {
					startedCount++;
				}
				else {
					startedCount--;
					if (startedCount < 0) {
						logger.warning("Bad input data at '" + IsoDateFormat.formatDate(new Date(msg.timestamp)) + "': more calls finished than started.");
					}
				}
				// replot the last value, but with current time, to get a straight line in the diagram for times without any calls.
				if (last != null) {
					ret.add(new TimeValue<Integer>(msg.timestamp, last.value));
				}
				// add current data
				TimeValue<Integer> current = new TimeValue<Integer>(msg.timestamp, startedCount);
				ret.add(current); 
				
				last = current;
			}
		}
		return ret;
	}
	
	public void printConcurrentCalls(PrintStream out) {
		List<TimeValue<Integer>> concCalls = getConcurrentCalls();
		int maxConcurrentCalls = 0;
		for (TimeValue<Integer> timeValue : concCalls) {
			if (timeValue.value > maxConcurrentCalls) {
				maxConcurrentCalls = timeValue.value; 
			}
			out.println(
					IsoDateFormat.formatDate(new Date(timeValue.timeMillis)) +
					" " + timeValue.value );
		}
		out.close();
		logger.info("Maximum number of concurrent calls: " + maxConcurrentCalls);
	}
	
	public static long floor(long value, long multipleOf) {
		return (value / multipleOf) * multipleOf;
	}
//	public long ceiling(long value, long multipleOf) {
//		if (value % multipleOf == 0) {
//			return value;
//		}
//		return ((value / multipleOf) + 1) * multipleOf;
//	}
	
	
	public static void main(String[] args) {
		try {
			String inFileName = "hibernateCdbJDal-2011-09-12T153856.txt";
			Logger logger = ClientLogManager.getAcsLogManager().getLoggerForApplication("Statistics", false);
			OrbProfilerParser p = new OrbProfilerParser(logger);
			List<ProfilerMessage> messages = p.parse(new File(inFileName));
			OrbProfilerStatistics stat = new OrbProfilerStatistics(messages, logger);
			
			// Concurrent calls
			stat.printConcurrentCalls(new PrintStream(new File(inFileName + "_concurrent")));
			
			logger.info("Done.");
		} catch (Exception ex) {
			ex.printStackTrace();
		}
	}

}
