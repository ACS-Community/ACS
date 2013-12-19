/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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

package alma.acs.util;

import static org.hamcrest.Matchers.closeTo;
import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.lessThan;
import static org.hamcrest.Matchers.startsWith;
import static org.junit.Assert.assertThat;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.junit.Test;

import alma.acs.testsupport.LogRecordCollectingLogger;
import alma.acs.testsupport.TestLogger;


/**
 * @author hsommer
 */
public class StopWatchTest
{
	private final long sleepMillis = 100;
	
	/**
	 * Takes account of the system-dependent time granularity
	 * when testing times. May have to be raised when running this test in VMs.
	 * <p> 
	 * In addition to the granularity, this value also includes the very small times
	 * it takes for the StopWatch and StopWatchTest code itself to execute.
	 */
	private final long timeGranularityMillis = 15L;

	@Test
	public void testNoSubtaskNoLogging() throws Exception {
		
		// (1) Do not pass a logger. Do not call stop.
		StopWatch sw1 = new StopWatch();
		Thread.sleep(sleepMillis);
		long timeNanos = sw1.getLapTimeNanos();
		long timeMillis = sw1.getLapTimeMillis();
		assertThat("10 ms timing accuracy expected.", Math.abs(timeMillis - sleepMillis), lessThan(timeGranularityMillis) );
		assertThat("Compatible ms and ns values expected.", Math.abs(timeMillis*1000000 - timeNanos ), lessThan(timeGranularityMillis * 1000000L) );

		// (2) Do not pass a logger. Do call stop.
		StopWatch sw2 = new StopWatch();
		Thread.sleep(sleepMillis);
		sw2.stop();
		timeNanos = sw2.getLapTimeNanos();
		timeMillis = sw2.getLapTimeMillis();
		assertThat("10 ms timing accuracy expected.", Math.abs(timeMillis - sleepMillis), lessThan(timeGranularityMillis) );
		assertThat("Compatible ms and ns values expected.", TimeUnit.NANOSECONDS.toMillis(timeNanos), equalTo(timeMillis));
		
		// (3) Pass a dummy logger. Do call stop().
		Logger unusedLogger = TestLogger.getLogger("dummy");
		StopWatch sw3 = new StopWatch(unusedLogger);
		Thread.sleep(sleepMillis);
		sw3.stop();
		timeNanos = sw3.getLapTimeNanos();
		timeMillis = sw3.getLapTimeMillis();
		assertThat("10 ms timing accuracy expected.", Math.abs(timeMillis - sleepMillis), lessThan(timeGranularityMillis) );
		assertThat("Compatible ms and ns values expected.", TimeUnit.NANOSECONDS.toMillis(timeNanos), equalTo(timeMillis));

		// (4) Reuse / Reset a stopwatch
		sw3.reset();
		Thread.sleep(sleepMillis);
		sw3.stop();
		timeMillis = sw3.getLapTimeMillis();
		assertThat("10 ms timing accuracy expected.", Math.abs(timeMillis - sleepMillis), lessThan(timeGranularityMillis) );
	}

	
	@Test
	public void testNoSubtaskWithLogging() throws Exception {
		
		LogRecordCollectingLogger logger1 = LogRecordCollectingLogger.getCollectingLogger("testNoSubtaskWithLogging");
		StopWatch sw1 = new StopWatch(logger1);
		Thread.sleep(sleepMillis);
		sw1.logLapTime("sleep 100 ms");
		LogRecord[] logRecords = logger1.getCollectedLogRecords();
		assertThat(logRecords.length, equalTo(1));
		assertThat(logRecords[0].getLevel(), equalTo(Level.FINE));
		String msg = logRecords[0].getMessage();
		assertThat(msg, startsWith("elapsed time in ms to sleep 100 ms: "));
		String loggedTimeString = msg.split(":")[1].trim();
		double timeMillis = Double.parseDouble(loggedTimeString);
		assertThat(timeMillis, closeTo(sleepMillis, timeGranularityMillis) );
	}

	@Test
	public void testWithSubtaskWithLogging() throws Exception {
		LogRecordCollectingLogger logger = LogRecordCollectingLogger.getCollectingLogger("testWithSubtaskWithLogging");
		
		StopWatch sw_top = new StopWatch(logger);
		Thread.sleep(sleepMillis);
		
		StopWatch sw_sub_1 = sw_top.createStopWatchForSubtask("subtask-1");
		Thread.sleep(25);
		sw_sub_1.stop();
		
		StopWatch sw_sub_2 = sw_top.createStopWatchForSubtask("subtask-2");
		StopWatch sw_sub_2_1 = sw_sub_2.createStopWatchForSubtask("subtask-2-1");
		Thread.sleep(40);
		sw_sub_2_1.stop();
		StopWatch sw_sub_2_2 = sw_sub_2.createStopWatchForSubtask("subtask-2-2");
		Thread.sleep(35);
		sw_sub_2_2.stop();
		sw_sub_2.stop();

		StopWatch sw_sub_3 = sw_top.createStopWatchForSubtask("subtask-3");
		Thread.sleep(15);
		sw_sub_3.stop();

		sw_top.logLapTimeWithSubtaskDetails("sleep and perform subtasks", Level.INFO);
		LogRecord[] logRecords = logger.getCollectedLogRecords();
		assertThat(logRecords.length, equalTo(1));
		assertThat(logRecords[0].getLevel(), equalTo(Level.INFO));
		String msg = logRecords[0].getMessage();
		Map<String, Double> taskTimes = parseStopWatchTimes(msg);
		assertThat(taskTimes.get("total"), closeTo(215, timeGranularityMillis));
		assertThat(taskTimes.get("subtask-1"), closeTo(25, timeGranularityMillis));
		assertThat(taskTimes.get("subtask-2"), closeTo(75, timeGranularityMillis));
		assertThat(taskTimes.get("subtask-2-1"), closeTo(40, timeGranularityMillis));
		assertThat(taskTimes.get("subtask-2-2"), closeTo(35, timeGranularityMillis));
		assertThat(taskTimes.get("subtask-3"), closeTo(15, timeGranularityMillis));
		
		
		// Use a null logger (stdout). Currently we only verify there is no exception. Could inject stdout handler...
		sw_top = new StopWatch();
		Thread.sleep(sleepMillis);
		sw_sub_1 = sw_top.createStopWatchForSubtask("subtask-1");
		Thread.sleep(25);
		sw_top.logLapTimeWithSubtaskDetails("sleep and perform subtasks", Level.INFO);
	}

	/**
	 * Helper method that parses a subtask log string such as
	 * "elapsed time in ms to sleep and perform subtasks: 216.2 { subtask-1=25.3, subtask-2=75.2 { subtask-2-1=40, subtask-2-2=35.1 }, subtask-3=15.5 }"
	 * and returns a map with subtask names and values (key is "total" for the total time). 
	 */
	private Map<String, Double> parseStopWatchTimes(String logWithSubtasks) {
		
		Map<String, Double> ret = new LinkedHashMap<String, Double>();
		
		int totalTimeStartIndex = logWithSubtasks.indexOf(':') + 2;
		int totalTimeEndIndex = logWithSubtasks.indexOf(' ', totalTimeStartIndex);
		String totalTimeString = logWithSubtasks.substring(totalTimeStartIndex, totalTimeEndIndex);
		ret.put("total", Double.parseDouble(totalTimeString));
		
		String regexSubtaskWithTime = "subtask-[-0-9=.]*";
		Matcher m = Pattern.compile(regexSubtaskWithTime).matcher(logWithSubtasks);
		while (m.find()) {
			String[] splitSubtaskInfo = m.group().split("=");
			String subtaskName = splitSubtaskInfo[0];
			String timeString = splitSubtaskInfo[1];
			ret.put(subtaskName, Double.parseDouble(timeString));
		}
		return ret;
	}

}