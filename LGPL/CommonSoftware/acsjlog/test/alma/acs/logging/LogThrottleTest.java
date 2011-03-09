package alma.acs.logging;

import java.util.Arrays;
import java.util.Date;
import java.util.HashSet;
import java.util.Set;
import java.util.logging.Level;
import java.util.logging.LogRecord;
import java.util.logging.Logger;

import junit.framework.TestCase;

import alma.acs.logging.config.LogConfig;
import alma.acs.logging.formatters.ConsoleLogFormatter;
import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.util.IsoDateFormat;

public class LogThrottleTest extends TestCase
{
//	private LogConfig logConfig;
//	private TestThrottleCallback throttleCallback;
//	private LogThrottle throttle;

	protected void setUp() throws Exception {
		super.setUp();
	}
	

	/**
	 * Tests the LogThrottle class in isolation. 
	 * Many logs are simulated, over more than a second, to test also the re-opening of the throttle.
	 * Covers configuration without throttle, "total throttle" suppressing all logs, and "normal" throttling.
	 */
	public void testSimulatedLogs() throws Exception {
		LogConfig logConfig = new LogConfig();
		
		TestThrottleCallback throttleCallback = new TestThrottleCallback();
		LogThrottle throttle = new LogThrottle(logConfig, throttleCallback);
		assertEquals(-1, logConfig.getMaxLogsPerSecond());

		System.out.println("Will simulate logging (above throttle limit), for little more than 2 seconds, which should suppress & clear logs twice.");
		int maxLogsPerSecond = 10;
		logConfig.setMaxLogsPerSecond(maxLogsPerSecond);
		assertEquals(maxLogsPerSecond, logConfig.getMaxLogsPerSecond());
		simulateLogging(throttle, 2100, 20, 2*maxLogsPerSecond);
		Thread.sleep(1000);
		assertEquals(2, throttleCallback.getAndResetClearanceCounter());
		
		System.out.println("Will simulate logging without throttle set");
		logConfig.setMaxLogsPerSecond(-1);
		simulateLogging(throttle, 2100, 20, 60); // normally ~100, but some machines are slow.
		Thread.sleep(1000);
		assertEquals(0, throttleCallback.getAndResetClearanceCounter());

		System.out.println("Will simulate logging with degenerated throttle (all logs rejected)");
		logConfig.setMaxLogsPerSecond(0);
		simulateLogging(throttle, 2100, 10, 0);
		Thread.sleep(1000);
		assertEquals(0, throttleCallback.getAndResetClearanceCounter());
	}

	/**
	 * Broken out from {@link #testSimulatedLogs()}.
	 */
	private void simulateLogging(LogThrottle throttle, long durationMillis, long sleepMillis, int expectedNumberPassingLocalLogs) throws InterruptedException {
		long startTime = System.currentTimeMillis();
		System.out.println(IsoDateFormat.formatDate(new Date(startTime)) + " Starting with simulated logs...");
		int numberPassingLocalLogs = 0;
		while (System.currentTimeMillis() <= startTime + durationMillis) {
			if (throttle.checkPublishLogRecordLocal()) {
				numberPassingLocalLogs++;
			}
			// currently the test does not keep track of the "publish remote" call results
			throttle.checkPublishLogRecordRemote();
			
			Thread.sleep(sleepMillis);
		}
		assertTrue("A total of at least " + expectedNumberPassingLocalLogs + " should have passed the throttle, but was " + numberPassingLocalLogs, 
				expectedNumberPassingLocalLogs <= numberPassingLocalLogs);
	}

	/**
	 * Tests the LogThrottle class as part of the ClientLogManager - handler - formatter hierarchy.
	 * Installs mock objects to count the passing logs that go to stdout and remote. 
	 * To verify that the throttle only applies to logs that are not filtered out by log level settings, 
	 * for the remote log stream only log levels >= CRITICAL are logged. 
	 * The test then verifies that suppression of local logs does not yet suppress the fewer remote logs,
	 * while eventually also the remote logs get suppressed.
	 */
	public void testRealLogs() {
		for (int i = 1; i < 5; i++) {
			_testRealLogs(i);
		}
	}
	
	public void _testRealLogs(int numLoops) {
		TestClientLogManager clientLogManager = new TestClientLogManager(); // instead of usual static method getAcsLogManager()
		Logger logger = clientLogManager.getLoggerForApplication(getName(), true);
		
		LogConfig logConfig = clientLogManager.getLogConfig();
		AcsLogLevelDefinition remoteLevel = AcsLogLevelDefinition.CRITICAL;
		logConfig.setDefaultMinLogLevel(remoteLevel);
		logConfig.setDefaultMinLogLevelLocal(AcsLogLevelDefinition.TRACE);
		
		// the set of all levels used for ACS java logging (omitting JDK levels that have the same numerical value as a matching ACS level)
		Set<Level> levels = new HashSet<Level>();
		for (AcsLogLevelDefinition levelEnum : AcsLogLevelDefinition.values()) {
			if (levelEnum != AcsLogLevelDefinition.OFF) {
				levels.add(AcsLogLevel.fromAcsCoreLevel(levelEnum));
			}
		}
		levels.addAll(Arrays.asList(new Level[] {Level.FINEST, Level.FINER, Level.FINE, Level.INFO, Level.WARNING, Level.SEVERE}));
		int numLoggableRemoteLevels = 1;
		AcsLogLevelDefinition tmpLevel = remoteLevel;
		while (tmpLevel.getNextHigherLevel() != null) {
			tmpLevel = tmpLevel.getNextHigherLevel();
			numLoggableRemoteLevels++;
		}

		int expectedNumberOfLocalLogs = levels.size();
		int expectedNumberOfRemoteLogs = Math.min(expectedNumberOfLocalLogs, numLoggableRemoteLevels * numLoops);
		// set throttle so that every log level should be logged once
		logConfig.setMaxLogsPerSecond(expectedNumberOfLocalLogs);

		System.out.println("Will log a burst of " + levels.size() * numLoops + " messages of various levels, setting the throttle to " +
				expectedNumberOfLocalLogs + " logs per second.");
		System.out.println("For stdout logs (level = TRACE) we expect " + expectedNumberOfLocalLogs + " logs, while for remote logs " + 
				"(level = " + remoteLevel.name() + ") we expect " + expectedNumberOfRemoteLogs + " logs.");
		
		for (int i = 0; i < numLoops; i++) {
			for (Level level : levels) {
				logger.log(level, "test message with level " + level.getName());
			}
		}
		
		// verify the number of actual local logs
		assertEquals("Each log level should have produced only one local log.", expectedNumberOfLocalLogs, clientLogManager.getNumberLocalLogs());

		// The way this is constructed, the throttle should be active if numLoops > 1. 
		// Since no alarms are configured, we expect one warning log about the failure to raise a throttle alarm.
		int expectedAlarmErrorLogs = (numLoops > 1 ? 1 : 0);
		assertEquals("Bad number of throttle alarm error log(s)", expectedAlarmErrorLogs, clientLogManager.getNumberLocalThrottleAlarmErrorLogs());
		
		int numUserRemoteLogs = 0;
		for (LogRecord logRecord : clientLogManager.testLogQueue.logRecords) {
			if (!logRecord.getMessage().startsWith("Changed processName=")) {
				numUserRemoteLogs++;
			}
		}
		assertEquals("Wrong number of remote logs", expectedNumberOfRemoteLogs, numUserRemoteLogs);
	}

	
	/**
	 * Throttle action callback object, that counts the callbacks.
	 */
	private static class TestThrottleCallback implements LogThrottle.ThrottleCallback {
		private volatile int numSuppressed;
		private volatile int numClearance;
		
		public void clearedLogSuppression() {
			System.out.println(IsoDateFormat.formatCurrentDate() + " ClearedLogSuppression, after having suppressed " + numSuppressed + " logs.");
			numSuppressed = 0;
			numClearance++;
		}

		public void suppressedLog(boolean remoteLog) {
			if (numSuppressed == 0) {
				System.out.println(IsoDateFormat.formatCurrentDate() + " Suppression starts with remote=" + remoteLog);
			}
			else {
//				System.out.println(IsoDateFormat.formatCurrentDate() + " suppressed a log...");
			}
			numSuppressed++;
		}
		
		int getAndResetClearanceCounter() {
			int ret = numClearance;
			numClearance = 0;
			return ret;
		}
	}
	
	/**
	 * An instrumented version of ClientLogManager, which uses special log formatters to count the actual number of 
	 * local and remote logs.
	 */
	private static class TestClientLogManager extends ClientLogManager {
		
		private volatile int numLocalLogs;
		private volatile int numLocalThrottleAlarmErrorLogs;
		TestLogQueue testLogQueue;
		
		int getNumberLocalLogs() {
			return numLocalLogs;
		}

		int getNumberLocalThrottleAlarmErrorLogs() {
			return numLocalThrottleAlarmErrorLogs;
		}

		@Override
		protected StdOutConsoleHandler createStdOutConsoleHandlerWithFormatter(LogConfig logConfig, String loggerName, LogThrottle throttle) {
			StdOutConsoleHandler localHandler = new StdOutConsoleHandler(logConfig, loggerName, throttle);
			localHandler.setFormatter(new ConsoleLogFormatter() {
				@Override
				public String format(LogRecord record) {
					if (record.getMessage().equals("Cannot raise alarm about log throttle action because alarm callback or process name is not yet available.")) {
						numLocalThrottleAlarmErrorLogs++;
					}
					else if (!record.getMessage().startsWith("Changed processName=")) { 
						numLocalLogs++;
					}
					return super.format(record);
				}
			});
			return localHandler;
		}
		
		@Override
		protected DispatchingLogQueue createDispatchingLogQueue() {
			testLogQueue = new TestLogQueue(); // will not actually do remote logging, just counts the invocations
			return testLogQueue;
		}
	}
}
