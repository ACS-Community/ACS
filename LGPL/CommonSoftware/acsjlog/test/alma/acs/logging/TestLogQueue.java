package alma.acs.logging;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Future;
import java.util.logging.LogRecord;

/**
 * Test log queue that allows verification of calls to <code>log</code> and <code>flush</code>.
 * <p> 
 * Actually this class does not support any of the special features of DispatchingLogQueue.
 * It will not even forward the log record to it.
 * It only extends DispatchingLogQueue because AcsLoggingHandler requires a DispatchingLogQueue in the ctor
 * and I see no big advantage in refactoring this.
 */
class TestLogQueue extends DispatchingLogQueue {
	public int nFlush;
	public List<LogRecord> logRecords;
	
	TestLogQueue() {
		logRecords = new ArrayList<LogRecord>();
		reset();
	}
	
	void reset() {
		nFlush = 0;
		logRecords.clear();
	}
	
	Future<Boolean> flush() {
		nFlush++;
		return super.flush();
	}

	synchronized boolean log(LogRecord logRecord) {
		logRecords.add(logRecord);
//		System.out.println("remote log: " + logRecord.getMessage());
		return true;
	}
}
