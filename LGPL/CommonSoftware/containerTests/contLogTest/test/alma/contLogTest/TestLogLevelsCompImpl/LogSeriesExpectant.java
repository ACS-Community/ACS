package alma.contLogTest.TestLogLevelsCompImpl;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;

import com.cosylab.logging.engine.log.ILogEntry; 

import alma.acs.logging.level.AcsLogLevelDefinition;
import alma.acs.logging.engine.LogReceiver;
import alma.acs.logging.engine.LogReceiver.DelayedLogEntry;
import alma.acs.logging.engine.LogReceiver.ReceivedLogRecord;


/**
 * Helper class for tests that must receive log records from the Log service.
 */
public class LogSeriesExpectant
{
	private LogReceiver logReceiver;

	/**
	 * @param logReceiver  an already initialized LogReceiver from which logs will be fetched.
	 *                     This class will not {@link LogReceiver#stop() stop} the logReceiver,
	 *                     which therefore remains the task of the calling class.
	 * @throws IllegalArgumentException if logReceiver is not initialized. 
	 */
	LogSeriesExpectant(LogReceiver logReceiver) {
		
		if (!logReceiver.isInitialized()) {
			throw new IllegalArgumentException("The given logReceiver must be initialized!");
		}
		this.logReceiver = logReceiver;
	}
	

	/**
	 * Call this method before the logs that we are interested in get produced.
	 */
	public void clearLogs() {
		logReceiver.getLogQueue().clear();
	}
	
	
	/**
	 * Call this method after the logs that should be received have been produced.
	 * Then the call may take up to <code>logRoundtripTimeSeconds</code> seconds to return.  
	 * 
	 * @param loggerName  The name of the logger whose records are to be collected. 
	 *                    Filtering of arriving log records will be done for the {@link ILogEntry.Field#SOURCEOBJECT} field.
	 * @param logRoundtripTimeSeconds  Maximum roundtrip time of a log record 
	 *                                 (incl. caching by container, sending to Log service, processing by Log service, arrival at LogReceiver).
	 */
	LogList awaitLogRecords(String loggerName, int logRoundtripTimeSeconds) {
		int sortingTimeWindowSeconds = 0; // doesn't look like we need a running time windows to sort log records as long as we sort the list in the end.
		long endTimeMillis = System.currentTimeMillis() + (logRoundtripTimeSeconds + sortingTimeWindowSeconds) * 1000;
		LogList logRecords = new LogList();
		logReceiver.setDelayMillis(sortingTimeWindowSeconds * 1000);
		BlockingQueue<DelayedLogEntry> queue = logReceiver.getLogQueue();
		
		long waitTimeMillis = -1;
        while ((waitTimeMillis = endTimeMillis - System.currentTimeMillis()) > 0) {
            DelayedLogEntry delayedLogEntry = null;
			try {
				delayedLogEntry = queue.poll(waitTimeMillis, TimeUnit.MILLISECONDS);
			} catch (InterruptedException ex) {
				// continue
			}
            if (delayedLogEntry != null) { // timeout yields a null
                ReceivedLogRecord logRecord = delayedLogEntry.getLogRecord();
                String sourceObjectName = logRecord.getSourceObject();
                if (sourceObjectName != null && sourceObjectName.equals(loggerName)) {
                	// printing this could be useful for debugging the test
                	//System.out.println(logRecord.getMessage());
                	
                	if (logRecord.getMessage() == null) {
                		System.out.println("BUG: got a null log-message from " + sourceObjectName);
                		continue;
                	}
                	// Filter out python trace-logs (which could make it or not, depending on timing)
                	if (logRecord.getMessage().startsWith("initialize -") ||
                		logRecord.getMessage().startsWith("getLevels -") ||
                		logRecord.getMessage().startsWith("cleanUp -")) {
                    	//System.out.println("Dropping this log: "+ logRecord.getMessage());
                		continue;
                	}
                	// Filter out messages which are a work-around for a python issue 
                	if (!logRecord.getMessage().endsWith("===packet fill-up message===")) {
                		logRecords.add(logRecord);
                	}
                	if (logRecord.getMessage().endsWith("===last log message===")) {
                		// don't wait for timeout
                		break;
                	}
                }
                else {
                	// printing this could be useful for debugging the test
                	//System.out.println("Dropped log message from " + sourceObjectName + ": " + logRecord.getMessage());
                	//System.out.println("Dropped log message from " + sourceObjectName + ": " + logEntry.getField(ILogEntry.Field.LOGMESSAGE));
                }
            }
        }
        // sort by timestamp
        Collections.sort(logRecords, new Comparator<ReceivedLogRecord>() {
			public int compare(ReceivedLogRecord o1, ReceivedLogRecord o2) {
				return o1.getTimestamp().compareTo(o2.getTimestamp()) ;
			}        	
        });
        return logRecords;
	}
		
	
//	/**
//	 * Sleeps as long as the granularity of OS time makes {@link System#currentTimeMillis()} return the same value.
//	 * Calling this method between sending of two logs guarantees that the log records carry different time stamps.
//	 */
//	public void awaitSystimeChange() {
//		long t1 = System.currentTimeMillis();
//		while (System.currentTimeMillis() == t1) {
//			try {
//				Thread.sleep(20);
//			} catch (InterruptedException ex) {
//				ex.printStackTrace();
//			}
//		}
//	}
	
	public static class LogList extends ArrayList<ReceivedLogRecord> {

		public int getMinLogLevel() {
			int minLevel = Integer.MAX_VALUE;
			for (ReceivedLogRecord logRecord : this) {
				minLevel = Math.min(minLevel, logRecord.getLevel().acsCoreLevel.value);
			}
			return minLevel;
		}
		
		public int getMaxLogLevel() {
			int maxLevel = -1;
			for (ReceivedLogRecord logRecord : this) {
				maxLevel = Math.max(maxLevel, logRecord.getLevel().acsCoreLevel.value);
			}
			return maxLevel;
		}
	}
	
}
