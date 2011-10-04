package alma.acs.profiling.orb;

import java.io.File;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;
import java.util.logging.Logger;

import alma.acs.logging.ClientLogManager;
import alma.acs.profiling.orb.ProfilerMessage.Type;
import alma.acs.util.IsoDateFormat;

public class OrbProfilerStatistics
{

	private final List<ProfilerMessage> messages;
	private final Logger logger;
//	private final long toleratedTimestampFluctuationMillis = 1;

	public OrbProfilerStatistics(List<ProfilerMessage> messages, Logger logger) {
		this.messages = messages;
		this.logger = logger;
	}
	
	public static class TimeValue implements Comparable<TimeValue> {
		long timeMillis;
		int value;
		TimeValue(long timestamp, int value) {
			this.timeMillis = timestamp;
			this.value = value;
		}
		@Override
		public String toString() {
			return IsoDateFormat.formatDate(new Date(timeMillis)) + " " + value;
		}
		@Override
		public int compareTo(TimeValue other) {
			if (this.timeMillis < other.timeMillis) return -1;
			if (this.timeMillis > other.timeMillis) return 1;
			return 0; // not consistent with equals
		}
	}
	
	public static class BinnedTimeValues {
		long timeMillis; // center of binning interval
		List<TimeValue> binnedData;
		BinnedTimeValues(long timeMillis, List<TimeValue> binnedData) {
			this.timeMillis = timeMillis;
			this.binnedData = binnedData;
		}
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
	
	
	public List<TimeValue> getFinishedRequests(String operation) {
		List<TimeValue> ret = new ArrayList<TimeValue>();
		
		for (ProfilerMessage msg : messages) {
			if (msg.type == Type.REQUEST_FINISHED
				&& (operation == null || operation.equals(msg.operation))) {
					TimeValue tv = new TimeValue(msg.timestamp, (int)msg.timeElapsedMillis);
					ret.add(tv);
			}
		}
		return ret;
	}
	
	
	/**
	 * Apache commons math etc do not seem to provide decent binning functionality
	 * (except for a possible abuse of class EmpiricalDistributionImpl).
	 * That's why we do it here.
	 */
	public List<BinnedTimeValues> binTimedData(List<TimeValue> data, int binningIntervalMillis) {
		if (binningIntervalMillis <= 1 || 
			((binningIntervalMillis % 1000 != 0) && ((1000 % (binningIntervalMillis % 1000)) != 0) )) {
			throw new IllegalArgumentException("Bad binningIntervalMillis=" + binningIntervalMillis);
		}
		List<BinnedTimeValues> ret = new ArrayList<BinnedTimeValues>();

		long t0 = floor(data.get(0).timeMillis, binningIntervalMillis);
		long tBinFloor = t0; // floor time in ms is included in the bin interval. 
		long tCurrent = t0; 
		List<TimeValue> currentBinData = new ArrayList<TimeValue>();
		for (Iterator<TimeValue> dataIter = data.iterator(); dataIter.hasNext();) {
			TimeValue timeValue = dataIter.next();
			// assert time ordered list
			if (timeValue.timeMillis < tCurrent) {
				throw new IllegalArgumentException("Expecting time-ordered list! Error at time " + IsoDateFormat.formatDate(new Date(timeValue.timeMillis)) );
			}
			tCurrent = timeValue.timeMillis;
			// Leaving the current bin?
			while (tCurrent >= tBinFloor + binningIntervalMillis) {
				// store old bin data
				BinnedTimeValues binnedTimeValue = new BinnedTimeValues(tBinFloor + binningIntervalMillis/2, currentBinData);
				ret.add(binnedTimeValue);
				// prepare next bin (possibly empty)
				currentBinData = new ArrayList<TimeValue>();
				tBinFloor += binningIntervalMillis;
			}
			currentBinData.add(timeValue);
			// last bin?
			if (!dataIter.hasNext() && !currentBinData.isEmpty()) {
				BinnedTimeValues binnedTimeValue = new BinnedTimeValues(tBinFloor + binningIntervalMillis/2, currentBinData);
				ret.add(binnedTimeValue);
			}
		}
		
		return ret;
	}

	
	public List<TimeValue> getConcurrentCalls() {
		int startedCount = 0;
		List<TimeValue> ret = new ArrayList<TimeValue>();
		
		for (ProfilerMessage msg : messages) {
			if (msg.type == ProfilerMessage.Type.REQUEST_STARTED ||
					msg.type == ProfilerMessage.Type.REQUEST_FINISHED) {
				if (msg.type == ProfilerMessage.Type.REQUEST_STARTED) {
					startedCount++;
				}
				else {
					if (startedCount > 0) {
						startedCount--;
					}
					else {
						logger.warning("Bad input data at '" + IsoDateFormat.formatDate(new Date(msg.timestamp)) + "': more calls finished than started.");
					}
				}
				TimeValue tv = new TimeValue(msg.timestamp, startedCount);
				ret.add(tv);
			}
		}
		return ret;
	}
	
	public void printConcurrentCalls(PrintStream out) {
		List<TimeValue> concCalls = getConcurrentCalls();
		int maxConcurrentCalls = 0;
		for (TimeValue timeValue : concCalls) {
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
	
	
//	public long ceiling(long value, long multipleOf) {
//		if (value % multipleOf == 0) {
//			return value;
//		}
//		return ((value / multipleOf) + 1) * multipleOf;
//	}
	
	public long floor(long value, long multipleOf) {
		return (value / multipleOf) * multipleOf;
	}
	
	
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
