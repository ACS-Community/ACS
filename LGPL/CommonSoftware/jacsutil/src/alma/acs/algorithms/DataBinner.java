package alma.acs.algorithms;

import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;

import org.apache.commons.lang.builder.EqualsBuilder;
import org.apache.commons.lang.builder.HashCodeBuilder;

import alma.acs.util.IsoDateFormat;

/**
 * Apache commons math etc do not seem to provide decent binning functionality
 * (except for a possible abuse of class EmpiricalDistributionImpl).
 * That's why we do it here.
 * It's an ad-hoc implementation used for binning various parsed log data
 * in post-mortem debugging.  Not yet tested enough for important operational use.
 */
public class DataBinner
{
	/**
	 * Class to represent a point in a time series of data. 
	 * The binning will be done over time intervals. 
	 * (Instead of time in milliseconds, any other "long" kind of data can be used for binning.)
	 * The <code>value</code> carries arbitrary data, which can later be 
	 * retrieved as a list for a given bin interval.
	 * @param <T> The type of {@link #value}.
	 */
	public static class TimeValue<T extends Comparable<T>> implements Comparable<TimeValue<T>> {
		public final long timeMillis;
		public final T value;
		public TimeValue(long timestamp, T value) {
			this.timeMillis = timestamp;
			this.value = value;
		}
		@Override
		public String toString() {
			return IsoDateFormat.formatDate(new Date(timeMillis)) + " " + value;
		}
		@Override
		public int compareTo(TimeValue<T> other) {
			if (this.timeMillis < other.timeMillis) return -1;
			if (this.timeMillis > other.timeMillis) return 1;
			return (this.value.compareTo(other.value));
		}
		@Override
		public boolean equals(Object obj) {
			if (this == obj) return true;
			if (!(obj instanceof TimeValue<?>)) return false;
			TimeValue<?> other = (TimeValue<?>) obj;
			return new EqualsBuilder()
				.append(timeMillis, other.timeMillis)
				.append(value, other.value)
				.isEquals();
		}
		@Override
		public int hashCode() {
			return new HashCodeBuilder().
				append(timeMillis).
				append(value).
				toHashCode();
		}
	}
	
	/**
	 * Represents a binning (time) interval and the data mapped to it.
	 * @param <T>
	 */
	public static class BinnedTimeValues<T extends Comparable<T>> {
		/**
		 * Center of binning interval.
		 */
		public final long timeMillis;  
		public final List<TimeValue<T>> binnedData;
		BinnedTimeValues(long timeMillis, List<TimeValue<T>> binnedData) {
			this.timeMillis = timeMillis;
			this.binnedData = binnedData;
		}
	}
	

	/**
	 * Distributes the <code>data</code> into binning intervals of equal sizes (<code>binningIntervalMillis</code>).
	 * @param <T> The type of data associated with a time.
	 * @param data
	 * @param binningIntervalMillis
	 * @return data mapped to bins.
	 */
	public <T extends Comparable<T>> List<BinnedTimeValues<T>> binTimedData(List<TimeValue<T>> data, int binningIntervalMillis) {
		if (binningIntervalMillis <= 1 || 
			((binningIntervalMillis % 1000 != 0) && ((1000 % (binningIntervalMillis % 1000)) != 0) )) {
			throw new IllegalArgumentException("Bad binningIntervalMillis=" + binningIntervalMillis);
		}
		List<BinnedTimeValues<T>> ret = new ArrayList<BinnedTimeValues<T>>();

		long t0 = floor(data.get(0).timeMillis, binningIntervalMillis);
		long tBinFloor = t0; // floor time in ms is included in the bin interval. 
		long tCurrent = t0; 
		List<TimeValue<T>> currentBinData = new ArrayList<TimeValue<T>>();
		for (Iterator<TimeValue<T>> dataIter = data.iterator(); dataIter.hasNext();) {
			TimeValue<T> timeValue = dataIter.next();
			// assert time ordered list
			if (timeValue.timeMillis < tCurrent) {
				throw new IllegalArgumentException("Expecting time-ordered list! Error at time " + IsoDateFormat.formatDate(new Date(timeValue.timeMillis)) );
			}
			tCurrent = timeValue.timeMillis;
			// Leaving the current bin?
			while (tCurrent >= tBinFloor + binningIntervalMillis) {
				// store old bin data
				BinnedTimeValues<T> binnedTimeValue = new BinnedTimeValues<T>(tBinFloor + binningIntervalMillis/2, currentBinData);
				ret.add(binnedTimeValue);
				// prepare next bin (possibly empty)
				currentBinData = new ArrayList<TimeValue<T>>();
				tBinFloor += binningIntervalMillis;
			}
			currentBinData.add(timeValue);
			// last bin?
			if (!dataIter.hasNext() && !currentBinData.isEmpty()) {
				BinnedTimeValues<T> binnedTimeValue = new BinnedTimeValues<T>(tBinFloor + binningIntervalMillis/2, currentBinData);
				ret.add(binnedTimeValue);
			}
		}
		
		return ret;
	}

	public static long floor(long value, long multipleOf) {
		return (value / multipleOf) * multipleOf;
	}

}
