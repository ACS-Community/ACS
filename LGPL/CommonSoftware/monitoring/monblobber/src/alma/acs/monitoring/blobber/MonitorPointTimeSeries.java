package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.List;


/**
 * Holds the time-series data of one logical monitor point that was extracted 
 * from the Corba Any representation sent by the monitor collector.
 * <p>
 * Note that data from one multi-valued monitor point
 * may have been split into several MonitorPointTimeSeries instances.
 */
public class MonitorPointTimeSeries
{
	/**
	 * 0-based property index.
	 * It is greater than zero only for monitor points that were extracted
	 * (=demultiplexed) from a single baci property that represents multiple logical monitor points. 
	 */
	private final int index;

	/**
	 * @see #getCorbaTypeId()
	 */
	private final String corbaTypeId;
	
	/**
	 * The time series list.
	 */
	private final List<MonitorPointValue> dataList;


	/**
	 * Constructor.
	 * 
	 * @param index The index is <code>0</code> if this monitor point represents one baci property;
	 *              It is a running integer value if multiple logical monitor points get extracted from one baci property.
	 * @param corbaTypeId  See {@link #getCorbaTypeId()}.
	 */
	MonitorPointTimeSeries(int index, String corbaTypeId) {
		this.index = index;
		this.corbaTypeId = corbaTypeId;
		dataList = new ArrayList<MonitorPointValue>();
	}

	/**
	 * Adds a <code>MonitorPointValue</code> to the list of time-series data.
	 */
	void addMonitorPointValue(MonitorPointValue value) {
		dataList.add(value);
	}
	
	int getMonitorPointIndex() {
		return index;
	}

	/**
	 * Gets the Corba Repository Id that the data was wrapped in 
	 * (or would have been wrapped in, had it not been multiplexed).
	 * <p> 
	 * Currently used only for error logs, where it could become even more useful 
	 * if we have to debug issues with unsigned IDL type mismatches in the future.
	 */
	String getCorbaTypeId() {
		return corbaTypeId;
	}

	/**
	 * The time series list of MonitorPointValue objects,
	 * each of which holds one Number, Boolean, String etc object,
	 * or several such objects, depending on the monitor point type.
	 */
	List<MonitorPointValue> getDataList() {
		return dataList;
	}
}