package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.List;

/**
 * Holds the value(s) of a logical monitor point at one instant of time.
 */
public class MonitorPointValue
{
	/**
	 * Timestamp of the data.
	 */
	private final long time;
	
	/**
	 * The data list. 
	 * Contains one Number, Boolean, String etc object for a single-valued monitor point,
	 * or several such objects for a multi-valued monitor point. 
	 */
	private final List<Object> data = new ArrayList<Object>();
	
	
	MonitorPointValue(long time) {
		this.time = time;
	}
	
	void addValue(Object value) {
		data.add(value);
	}
	
	long getTime() {
		return time;
	}
	
	/**
	 * Gets the data list that contains a single Number, Boolean, String etc object for a single-valued monitor point,
	 * or several such objects for a multi-valued monitor point. 
	 */
	List<Object> getData() {
		return data;
	}
	
	boolean isMultiValued() {
		return (data.size() > 1);
	}
}