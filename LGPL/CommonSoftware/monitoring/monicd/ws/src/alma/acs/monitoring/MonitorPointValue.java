package alma.acs.monitoring;

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
	
	
	public MonitorPointValue(long time) {
		this.time = time;
	}
	
	public void addValue(Object value) {
		data.add(value);
	}
	
	public long getTime() {
		return time;
	}
	
	/**
	 * Gets the data list that contains a single Number, Boolean, String etc object for a single-valued monitor point,
	 * or several such objects for a multi-valued monitor point. 
	 */
	public List<Object> getData() {
		return data;
	}
	
	public boolean isMultiValued() {
		return (data.size() > 1);
	}
}