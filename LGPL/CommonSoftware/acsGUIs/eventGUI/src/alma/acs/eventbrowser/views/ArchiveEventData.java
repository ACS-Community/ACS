package alma.acs.eventbrowser.views;

import java.util.concurrent.atomic.AtomicLong;

/**
 * TODO: Expand to get the parameter type as well, so that value can be interpreted
 */

public class ArchiveEventData extends AbstractEventData {
	protected static AtomicLong totalEventsProcessed = new AtomicLong();
	protected static AtomicLong timeFirstEventProcessed = new AtomicLong();
	
	private final String device;
	private final String parameter;
	private final Object value;
	
	public ArchiveEventData(Long time, String device, String parameter, Object value) {
		super();
		timestamp = time;
		this.device = device;
		this.parameter = parameter;
		this.value = value;
		totalEventsProcessed.getAndIncrement();
		timeFirstEventProcessed.compareAndSet(0L, System.currentTimeMillis());
	}
	
	public static long getTotalEventsProcessed() {
		return totalEventsProcessed.get();
	}
	
	/**
	 * @return Time (in ms) of first event processed
	 */
	public static Long getTimeFirstEventProcessed() {
		return timeFirstEventProcessed.get();
	}
	
	/**
	 * @return Average number of events/s since the first one that was processed
	 */
	public static float getAverageRate() {
		return ((float)getTotalEventsProcessed()*1000.f)/((float)(System.currentTimeMillis()-getTimeFirstEventProcessed()));
	}

	public String getDevice() {
		return device;
	}


	public String getParameter() {
		return parameter;
	}

	public Object getValue() {
		return value;
	}
	
	public String toString() {
		return "Timestamp "+getTimestamp()+" Device "+getDevice()+" Property/parameter "+getParameter()+" Value "+getValue().toString();
	}

}
