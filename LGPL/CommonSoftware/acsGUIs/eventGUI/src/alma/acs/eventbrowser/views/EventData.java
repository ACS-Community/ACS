package alma.acs.eventbrowser.views;

import java.util.concurrent.atomic.AtomicLong;

import org.omg.CORBA.Any;

public class EventData extends AbstractEventData {

	protected static AtomicLong totalEventsProcessed = new AtomicLong();
	protected static AtomicLong timeFirstEventProcessed = new AtomicLong();
	/*
	 * "Time "+timeStamp+" "+m_channelName+" "+component+" "+count+" "+channelEventCount+" "
			+" "+evtTypeName+" "+evtCounter.get(evtTypeName)
	 */
	private final String sourceObject;
	private final long channelEventCount;
	private final String eventTypeName;
	private final long eventTypeCount;
	private final String channelName;
	private final Any eventAny;

	
	public EventData(long time, String srcObj, long count, String type, Integer typeCount, String chanName, Any any) {
		timestamp = time;
		sourceObject = srcObj;
		channelEventCount = count;
		eventTypeName = type;
		eventTypeCount = typeCount;
		channelName = chanName;
		eventAny = any;
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

	public String getSourceObject() {
		return sourceObject;
	}

	public long getChannelEventCount() {
		return channelEventCount;
	}

	public String getEventTypeName() {
		return eventTypeName;
	}

	public long getEventTypeCount() {
		return eventTypeCount;
	}
	
	public String getChannelName() {
		return channelName;
	}
	
	public Any getEventAny() {
		return eventAny;
	}
	
}
