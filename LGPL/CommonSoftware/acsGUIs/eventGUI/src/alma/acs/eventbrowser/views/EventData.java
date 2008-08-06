package alma.acs.eventbrowser.views;

import org.omg.CORBA.Any;

public class EventData {
	/*
	 * "Time "+timeStamp+" "+m_channelName+" "+component+" "+count+" "+channelEventCount+" "
			+" "+evtTypeName+" "+evtCounter.get(evtTypeName)
	 */
	private final long timestamp;
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
	}

	public long getTimestamp() {
		return timestamp;
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
