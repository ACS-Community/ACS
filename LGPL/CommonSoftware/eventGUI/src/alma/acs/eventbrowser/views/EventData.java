package alma.acs.eventbrowser.views;

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
	
	public EventData(long time, String srcObj, long count, String type, Integer typeCount) {
		timestamp = time;
		sourceObject = srcObj;
		channelEventCount = count;
		eventTypeName = type;
		eventTypeCount = typeCount;
	}

	public long getTimestamp() {
		return timestamp;
	}

	public String getChannelName() {
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
	
}
