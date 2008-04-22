package alma.acs.eventbrowser.views;

public class EventData {
	/*
	 * "Time "+timeStamp+" "+m_channelName+" "+component+" "+count+" "+channelEventCount+" "
			+" "+evtTypeName+" "+evtCounter.get(evtTypeName)
	 */
	private long timestamp;
	private String channelName;
	private long channelEventCount;
	private String eventTypeName;
	private long eventTypeCount;
	
	public EventData(long time, String channel, long count, String type, Integer typeCount) {
		timestamp = time;
		channelName = channel;
		channelEventCount = count;
		eventTypeName = type;
		eventTypeCount = typeCount;
	}

	public long getTimestamp() {
		return timestamp;
	}

	public String getChannelName() {
		return channelName;
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
