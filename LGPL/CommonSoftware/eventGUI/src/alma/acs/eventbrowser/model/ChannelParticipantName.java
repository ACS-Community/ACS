package alma.acs.eventbrowser.model;

public class ChannelParticipantName {
	private String consumerName;
	private MCStatistics parent;
	
	public ChannelParticipantName(String name, MCStatistics parent) {
		consumerName = name;
		this.parent = parent;
	}
	
	public String getName() {
		return consumerName;
	}

	public MCStatistics getParent() {
		// TODO Auto-generated method stub
		return parent;
	}
}
