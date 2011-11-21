package alma.acs.eventbrowser.model;

import java.util.ArrayList;

import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

public class ChannelConsumers extends MCStatistics {
	private  ArrayList<ChannelParticipantName> consumerNames;

	public ChannelConsumers(AbstractNotifyServiceElement parent) {
		super(parent);
		statName = "ConsumerNames";
	}
	
	@Override
	public String getStatistics() {
		String sc[];
		consumerNames = new ArrayList<ChannelParticipantName>();
		try {
			sc = mc.get_statistic(channelPrefix+statName).data_union.list();
			for (int i = 0; i < sc.length; i++) {
				consumerNames.add(new ChannelParticipantName(sc[i], this));
			}
		} catch (InvalidName e) {
			System.out.println("Invalid name: "+channelPrefix+statName);
		}
		return "Consumers: "+parent.getNumConsumersAndDelta();
	}
	
	public Object[] getNames() {
		return consumerNames.toArray();
	}
	
}
