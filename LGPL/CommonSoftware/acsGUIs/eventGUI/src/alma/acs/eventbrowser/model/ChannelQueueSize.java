package alma.acs.eventbrowser.model;

import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

public class ChannelQueueSize extends MCStatistics {

	public ChannelQueueSize(AbstractNotifyServiceElement parent) {
		super(parent);
		statName = "QueueSize";
	}
	
	@Override
	public String getStatistics() {
		long sc = 0;
		try {
			sc = (long) mc.get_statistic(channelPrefix+statName).data_union.num().last;
		} catch (InvalidName e) {
			System.out.println("Invalid name: "+channelPrefix+statName);
		}
		return statName+": "+String.valueOf(sc);
	}
	
}
