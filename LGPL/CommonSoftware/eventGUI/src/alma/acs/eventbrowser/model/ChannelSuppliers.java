package alma.acs.eventbrowser.model;

import java.util.ArrayList;

import gov.sandia.CosNotification.NotificationServiceMonitorControlPackage.InvalidName;

public class ChannelSuppliers extends MCStatistics {
	private  ArrayList<ChannelParticipantName> supplierNames;

	public ChannelSuppliers(AbstractNotifyServiceElement parent) {
		super(parent);
		statName = "SupplierNames";
	}
	
	@Override
	public String getStatistics() {
		String sc[];
		supplierNames = new ArrayList<ChannelParticipantName>();
		try {
			sc = mc.get_statistic(channelPrefix+statName).data_union.list();
			for (int i = 0; i < sc.length; i++) {
				supplierNames.add(new ChannelParticipantName(sc[i], this));
			}
		} catch (InvalidName e) {
			System.out.println("Invalid name: "+channelPrefix+statName);
		}
		return "Suppliers: "+parent.getNumSuppliersAndDelta();
	}
	
	public Object[] getNames() {
		return supplierNames.toArray();
	}
	
}
