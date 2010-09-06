package alma.acs.eventbrowser.model;

import alma.acs.eventbrowser.Application;
import alma.acs.eventbrowser.views.ArchiveEventData;

public class ArchiveReceiver {
	public void receive(Long timeStamp, String device, String parameter, Object value) {
		ArchiveEventData adata = new ArchiveEventData(timeStamp, device, parameter, value);
		Application.archQueue.add(adata);
		System.out.println(adata.toString()); // For diagnostic purposes
	}
}
