package alma.acs.nc;

import gov.sandia.NotifyMonitoringExt.EventChannelFactory;

public interface ReconnectableSubscriber {

	public void reconnect(EventChannelFactory ecf);
}
