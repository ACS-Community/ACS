package alma.acs.monitoring.blobber;

import alma.acs.component.ComponentImplBase;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.container.ContainerServices;

import alma.MonitorArchiver.ControllerHelper;
import alma.MonitorArchiver.ControllerOperations;

public class MockControllerImpl extends ComponentImplBase implements ControllerOperations {
	public void registerCollector(String collectorCompName) {
	}

	public void deregisterCollector(String collectorCompName) {
	}

	public void registerKnownCollectors(String blobberCompName) {
	}
}
