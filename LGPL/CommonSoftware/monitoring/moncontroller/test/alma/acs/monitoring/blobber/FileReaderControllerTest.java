package alma.acs.monitoring.blobber;

import alma.MonitorArchiver.ControllerHelper;
import alma.TMCDB.MonitorCollector;
import alma.TMCDB.MonitorCollectorHelper;
import alma.acs.component.client.ComponentClientTestCase;

public class FileReaderControllerTest extends ComponentClientTestCase {

	private static final String COLLECTOR_URL  = "ARCHIVE/TMCDB/FILEBASED_MONITOR_COLLECTOR";
	private static final String CONTROLLER_URL = "ARCHIVE/TMCDB/MONITOR_CONTROL";

	private static final int SECS = 10*60;

	private MonitorCollector m_collector;

	public FileReaderControllerTest() throws Exception {
		super(FileReaderControllerTest.class.getSimpleName());
	}

	public void setUp() throws Exception {
		super.setUp();
		m_collector = MonitorCollectorHelper.narrow(getContainerServices().getComponent(COLLECTOR_URL));
	}

	public void testDoFileBasedMonitoring() throws Exception {
		for(String component: FileReaderCollectorImpl.monitoredComponents)
			m_collector.startMonitoring(component);

		Thread.sleep(SECS*1000); // Let the blobber do its job

		// Now let's stop monitoring
		ControllerHelper.narrow(getContainerServices().getComponent(CONTROLLER_URL)).deregisterCollector(COLLECTOR_URL);
		for(String component: FileReaderCollectorImpl.monitoredComponents)
			m_collector.stopMonitoring(component);

		// And let the blobber container run a bit more, so it GCs well
		Thread.sleep(10*1000);
	}

	public void tearDown() throws Exception {
		getContainerServices().releaseComponent(COLLECTOR_URL, null);
		super.tearDown();
	}

}
