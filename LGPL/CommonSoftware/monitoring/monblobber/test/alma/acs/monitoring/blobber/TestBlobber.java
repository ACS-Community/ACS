package alma.acs.monitoring.blobber;

import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.concurrent.ThreadLoopRunner;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.DAO.ComponentStatistics;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.acs.monitoring.blobber.TestBlobberWorker.TestBlobberPlugin;


/**
 * Blobber mock component that uses mock blobber worker.
 */
public class TestBlobber extends BlobberImpl {

	public TestBlobber() {
	}
	
	/**
	 * Allows a unit test to read all property data via (@link TestBlobberWorker#fetchData()} 
	 * before the next property data can be inserted in {@link TestMonitorDAO#store(ComponentData)}.
	 */
	private DataLock<BlobData> myBlobDataLock;

	private BlobberPlugin myBlobberPlugin;
	
	/**
	 * Call this method instead of the inherited {@link #initialize(ContainerServices)}.
	 * @param inContainerServices
	 * @param inName Name of this test component instance, so that we can pass the JUnit test's ContainerServices object and still 
	 *               use a component name different from that test name.
	 * @throws ComponentLifecycleException
	 */
	public void initialize(ContainerServices inContainerServices, String inName) throws ComponentLifecycleException {
		Logger tmpLogger = inContainerServices.getLogger();
		myBlobDataLock = new DataLock<BlobData>(tmpLogger, "blobberworker");
		MonitorDAO monitorDAO = new TestMonitorDAO(tmpLogger, myBlobDataLock);
		TestMonitorPointExpert myMonitorPointExpert = new TestMonitorPointExpert();
		myBlobberPlugin = new TestBlobberWorker.TestBlobberPlugin(inContainerServices, monitorDAO, myMonitorPointExpert);
		initialize(inContainerServices); // calls createBlobberPlugin() etc
		m_instanceName = inName;
	}

	/**
	 * Creates a {@link TestBlobberPlugin}}.
	 * 
	 * @see alma.archive.tmcdb.monitor.BlobberImpl#createBlobberPlugin()
	 */
	@Override
	protected BlobberPlugin createBlobberPlugin() {
		return myBlobberPlugin;
	}

	/**
	 * Creates a {@link TestBlobberWorker}.
	 * 
	 * @see alma.archive.tmcdb.monitor.BlobberImpl#createWorker()
	 * @throws AcsJCouldntCreateObjectEx
	 *             if mock objects cannot be created.
	 */
	@Override
	protected BlobberWorker createWorker() throws AcsJCouldntCreateObjectEx {
		return new TestBlobberWorker(m_containerServices, myBlobberPlugin, myBlobDataLock);
	}

	/**
	 * @TODO Check if tests really benefit from setting collector intervals. If not, remove this method.
	 * @param collectorIntervalSeconds	new interval in seconds, must be greater than zero.
	 * @see ThreadLoopRunner#setDelayTime(long, TimeUnit)
	 */
	protected void setCollectorIntervalSeconds(long collectorIntervalSeconds) {
		if (collectorIntervalSeconds <= 0) {
			throw new IllegalArgumentException("collectorIntervalSeconds must be > 0.");
		}
		long oldCollectorIntervalSeconds = blobberLoopRunner.getDelayTimeMillis() / 1000;
		myWorker.notifyCollectorIntervalChange(collectorIntervalSeconds);
		blobberLoopRunner.setDelayTime(collectorIntervalSeconds, TimeUnit.SECONDS);
		this.m_logger.fine("Changed collector interval from " + oldCollectorIntervalSeconds + " s to " + collectorIntervalSeconds + " s.");
	}
	
	
	private static class TestMonitorDAO implements MonitorDAO
	{
		private final Logger logger;
		private final DataLock<BlobData> myBlobDataLock;
		
		public TestMonitorDAO(Logger logger, DataLock<BlobData> myBlobDataLock) {
			this.logger = logger;
			this.myBlobDataLock = myBlobDataLock;
		}
		
		/**
		 * Can only store property data once the previous ComponentData has been fetched 
		 * (see {@link TestBlobberWorker#fetchData()} by the test.
		 */
		@Override
		public void store(ComponentData inData) throws Exception {
			logger.fine("Storing blobber data.");
			// this cast is dirty, but so far we have ComponentData and not BlobData in the MontiorDAO interface...
			this.myBlobDataLock.put(cloneData((BlobData)inData));
		}

		@Override
		public void close() {
			// Method not used. Forced implementation from interface.
		}

		@Override
		public void openTransactionStore(String transactionName) {
		}

		@Override
		public void closeTransactionStore() {
		}

		/**
		 * TODO: Why do we clone the data? 
		 */
		private BlobData cloneData(BlobData inBlob) {
			BlobData outBlob = new BlobData(inBlob.getMonitorPointTimeSeries(), logger);
			outBlob.clob = inBlob.clob;
			outBlob.componentName = inBlob.componentName;
			outBlob.index = inBlob.index;
			outBlob.propertyName = inBlob.propertyName;
			outBlob.sampleSize = inBlob.sampleSize;
			outBlob.serialNumber = inBlob.serialNumber;
			outBlob.startTime = inBlob.startTime;
			outBlob.stopTime = inBlob.stopTime;
			if (inBlob.statistics != null) {
				ComponentStatistics stats = new ComponentStatistics();
				outBlob.statistics = stats;
				stats.max = inBlob.statistics.max;
				stats.mean = inBlob.statistics.mean;
				stats.min = inBlob.statistics.min;
				stats.stdDev = inBlob.statistics.stdDev;
			}
			return outBlob;
		}
	}
	
	static class TestMonitorPointExpert implements MonitorPointExpert {

		private Map<String, Boolean> isMultivaluedPropertyMap = new HashMap<String, Boolean>();
		
		@Override
		public boolean isMultivaluedMonitorPoint(String propertyName) {
			Boolean ret = isMultivaluedPropertyMap.get(propertyName);
//	System.out.println("isMultivaluedMonitorPoint(" + propertyName + "): " + ret);
			return ( ret != null ? ret.booleanValue() : false );
		}
		
		void setMultivalued(String propertyName, boolean isMultivalued) {
			isMultivaluedPropertyMap.put(propertyName, isMultivalued);
		}
	}


}
