package alma.acs.monitoring.blobber;

import java.lang.reflect.Constructor;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.acs.component.ComponentLifecycleException;
import alma.acs.concurrent.ThreadLoopRunner;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.DAO.ComponentStatistics;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.acs.monitoring.blobber.CollectorList.BlobData;
import alma.acs.monitoring.blobber.TestBlobberWorker.TestBlobberPlugin;


/**
 * Blobber mock component that uses mock blobber worker.
 */
public class TestBlobber extends BlobberImpl {

	private final ContainerServices containerServices;

	public TestBlobber(ContainerServices containerServices) {
		this.containerServices = containerServices;
	}
	
	private boolean useDatabase;
	
	/**
	 * Allows a unit test to read all property data via (@link TestBlobberWorker#fetchData()} 
	 * before the next property data can be inserted in {@link TestMonitorDAO#store(ComponentData)}.
	 */
	private DataLock<ComponentData> myBlobDataLock;

	/**
	 * Call this method instead of the inherited {@link #initialize(ContainerServices)}.
	 * @param inContainerServices
	 * @param inName Name of this test component instance, so that we can pass the JUnit test's ContainerServices object and still 
	 *               use a component name different from that test name.
	 * @param inUseDatabase see {@link TestBlobberWorker#setUseDatabase(boolean)}.
	 * @throws ComponentLifecycleException
	 */
	public void initialize(ContainerServices inContainerServices, String inName, boolean inUseDatabase) throws ComponentLifecycleException {
		useDatabase = inUseDatabase;
		myBlobDataLock = new DataLock<ComponentData>(inContainerServices.getLogger(), "blobberworker");
		initialize(inContainerServices);
		this.m_instanceName = inName;
	}

	/**
	 * Creates a {@link TestBlobberPlugin}}.
	 * 
	 * @see alma.archive.tmcdb.monitor.BlobberImpl#createBlobberPlugin()
	 */
	@Override
	protected BlobberPlugin createBlobberPlugin() throws AcsJCouldntCreateObjectEx {
		MonitorDAO monitorDAO = null;
		if (useDatabase) {
			try {
				// TODO If "useDatabase==true" is ever needed, then use alma.acs.monitoring.blobber.BlobberPluginAlmaImpl#getMonitorDAOs().
				// For now we just throw an exception to document that the code is broken.
				throw new AcsJCouldntCreateObjectEx("Currently not supported.");
//				Class<? extends MonitorDAO> daoClass = Class.forName("alma.archive.tmcdb.DAO.MonitorDAOImpl")
//						.asSubclass(MonitorDAO.class);
//				Constructor<? extends MonitorDAO> ctor = daoClass.getConstructor(Logger.class);
//				monitorDAO = ctor.newInstance(m_logger);
			} catch (Exception ex) {
				m_logger.log(Level.SEVERE, "Failed to create instance of alma.archive.tmcdb.DAO.MonitorDAOImpl", ex);
				throw new AcsJCouldntCreateObjectEx(ex);
			}
		} else {
			monitorDAO = new TestMonitorDAO(m_logger, myBlobDataLock);
		}
		return new TestBlobberWorker.TestBlobberPlugin(containerServices, monitorDAO);
	}

	/**
	 * Creates a {@link TestBlobberWorker}, or the real one if <code>inUseDatabase == true</code> in
	 * {@link #initialize(ContainerServices, String, boolean)}.
	 * 
	 * @see alma.archive.tmcdb.monitor.BlobberImpl#createWorker()
	 * @throws AcsJCouldntCreateObjectEx
	 *             if mock objects cannot be created.
	 */
	@Override
	protected BlobberWorker createWorker(BlobberPlugin blobberPlugin) throws AcsJCouldntCreateObjectEx {
		MonitorDAO monitorDAO = null;
		if (useDatabase) {
			try {
				// TODO If "useDatabase==true" is ever needed, then use alma.acs.monitoring.blobber.BlobberPluginAlmaImpl#getMonitorDAOs().
				// For now we just throw an exception to document that the code is broken.
				throw new AcsJCouldntCreateObjectEx("Currently not supported.");
//				Class<? extends MonitorDAO> daoClass = Class.forName("alma.archive.tmcdb.DAO.MonitorDAOImpl")
//						.asSubclass(MonitorDAO.class);
//				Constructor<? extends MonitorDAO> ctor = daoClass.getConstructor(ContainerServices.class);
//				monitorDAO = ctor.newInstance(containerServices);
			} catch (Exception ex) {
				m_logger.log(Level.SEVERE, "Failed to create instance of alma.archive.tmcdb.DAO.MonitorDAOImpl", ex);
				throw new AcsJCouldntCreateObjectEx(ex);
			}
		} else {
			monitorDAO = new TestMonitorDAO(m_logger, myBlobDataLock);
		}
		try {
			return new TestBlobberWorker(m_containerServices, monitorDAO, myBlobDataLock);
		} catch (AcsJCouldntCreateObjectEx ex) {
			throw new IllegalArgumentException(ex);
		}
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
		private final DataLock<ComponentData> myBlobDataLock;
		public TestMonitorDAO(Logger logger, DataLock<ComponentData> myBlobDataLock) {
			this.logger = logger;
			this.myBlobDataLock = myBlobDataLock;
		}
		
		/**
		 * Can only store property data once the previous ComponentData has been fetched 
		 * (see {@link TestBlobberWorker#fetchData()} by the test.
		 * @see alma.archive.tmcdb.DAO.MonitorDAO#store(alma.archive.tmcdb.DAO.ComponentData)
		 */
		@Override
		public void store(ComponentData inData) throws Exception {
			logger.fine("Storing blobber data.");
			this.myBlobDataLock.put(cloneData(inData));
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

		private BlobData cloneData(ComponentData inBlob) {
			BlobData outBlob = new BlobData();
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
}
