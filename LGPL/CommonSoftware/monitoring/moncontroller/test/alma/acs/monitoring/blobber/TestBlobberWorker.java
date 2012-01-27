package alma.acs.monitoring.blobber;

import java.util.ArrayList;
import java.util.List;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.TMCDB.MonitorCollectorOperations;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.acs.monitoring.blobber.CollectorList.BlobData;

/**
 * BlobberWorker mock.
 */
public class TestBlobberWorker extends BlobberWorker {

	private volatile MonitorTestCollector myCollector;

//	private volatile Long mySafetyMargin = 0L;

	private volatile Boolean myCanHandle = true;

	/**
	 * Allows a unit test to read all property data via (@link #fetchData()} 
	 * before the next property data can be inserted.
	 */
	private final DataLock<ComponentData> myBlobDataLock;


	/**
	 * @param inContainerServices
	 * @param monitorDAO MonitorDAO, either mock or real
	 * @param myBlobDataLock  see {@link #myBlobDataLock}.
	 * @throws AcsJCouldntCreateObjectEx
	 */
	public TestBlobberWorker(ContainerServices inContainerServices, MonitorDAO monitorDAO, DataLock<ComponentData> myBlobDataLock) 
			throws AcsJCouldntCreateObjectEx {
		super(inContainerServices, new TestBlobberPlugin(inContainerServices, monitorDAO));
		this.myBlobDataLock = myBlobDataLock;
	}

	@Override
	protected void initWorker() {
		this.myCollector = new MonitorTestCollector(myLogger);
	}

	@Override
	protected MonitorCollectorOperations getMonitorCollector(String inCollectorName) {
		return this.myCollector;
	}

	@Override
	protected boolean canHandle() {
		return this.myCanHandle;
	}

	protected void setCanHandle(boolean inValue) {
		this.myCanHandle = inValue;
	}

    /**
     * Fetches the data stored in {@link #storeData(BlobData)},
     * with the side effect that the next ComponentData can then be stored.
     * This mechanism puts the test in the position of the backing database that
     * can see what property data came in.
     * @throws InterruptedException 
     */
    protected ComponentData fetchData() throws InterruptedException {
        myLogger.fine("Fetching blobber data.");
        return this.myBlobDataLock.take();
    }

//    protected void setSafetyMargin(long inMargin) {
//    	this.mySafetyMargin = inMargin;
//    }

	protected MonitorTestCollector getCollector() {
		return this.myCollector;
	}

//	protected void setCollector(MonitorTestCollector inCollector) {
//		this.myCollector = inCollector;
//	}

	public static class TestBlobberPlugin extends BlobberPlugin {
		private final MonitorDAO monitorDAO;
		public TestBlobberPlugin(ContainerServices containerServices, MonitorDAO monitorDAO) {
			super(containerServices);
			this.monitorDAO = monitorDAO;
		}
		@Override
		public void init() {
		}
		@Override
		public void cleanUp() {
		}
		@Override
		public List<MonitorDAO> getMonitorDAOs() {
			List<MonitorDAO> ret = new ArrayList<MonitorDAO>();
			ret.add(monitorDAO);
			return ret;
		}
		@Override
		public int getCollectorIntervalSec() {
			return 60; // TODO old BlobberImpl.COLLECT_INTERVAL_SEC 
		}
		@Override
		public boolean isProfilingEnabled() {
			return false;
		}
		@Override
		public BlobberWatchDog getBlobberWatchDog() {
			return null;
		}
	}
}
