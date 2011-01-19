package alma.acs.monitoring.blobber;

import java.sql.Timestamp;
import java.util.List;
import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

import alma.TMCDB.MonitorCollector;
import alma.acs.monitoring.DAO.ComponentData;
import alma.acs.monitoring.DAO.ComponentStatistics;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.archive.tmcdb.DAO.MonitorDAOImpl;
import alma.acs.monitoring.blobber.CollectorList.BlobData;

public class TestBlobberWorker extends BlobberWorker implements MonitorDAO {

    private MonitorTestCollector myCollector;

    private Long mySafetyMargin = 0L;

    private Boolean myCanHandle = true;

    private DataLock<ComponentData> myBlobDataLock = new DataLock<ComponentData>();

    private MonitorDAO myDao;

    private static boolean useDatabase;

    public TestBlobberWorker() {
        super(null);
    }

    public static void setUseDatabase(boolean inUseDatabase) {
        useDatabase = inUseDatabase;
    }

    @Override
    protected void initWorker() {
        this.myLogger = Logger.getLogger("alma.acs.monitoring.blobber");

        // get the top Logger:
        Logger topLogger = java.util.logging.Logger.getLogger("");
        // Handler for console (reuse it if it already exists)
        Handler consoleHandler = null;
        // see if there is already a console handler
        for (Handler handler : topLogger.getHandlers()) {
            if (handler instanceof ConsoleHandler) {
                // found the console handler
                consoleHandler = handler;
                break;
            }
        }
        if (consoleHandler == null) {
            // there was no console handler found, create a new one
            consoleHandler = new ConsoleHandler();
            topLogger.addHandler(consoleHandler);
        }
        // set the console handler to finest
        consoleHandler.setLevel(java.util.logging.Level.FINEST);

        this.myLogger.setLevel(Level.FINE);
        this.myCollector = new MonitorTestCollector();
    }

    @Override
    protected void initMonitorDAO() {
        this.myMonitorDAO = this;
        if (useDatabase) {
            super.initMonitorDAO();
        }
    }

    @Override
    public void openTransactionStore() {
    }

    @Override
    public void closeTransactionStore() {
    }
    
    @Override
    protected MonitorCollector getMonitorCollector(String inCollectorName) {
        return this.myCollector;
    }

    @Override
    protected boolean canHandle() {
        synchronized (this.myCanHandle) {
            return this.myCanHandle;
        }
    }

    protected void setCanHandle(boolean inValue) {
        synchronized (this.myCanHandle) {
            this.myCanHandle = inValue;
        }
    }

    @Override
    protected void setCollectInterval(long inInterval) {
        synchronized (this.myCollectInterval) {
            this.myCollectInterval = inInterval;
        }
    }

    @Override
    public void store(ComponentData inData) throws Exception {
        this.myLogger.fine("Storing blobber data.");
        this.myBlobDataLock.put(cloneData(inData));
        if (useDatabase) {
            if (this.myDao == null) {
                this.myDao = new MonitorDAOImpl(this.myLogger);
            }
            this.myDao.store(inData);
        }
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

    protected ComponentData fetchData() {
        this.myLogger.fine("Fetching blobber data.");
        return this.myBlobDataLock.take();
    }

    protected void setSafetyMargin(long inMargin) {
        synchronized (this.mySafetyMargin) {
            this.mySafetyMargin = inMargin;
        }
    }

    protected MonitorTestCollector getCollector() {
        synchronized (this.myCollector) {
            return this.myCollector;
        }
    }

    protected void setCollector(MonitorTestCollector inCollector) {
        synchronized (this.myCollector) {
            this.myCollector = inCollector;
        }
    }

    @Override
    public List getMonitorData(long arg0, Timestamp arg1, Timestamp arg2) {
        /*
         * Method not used. Forced implementation from interface.
         */
        return null;
    }

    @Override
    public void close() {
        /*
         * Method not used. Forced implementation from interface.
         */
    }

}
