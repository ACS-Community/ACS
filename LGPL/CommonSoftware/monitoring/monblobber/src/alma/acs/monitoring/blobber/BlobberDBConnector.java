package alma.acs.monitoring.blobber;

import java.lang.InterruptedException;
import java.lang.reflect.Constructor;

import alma.ACSErrTypeCommon.wrappers.AcsJCouldntCreateObjectEx;
import alma.JavaContainerError.wrappers.AcsJContainerServicesEx;

import java.util.logging.Logger;
import java.util.concurrent.LinkedBlockingQueue;

//import alma.archive.tmcdb.DAO.MonitorDAO;
//import alma.archive.tmcdb.DAO.MonitorDAOImpl;
import alma.acs.monitoring.DAO.MonitorDAO;
import alma.acs.container.ContainerServices;
import alma.acs.monitoring.blobber.CollectorList.BlobData;
import alma.DAOErrType.wrappers.AcsJDBConnectionFailureEx;

public class BlobberDBConnector implements Runnable {

    private ContainerServices myContainerServices;
    protected Logger myLogger;
    protected MonitorDAO myMonitorDAO;
    private LinkedBlockingQueue<BlobData> myBlobDataQueue;
    private int insertCounter = 0;
    private BlobberPlugin blobberPlugin = null;

    public BlobberDBConnector(ContainerServices inContainerServices, 
			      LinkedBlockingQueue<BlobData> inBlobDataQueue) {
        this.myContainerServices = inContainerServices;
	this.myBlobDataQueue = inBlobDataQueue;
        this.myLogger = myContainerServices.getLogger();
        initMonitorDAO();
    }

    protected void initMonitorDAO() {
        myLogger.fine("Initializing MonitorDAO.");
        try {
            blobberPlugin = createBlobberPlugin();
        } catch (AcsJCouldntCreateObjectEx ex) {
            try {
                myContainerServices.raiseAlarm("Monitoring", "MonitorArchiver", 2);
            } catch (AcsJContainerServicesEx ex1) {
                myLogger.severe("Blobber initialization failed and alarm could not be raised.");
                // fall through to ComponentLifecycleException
            }
        } 
        try {
            myMonitorDAO = blobberPlugin.createMonitorDAO(); 
        } catch (AcsJCouldntCreateObjectEx ex) {
                myLogger.severe("DAO initialization failed");
        }
    }

    protected BlobberPlugin createBlobberPlugin() throws AcsJCouldntCreateObjectEx {
        try {
            Class<? extends BlobberPlugin> pluginClass = Class.forName("alma.acs.monitoring.blobber.BlobberPluginAlmaImpl").asSubclass(BlobberPlugin.class);
            Constructor<? extends BlobberPlugin> ctor = pluginClass.getConstructor(Logger.class);
            return ctor.newInstance(myLogger);
        } catch (Exception ex) {
            AcsJCouldntCreateObjectEx ex2 = new AcsJCouldntCreateObjectEx(ex);
            throw ex2;
        }
    }

    @Override
    public void run() {
        this.myLogger.info("Starting DB connector thread.");
        Thread.currentThread().setName("BlobberDBConnectorThread");
	try {
	    this.myMonitorDAO.openTransactionStore();
	} catch (Exception ex) {
	    ex.printStackTrace();
	}
	while(true){
	    insertCounter++;
	    if (insertCounter%10000 == 0){
		try {
		    this.myMonitorDAO.closeTransactionStore();
		    this.myMonitorDAO.openTransactionStore();
		} catch (Exception ex) {
		    ex.printStackTrace();
		}
		insertCounter = 0;
	    } 
	    BlobData tempBlobData = new BlobData();
	    try {
		tempBlobData = this.myBlobDataQueue.take();
	    } catch (InterruptedException ex) {
		ex.printStackTrace();
	    }
	    try {
		this.myMonitorDAO.store(tempBlobData);
	    } catch (Exception ex) {
		ex.printStackTrace();
	    }
	}
    }
}

