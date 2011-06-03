package alma.acs.monitoring.blobber;

import java.util.logging.Logger;
import java.util.concurrent.LinkedBlockingQueue;

import java.lang.management.*;
import javax.management.*;

import alma.acs.container.ContainerServices;
import alma.acs.monitoring.blobber.CollectorList.BlobData;
public class BlobberQueueWatchDog implements Runnable {

    private ContainerServices myContainerServices;
    protected Logger myLogger;
    private LinkedBlockingQueue<BlobData> myBlobDataQueue;
    private LinkedBlockingQueue<BlobData> myMQDataQueue;

    //JMX 
    public MBeanServer server = null;
    //private TMCStatistic CLOBProcessTime = null;
    //private TMCStatistic CLOBSize = null;
    //private TMCStatistic diskWriteTime = null;
    //private TMCProducerStats producerStats = null;
    //private Hashtable statList = null;

    public BlobberQueueWatchDog(ContainerServices inContainerServices,
				LinkedBlockingQueue<BlobData> inBlobDataQueue,
				LinkedBlockingQueue<BlobData> inMQDataQueue) {
        myContainerServices = inContainerServices;
    	myBlobDataQueue = inBlobDataQueue;
	myMQDataQueue = inMQDataQueue;
        myLogger = myContainerServices.getLogger();
	//this.statList = new Hashtable();
    }

    public long getBlobDataQueueSize() {
	return myBlobDataQueue.size();
    }

    public long getMQDataQueueSize() {
	return myMQDataQueue.size();
    }

    @Override
    public void run() {
        this.myLogger.info("Starting Queue Watch Dog Thread.");
        Thread.currentThread().setName("BlobberQueueWatchDogThread");

	String componentName = myContainerServices.getName();

	//JMX registration
	this.server = ManagementFactory.getPlatformMBeanServer();
	QueueWatcher qBean = new QueueWatcher(this);
	ObjectName qBeanName = null;

	try {
	    qBeanName = new ObjectName("alma.archive.tmcdb.monitor.BlobberQueueWatchDog:type=QueueWatcher,name=" + componentName);
	    this.server.registerMBean(qBean, qBeanName);
	    System.out.println("BlobberQueueWatchDog registered for component: " + componentName);
	} catch (Exception e) {
	    e.printStackTrace();
	}

	while(true){
	    if (myBlobDataQueue.remainingCapacity() < 1000) {
		this.myLogger.warning("Blob Data Queue's capacity less than 1000, claring it.");
		myBlobDataQueue.clear();
	    }
	    if (myMQDataQueue.remainingCapacity() < 1000) {
		this.myLogger.warning("MQ Data Queue's capacity less than 1000, claring it.");
		myMQDataQueue.clear();
	    }
	    try {
		// Sleep for one second
		Thread.sleep(1000);
	    } catch (InterruptedException ex) {
		ex.printStackTrace();
	    }
	}
    }
}
