package alma.acs.monitoring.blobber;


import java.util.concurrent.LinkedBlockingQueue;

/**
 * The watchdog gets used by the DAO objects provided in {@link BlobberPlugin#createMonitorDAOs()}
 * and can also be used by the ACS layers of the blobber. That's why we define it here.
 * <p>
 * It provides JMX data about the queues and other information.
 */
public interface BlobberWatchDog extends Runnable
{
	public void addQueueToWatch(LinkedBlockingQueue queue, String queuName);
	public void removeQueueToWatch(String queuName);

	public long  getQueueSize(String queueName);
}

