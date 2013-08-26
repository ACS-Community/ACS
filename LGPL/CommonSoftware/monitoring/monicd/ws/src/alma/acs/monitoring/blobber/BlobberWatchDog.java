package alma.acs.monitoring.blobber;


import java.util.Collection;

import alma.acs.monitoring.DAO.ComponentData;

/**
 * The watchdog gets used by the DAO objects provided in {@link BlobberPlugin#createMonitorDAOs()}
 * and can also be used by the ACS layers of the blobber. That's why we define it here.
 * <p>
 * It provides JMX data about the queues and other information.
 */
public interface BlobberWatchDog
{
	/**
	 * Adds a queue that should be watched by this watch dog. 
	 * For the watchdog purposes, we don't need the specific queue methods, so that any Collection can be monitored;
	 * however, the name "queue" sounds better also here. 
	 * @param queue  The collection / queue to be monitored.
	 * @param queueName  A name for the queue, used to select a queue and for reporting.
	 * @param maxQueueSize  The maximum possible or allowed queue size, used to evaluate how full the queue is.
	 */
	public void addQueueToWatch(Collection<ComponentData> queue, String queueName, int maxQueueSize);
	
	/**
	 * Removes a queue that should no longer be watched.
	 * @param queueName
	 */
	public void removeQueueToWatch(String queueName);

	/**
	 * Returns the size of the queue with the given name.
	 * @param queueName  Name of the queue.
	 * @return size, as in number of entries.
	 */
	public long getQueueSize(String queueName);
	
}

