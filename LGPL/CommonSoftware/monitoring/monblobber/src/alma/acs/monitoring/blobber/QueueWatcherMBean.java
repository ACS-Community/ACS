package alma.acs.monitoring.blobber;

public interface QueueWatcherMBean {
	//Attributes
	public long  getMQDataQueueSize();
	public long  getBlobDataQueueSize();

	// Operations 
}
