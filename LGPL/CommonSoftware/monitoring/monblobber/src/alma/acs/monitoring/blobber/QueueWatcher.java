package alma.acs.monitoring.blobber;

public class QueueWatcher implements QueueWatcherMBean {

	private BlobberQueueWatchDog watchDog;

	public QueueWatcher(BlobberQueueWatchDog inWatchDog) {
		this.watchDog = inWatchDog;
	}

	public void resetStatistics() {
	}

	public long getBlobDataQueueSize() {
		return watchDog.getBlobDataQueueSize();
	}

	public long getMQDataQueueSize() {
		return watchDog.getMQDataQueueSize();
	}

}
