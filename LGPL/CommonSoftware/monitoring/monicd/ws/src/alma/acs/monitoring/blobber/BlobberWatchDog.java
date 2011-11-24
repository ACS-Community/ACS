package alma.acs.monitoring.blobber;

/**
 * The watchdog gets used by the DAO objects provided in {@link BlobberPlugin#createMonitorDAOs()}
 * and can also be used by the ACS layers of the blobber. That's why we define it here.
 * <p>
 * It provides JMX data about the queues and other information.
 */
public interface BlobberWatchDog
{
	public void matiasShouldCreateTheMethods();
}
