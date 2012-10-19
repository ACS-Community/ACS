package alma.acs.nc.testsupport;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.container.ContainerServicesBase;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.nc.AcsEventSubscriber;
import alma.acsnc.EventDescription;

/**
 * This class provides a simple in-memory notification mechanism
 * that tries to mimic a corba notification channel,
 * without the need for ACS services to run.
 * <p>
 * <b>It should be used only to simplify unit test setups, and never in an operational environment.</b>
 * <p>
 * Unit tests may use this class in two ways:
 * <ul>
 *   <li>Explicitly: The test can instantiate InMemoryNcFake and create the publisher
 *       and subscriber objects from there. This will work fine if the classes under test are designed
 *       to get the supplier/subscriber objects injected, rather than retrieving them directly 
 *       from the container services.
 *   <li>Implicitly: The test can wrap the container services (e.g. by extending jcont :: ContainerServicesProxy)
 *       so that methods {@link ContainerServicesBase#createNotificationChannelPublisher(String, Class)}
 *       and {@link ContainerServicesBase#createNotificationChannelSubscriber(String, Class)} 
 *       are overridden and use this InMemoryNcFake class. This should work also for cases where the container services
 *       get passed around the code, and deep down in some class under test the suppliers / subscribers are created.
 * </ul> 
 * In either case, the resulting supplier and subscriber objects will have the same interfaces as their real NC variants.
 * See also http://jira.alma.cl/browse/COMP-2890 about such tests.
 */
public class InMemoryNcFake 
{
	private final String channelName;
	private final Logger logger;
	private final ContainerServicesBase services;
	
	/**
	 * Not clear yet if it makes sense to keep track of the connected publishers.
	 * It may be interesting for some tests to get a list of publishers and subscribers
	 * in the end to check if some were not disconnected.
	 * At the moment we just keep track of the publishers without further using this information.
	 */
	private final List<InMemoryPublisher<?>> publishers;
	
	private final List<InMemorySubscriber<?>> subscribers;
	
	/**
	 * Lets multiple supplier threads send data concurrently to the subscribers
	 * but protects them from changes in the {@link #subscribers} list.
	 */
	private final ReentrantReadWriteLock subscribersLock;


	/**
	 * @param services
	 * @param channelName
	 */
	public InMemoryNcFake(ContainerServicesBase services, String channelName) {
		this.channelName = channelName;
		this.services = services;
		this.logger = services.getLogger();
		this.publishers = new ArrayList<InMemoryPublisher<?>>();
		this.subscribers = new ArrayList<InMemorySubscriber<?>>();
		subscribersLock = new ReentrantReadWriteLock(true);
	}
	
	/**
	 * Factory method for publishers.
	 */
	public synchronized <T> AcsEventPublisher<T> createPublisher(String publisherName, Class<T> eventType) {
		InMemoryPublisher<T> ret = new InMemoryPublisher<T>(this, publisherName, logger);
		publishers.add(ret);
		return ret;
	}
	
	/**
	 * Factory method for subscribers.
	 */
	public <T> AcsEventSubscriber<T> createSubscriber(String subscriberName, Class<T> eventType) throws AcsJException {

		InMemorySubscriber<T> ret = new InMemorySubscriber<T>(this, services, subscriberName, eventType);

		subscribersLock.writeLock().lock();
		try {
			subscribers.add(ret);
		} finally {
			subscribersLock.writeLock().unlock();
		}
		return ret;
	}

	/**
	 * Called by {@link InMemoryPublisher#disconnect()}
	 * when the user disconnects the publisher.
	 */
	void disconnectPublisher(InMemoryPublisher<?> publisher) {
		boolean done = publishers.remove(publisher);
		if (done) {
			logger.finer("Disconnected publisher " + publisher.publisherName);
		}
		else {
			logger.warning("Failed to disconnect publisher " + publisher.publisherName);
		}
	}

	/**
	 * Called by {@link InMemorySubscriber#destroyConnectionAction}
	 * when the user disconnects the subscriber.
	 */
	void disconnectSubscriber(InMemorySubscriber<?> subscriber) {
		subscribersLock.writeLock().lock();
		try {
			boolean done = subscribers.remove(subscriber);
			if (done) {
				logger.finer("Disconnected subscriber " + subscriber.getClientName());
			}
			else {
				logger.warning("Failed to disconnect subscriber " + subscriber.getClientName());
			}
		} finally {
			subscribersLock.writeLock().unlock();
		}
	}

	
	/**
	 * Called by the publishers to deliver their data.
	 * <p>
	 * This method dispatches (synchronously in the calling thread) the data to all subscribers. 
	 * This is OK because the subscribers implement buffers, so that even with slow subscribers
	 * this call should return fast.
	 * We do not filter by event type here because the in-memory subscriber is in charge of this.
	 * The subscriber may need all events if it has a generic subscription.
	 * <p>
	 * This method is thread-safe.
	 * @param data
	 * @param desc
	 */
	void pushData(Object data, EventDescription desc) {
		
		// 
		subscribersLock.readLock().lock();
		try {
			for (InMemorySubscriber<?> subscriber : subscribers) {
				try {
					subscriber.pushData(data, desc);
				} catch (AcsJIllegalStateEventEx ex) {
						// Could legally happen if startReceivingEvents was never called on the subscriber.
				}
			}
		}
		finally {
			subscribersLock.readLock().unlock();
		}
	}
	
	
	/**
	 * hashCode() based on {{@link #channelName}.
	 */
	@Override
	public int hashCode() {
		return channelName.hashCode();
	}

	/**
	 * equals based on {{@link #channelName}.
	 */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (!(obj instanceof InMemoryNcFake))
			return false;
		InMemoryNcFake other = (InMemoryNcFake) obj;
		if (!channelName.equals(other.channelName))
			return false;
		return true;
	}
}

