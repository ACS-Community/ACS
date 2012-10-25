package alma.acs.nc.testsupport;

import java.util.concurrent.atomic.AtomicLong;
import java.util.logging.Logger;

import alma.ACSErrTypeCommon.wrappers.AcsJIllegalStateEventEx;
import alma.acs.exceptions.AcsJException;
import alma.acs.nc.AcsEventPublisher;
import alma.acs.util.UTCUtility;
import alma.acsnc.EventDescription;

/**
 * @param <T> See {@link AcsEventPublisher}.
 * @see InMemoryNcFake
 */
class InMemoryPublisher<T> implements AcsEventPublisher<T>
{
	private final InMemoryNcFake nc;
	private final Logger logger;
	protected final String publisherName;
	
	private final AtomicLong count = new AtomicLong(0);

	InMemoryPublisher(InMemoryNcFake nc, String publisherName, Logger logger) {
		this.nc = nc;
		this.publisherName = publisherName;
		this.logger = logger;
	}
	
	/**
	 * {@inheritDoc}
	 * <p>
	 * This method is thread-safe.
	 */
	@Override
	public void publishEvent(T customStruct) throws AcsJException {
		long currentOmgTime = UTCUtility.utcJavaToOmg(System.currentTimeMillis());
		EventDescription desc = new EventDescription(publisherName, currentOmgTime, count.getAndIncrement());
		nc.pushData(customStruct, desc);
	}

	@Override
	public void disconnect() throws AcsJIllegalStateEventEx {
		nc.disconnectPublisher(this);
	}

	/**
	 * @throws UnsupportedOperationException because it's not (yet) supported; please report if you need it.
	 */
	@Override
	public void enableEventQueue(int queueSize, EventProcessingHandler<T> handler) {
		throw new UnsupportedOperationException("Event queue handling is currently not supported for in-memory fake NCs. See http://jira.alma.cl/browse/COMP-2890.");
	}
	
	/**
	 * Returns the total number of events published. 
	 * Should be used only for testing.
	 */
	public long getEventCount() {
		return count.get();
	}
}
