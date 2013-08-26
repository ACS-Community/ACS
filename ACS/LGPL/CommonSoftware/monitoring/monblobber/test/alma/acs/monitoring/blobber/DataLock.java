package alma.acs.monitoring.blobber;

import java.util.concurrent.ArrayBlockingQueue;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

/**
 * A "stack" of size one for a given data item: First the data must be stored (in method {@link #put(Object)}), then it
 * can alternately be popped (method {@link #take()}) and pushed again. The pushing and popping get blocked until the
 * stack is free or full respectively.
 */
public class DataLock<C>
{
	private final BlockingQueue<C> delegate = new ArrayBlockingQueue<C>(1);
	
	/**
	 * Timeout in seconds. Used to make tests fail rather than hang. 
	 */
	public static final int timeoutSec = 100;

	private final Logger logger;
	private final String name;


	/**
	 * @param logger
	 * @param name Used for debug logs
	 */
	public DataLock(Logger logger, String name) {
		this.logger = logger;
		this.name = name;
	}

	/**
	 * Stores the given data as soon as previously stored data gets taken.
	 * @param inData The data.
	 * @throws InterruptedException If interrupted (e.g. by timeout) while blocking.
	 */
	public void put(C inData) throws InterruptedException {
		logger.finer("DataLock " + name + ": Got a call to put(" + inData.getClass().getSimpleName() + "); thread = "
				+ Thread.currentThread().getName() + "; lockReady=" + (delegate.size() == 0));

		if (delegate.offer(inData, timeoutSec, TimeUnit.SECONDS)) {
			logger.finer("DataLock " + name + ": Returning from put(" + inData.getClass().getSimpleName() + ").");
		} else {
			logger.warning("DataLock " + name + ": Timed out in put(" + inData.getClass().getSimpleName() + ").");
			throw new InterruptedException("Storing data was interrupted by timeout=" + timeoutSec + " s.");
		}
	}

	/**
	 * Retrieves data, as soon as data becomes available.
	 * @return The data.
	 * @throws InterruptedException If the call gets interrupted while waiting for data, e.g. timeout.
	 */
	public C take() throws InterruptedException {
		logger.finer("DataLock " + name + ": Got a call to take(); thread = " + Thread.currentThread().getName()
				+ "; lockReady=" + (delegate.size() > 0));

		C data = delegate.poll(timeoutSec, TimeUnit.SECONDS);
		if (data != null) {
			logger.finer("DataLock " + name + ": Returning from take().");
			return data;
		} else {
			logger.warning("DataLock " + name + ": Timed out in take().");
			throw new InterruptedException("Fetching data was interrupted by timeout=" + timeoutSec + " s.");
		}
	}
}
