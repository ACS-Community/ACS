package alma.acs.daemontest;

import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.ACSErr.Completion;
import alma.acsdaemon.DaemonSequenceCallbackPOA;

public class DaemonSequenceCallbackImpl extends DaemonSequenceCallbackPOA
{
	private final Logger logger;
	
	private volatile CountDownLatch syncDone;
	private volatile Completion lastDoneCompletion;

	DaemonSequenceCallbackImpl(Logger logger) {
		this.logger = logger;
	}
	
	public void done(Completion comp) {
		logger.info("done: comp=" + comp.timeStamp+", comp.code="+comp.code);
		if (syncDone != null) {
			syncDone.countDown();
		}
	}

	public void working(String service, String host, short instance_number, Completion comp) {
		logger.info("working: service=" + service + " host=" + host + " instance=" + instance_number + " comp=" + comp.timeStamp+", comp.code="+comp.code);
	}
	
	/**
	 * Must be called before the call that can trigger the callback to {@link #done(Completion)} 
	 * and before {@link #waitForDone(long, TimeUnit)}, so as to (re-)activate waiting for the done callback.
	 * This ensures that we don't miss the done callback even if it occurs faster than the client thread can call waitForDone.
	 */
	public void prepareWaitForDone() {
		syncDone = new CountDownLatch(1);
	}
	
	/**
	 * {@link #prepareWaitForDone()} must be called first.
	 * @return false if the waiting time elapsed before the done() method was called.
	 * @see CountDownLatch#await(long, TimeUnit)
	 */
	public boolean waitForDone(long timeout, TimeUnit unit) throws InterruptedException {
		return syncDone.await(timeout, unit);
	}
}
