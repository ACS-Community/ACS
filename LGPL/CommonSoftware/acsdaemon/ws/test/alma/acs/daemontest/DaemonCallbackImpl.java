package alma.acs.daemontest;

import java.util.Date;
import java.util.concurrent.CountDownLatch;
import java.util.concurrent.TimeUnit;
import java.util.logging.Logger;

import alma.ACSErr.Completion;
import alma.acs.util.IsoDateFormat;
import alma.acs.util.UTCUtility;
import alma.acsdaemon.DaemonCallbackPOA;

public class DaemonCallbackImpl extends DaemonCallbackPOA
{
	private final Logger logger;
	private volatile String srvName;
	private volatile CountDownLatch syncDone;
	private volatile Completion lastDoneCompletion;
	
	DaemonCallbackImpl(Logger logger) {
		this.logger = logger;
	}
	
	public void done(Completion comp) {
		lastDoneCompletion = comp;
		logger.info(srvName + " - DaemonCallbackImpl#done: comp.timeStamp=" + isoDateFromOmgTime(comp.timeStamp));
		if (syncDone != null) {
			syncDone.countDown();
		}
		srvName = null;
	}

	public void working(Completion comp) {
		logger.info(srvName + " - DaemonCallbackImpl#working: comp.timeStamp=" + isoDateFromOmgTime(comp.timeStamp));
	}
	
	/**
	 * Must be called before the call that can trigger the callback to {@link #done(Completion)} 
	 * and before {@link #waitForDone(long, TimeUnit)}, so as to (re-)activate waiting for the done callback.
	 * This ensures that we don't miss the done callback even if it occurs faster than the client thread can call waitForDone.
	 * @param srvName used for logging the working and done calls
	 */
	void prepareWaitForDone(String srvName) {
		syncDone = new CountDownLatch(1);
		this.srvName = srvName;
	}
	
	/**
	 * {@link #prepareWaitForDone()} must be called first.
	 * @return false if the waiting time elapsed before the done() method was called.
	 * @see CountDownLatch#await(long, TimeUnit)
	 */
	boolean waitForDone(long timeout, TimeUnit unit) throws InterruptedException {
		return syncDone.await(timeout, unit);
	}

	
	private String isoDateFromOmgTime(long omgTime) {
		return IsoDateFormat.formatDate(new Date(UTCUtility.utcOmgToJava(omgTime)));
	}

	Completion getLastDoneCompletion() {
		return lastDoneCompletion;
	}
}
