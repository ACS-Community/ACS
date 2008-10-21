package alma.acs.daemontest;

import java.util.logging.Logger;

import alma.ACSErr.Completion;
import alma.acsdaemon.DaemonCallbackPOA;

public class DaemonCallbackImpl extends DaemonCallbackPOA
{
	private final Logger logger;

	DaemonCallbackImpl(Logger logger) {
		this.logger = logger;
	}
	
	public void done(Completion comp) {
		logger.info("done: comp=" + comp.timeStamp);
	}

	public void working(Completion comp) {
		logger.info("working: comp=" + comp.timeStamp);
	}
}
