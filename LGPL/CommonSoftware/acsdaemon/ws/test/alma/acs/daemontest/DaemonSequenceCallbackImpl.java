package alma.acs.daemontest;

import java.util.logging.Logger;

import alma.ACSErr.Completion;
import alma.acsdaemon.DaemonSequenceCallbackPOA;

public class DaemonSequenceCallbackImpl extends DaemonSequenceCallbackPOA
{
	private final Logger logger;

	DaemonSequenceCallbackImpl(Logger logger) {
		this.logger = logger;
	}
	
	public void done(Completion comp) {
		logger.info("done: comp=" + comp.timeStamp);
	}

	public void working(String service, String host, short instance_number, Completion comp) {
		logger.info("working: service=" + service + " host=" + host + " instance=" + instance_number + " comp=" + comp.timeStamp);
	}
}
