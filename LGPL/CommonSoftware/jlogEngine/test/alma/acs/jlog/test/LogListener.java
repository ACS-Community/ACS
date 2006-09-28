package alma.acs.jlog.test;

import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.ACS.ACSLogConnectionListener;
import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * A class to test the receptions of the logs through the listeners.
 * The class instantiate a LCEngine and register itself as listeners.
 * The logs and messages received are printed in the stdout
 * 
 * @author acaproni
 *
 */
public class LogListener implements 
	ACSLogConnectionListener, 
	ACSRemoteLogListener, 
	ACSRemoteRawLogListener {

	// The engine that connects to the logging client
	private LCEngine engine;
	
	/**
	 * The constructor
	 *
	 */
	public LogListener() {
		engine = new LCEngine();
		engine.addLogConnectionListener(this);
		engine.addLogListener(this);
		engine.addRawLogListener(this);
		engine.connect();
	}
	
	/**
	 * Main
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		LogListener ll = new LogListener();
		try {
			Thread.sleep(120000);
		} catch (InterruptedException ie) {}
	}
	
	public void acsLogConnEstablished() {
		System.out.println("Connection established");
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener
	 */
	public void acsLogConnDisconnected() {
		System.out.println("Connection disconnected");
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener
	 */
	public void acsLogConnLost() {
		System.out.println("Connection lost");
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener
	 */
	public void acsLogConnConnecting() {
		System.out.println("Connecting");
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener
	 */
	public void acsLogConnSuspended() {
		System.out.println("Connection suspended");
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener
	 */
	public void acsLogsDelay() {
		System.out.println("Delay detected");
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSLogConnectionListener
	 */
	public void reportStatus(String status) {
		System.out.println("Status msg received: "+status);
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener
	 */
	public void xmlEntryReceived(String str) {
		System.out.println("RAW log: "+str);
	}
	
	/**
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener
	 */
	public void logEntryReceived(ILogEntry log) {
		System.out.println("Log recived: "+log.toString());
	}
}
