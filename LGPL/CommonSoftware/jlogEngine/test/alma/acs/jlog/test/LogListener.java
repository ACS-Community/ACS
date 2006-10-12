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
			Thread.sleep(90000);
		} catch (InterruptedException ie) {}
		ll.disconnet();
		System.exit(0);
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
	 * Print only the messages received from the client
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener
	 */
	public void xmlEntryReceived(String str) {
		if (str.indexOf("logClient.cpp")>=0) {
			System.out.println("RAW log: "+str);
		}
	}
	
	/**
	 * Print only the messages received from the client
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener
	 */
	public void logEntryReceived(ILogEntry log) {
		if (log.getField(ILogEntry.FIELD_FILE).toString().compareTo("logClient.cpp")==0) {
			System.out.println("Log received: "+log.toString());
		}
	}
	
	/**
	 * Disconnect from the logging channel
	 *
	 */
	public void disconnet() {
		engine.disconnect();
	}
}
