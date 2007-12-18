package alma.acs.jlog.test;

import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.Field;

public class LogListenerStressTest implements ACSRemoteLogListener, ACSRemoteRawLogListener{
	
	private int xmlReceived=0;
	private int logReceived=0;
	
	private int[] logTypeReceived=new int[LogTypeHelper.values().length];
	
	public Object done = new Object();
	
	//	 The engine that connects to the logging client
	private LCEngine engine;
	
	boolean msgPrinted = false;
	
	public LogListenerStressTest() {
		engine = new LCEngine();
		engine.addLogListener(this);
		engine.addRawLogListener(this);
		engine.connect();
	}
	/**
	 * Print only the messages received from the client
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener
	 */
	public void xmlEntryReceived(String str) {
		if (str.indexOf("logClient.cpp")>=0) {
			xmlReceived++;
			if (str.indexOf("Done")>=0) {
				System.out.println("xmlEntryReceived => Done received");
			}
		}
	}
	
	/**
	 * Print only the messages received from the client
	 * 
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener
	 */
	public void logEntryReceived(ILogEntry log) {
		if (log.getField(Field.FILE).toString().indexOf("logClient.cpp")>=0) {
			logReceived++;
			Integer logType = ((LogTypeHelper)log.getField(Field.ENTRYTYPE)).ordinal();
			logTypeReceived[logType]++;
			if (log.getField(Field.LOGMESSAGE).toString().indexOf("Done")>=0) {
				System.out.println("logEntryReceived => Done received");
				printNums();
			}
		}
	}
	
	public void printNums() {
		msgPrinted = true;
		System.out.println("XML entries received: "+xmlReceived);
		System.out.println("Log entries received: "+xmlReceived);
		for (int t=0; t<logTypeReceived.length; t++) {
			System.out.print("Num. of received logs of type "+LogTypeHelper.values()[t].logEntryType);
			System.out.println(": "+logTypeReceived[t]);
		}
		synchronized(this) {
			notifyAll();
		}
	}
	
	public void finalize() { 
		if (!msgPrinted) {
			System.out.println("Forced to print summary");
			printNums();
		}
	}
	
	/**
	 * Disconnect from the logging channel
	 *
	 */
	public void disconnet() {
		engine.disconnect();
	}
	
	/**
	 * Main
	 * 
	 * @param args
	 */
	public static void main(String[] args) {
		LogListenerStressTest ll = new LogListenerStressTest();
		synchronized(ll.done) {
			try {
				ll.done.wait();
			} catch (InterruptedException ie) {}
		} 
		ll.disconnet();
		System.out.println("Exiting");
	}
}
