/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/
package alma.acs.jlog.test;

import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

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
		if (log.getField(LogField.FILE).toString().indexOf("logClient.cpp")>=0) {
			logReceived++;
			Integer logType = ((LogTypeHelper)log.getField(LogField.ENTRYTYPE)).ordinal();
			logTypeReceived[logType]++;
			if (log.getField(LogField.LOGMESSAGE).toString().indexOf("Done")>=0) {
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
