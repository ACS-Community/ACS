/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2010
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston, 
 *    MA 02111-1307  USA
 */
package alma.acs.jlog.test;

import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSRemoteRawLogListener;
import com.cosylab.logging.engine.ACS.LCEngine;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * Objects of this class connects to the logging NC with an
 * instance of LCEngine and gets logs.
 * The logs received are discarded i.e. this class does not add
 * any computation/storage to what the jlog engine does so that 
 * we can profile the engine classes launching this log consumer.
 * <P>
 * The process never ends so the user has to press CTRL+C
 * to terminate it.
 * 
 * @author acaproni
 *
 */
public class LogEngineProfiler implements 
ACSRemoteLogListener, ACSRemoteRawLogListener {
	
	/**
	 * The engine to connect to logging NC
	 */
	private final LCEngine engine;
	
	/**
	 * The object to wait for terminating the process
	 */
	private final Object mutex = new Object();
	
	/**
	 * Constructor
	 */
	public LogEngineProfiler() {
		engine = new LCEngine();
		engine.addRawLogListener(this);
		engine.addLogListener(this);
		engine.connect();
	}
	
	/**
	 * Wait until the user presses CTRL+C.
	 */
	public void execute() {
		synchronized (mutex) {
			try {
				mutex.wait();
			} catch (InterruptedException ie) {}
		}
	}

	public static void main(String[] args) {
		LogEngineProfiler logProfiler = new LogEngineProfiler();
		logProfiler.execute();
	}
	
	/**
	 * Closes the engine when the user presses CTRL+C
	 */
	public void addShutdownHook() {
		Runtime rt= Runtime.getRuntime();
		rt.addShutdownHook(new Thread() {
			public void run() {
				System.out.println("HALT invoked");
				synchronized (mutex) {
					mutex.notify();
				}
				if (engine==null) {
					return;
				}
				if (engine.isConnected()) {
					engine.close(true);
				}
			}
		});
	}

	@Override
	public void xmlEntryReceived(String xmlLogString) {
		// Does nothing!
		
	}

	@Override
	public void logEntryReceived(ILogEntry logEntry) {
		// Does nothing!
		
	}
}
