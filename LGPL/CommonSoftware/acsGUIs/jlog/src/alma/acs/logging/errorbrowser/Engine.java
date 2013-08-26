/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2002, All rights reserved
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
package alma.acs.logging.errorbrowser;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

import alma.acs.logging.table.LogEntryTableModelBase;

/**
 * The error browser engine is the object in charge of extracting all the logs
 * of an error trace.
 * 
 * @author acaproni
 *
 */
public class Engine extends Thread {
	
	/**
	 * The table model to read logs part of the stack trace
	 */
	private final LogEntryTableModelBase srcModel;
	
	/**
	 * The table model to push logs part of the stack trace in
	 */
	private final LogEntryTableModelBase dstModel;
	
	/**
	 * The stackId of the logs to show in the error trace
	 */
	private final String stackId;
	
	/**
	 * Signal the thread to terminate
	 */
	private volatile boolean terminateThread;
	
	/**
	 * Constructor
	 * 
	 * @param stackID The ID of the logs of the error trace
	 * @param model The table model to read logs from
	 * @param listener The model to push logs into
	 */
	public Engine(LogEntryTableModelBase model, String stackID, LogEntryTableModelBase listener) {
		if (model==null) {
			throw new IllegalArgumentException("The source model can't be null");
		}
		this.srcModel=model;
		
		if (listener==null) {
			throw new IllegalArgumentException("The destination model can't be null");
		}
		this.dstModel=listener;
		
		if (stackID==null || stackID.isEmpty()) { 
			throw new IllegalArgumentException("The STACK ID can't be null nor empty");	
		}
		this.stackId=stackID;
		
		// Start the thread to get the logs of the error trace
		terminateThread=false;
		Thread t = new Thread(this);
		t.setName("ErrorBrowserEngine");
		t.setDaemon(true);
		t.start();
	}
	
	/**
	 * Stop the thread
	 */
	public void close() {
		terminateThread=true;
	}

	/**
	 * The thread to get all the logs of the error trace
	 */
	@Override
	public void run() {
		for (int t=0; t<srcModel.totalLogNumber() &&!terminateThread; t++) {
			ILogEntry log = srcModel.getVisibleLogEntry(t);
			if (log==null) {
				continue;
			}
			if (stackId.equals((String)log.getField(LogField.STACKID))) {
				dstModel.appendLog(log);
			}
		}
		
	}
	
}
