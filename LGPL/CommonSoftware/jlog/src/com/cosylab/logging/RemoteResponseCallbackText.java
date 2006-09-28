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
package com.cosylab.logging;

import com.cosylab.logging.engine.ACS.ACSRemoteLogListener;
import com.cosylab.logging.engine.ACS.ACSLogConnectionListener;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * This class implements ACSRemoteLogListener and its methods logEntryReceived and reportStatus.
 * Creation date: (11/2/2001 3:21:42 PM)
 * @author: 
 */
public class RemoteResponseCallbackText implements ACSRemoteLogListener, ACSLogConnectionListener {
/**
	 * RemoteResponeCallbackText constructor comment.
	 */
	public RemoteResponseCallbackText() {
		super();
	}
	
	/**
	 */
	public void logEntryReceived(ILogEntry logEntry) {
		System.out.println(logEntry);
	}
	
	/**
	 * reportStatus method comment.
	 */
	public void reportStatus(java.lang.String status) {
		System.out.println("Status report: " + status);
	}
	
	/*
	 *  (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#acsLogConnEstablished()
	 */
	public void acsLogConnEstablished() {
		System.out.println("Connection established");
	}
	
	/*
	 *  (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#acsLogConnLost()
	 */
	public void acsLogConnLost() {
		System.out.println("Connection lost");
	}
	
	/*
	 *  (non-Javadoc)
	 * @see com.cosylab.logging.engine.ACS.ACSRemoteLogListener#acsLogConnConnecting()
	 */
	public void acsLogConnConnecting() {
		System.out.println("Connecting");
	}
	
	public void acsLogConnDisconnected() {
		System.out.println("Disconnected");
	}
	
	public void acsLogConnSuspended() {
		System.out.println("Suspended");
	}
	
	public void acsLogsDelay() {
		System.out.println("Discarding");
	}
}
