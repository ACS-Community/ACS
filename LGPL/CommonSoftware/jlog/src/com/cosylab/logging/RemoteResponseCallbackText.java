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

import com.cosylab.logging.engine.*;
/**
 * This class implements RemoteResponseCallback and its methods logEntryReceived and reportStatus.
 * Creation date: (11/2/2001 3:21:42 PM)
 * @author: 
 */
public class RemoteResponseCallbackText implements RemoteResponseCallback {
/**
 * RemoteResponeCallbackText constructor comment.
 */
public RemoteResponseCallbackText() {
	super();
}
/**
 * A message identifying the destruction of the remote process
 * that delivers the responses for the given invocation.
 * This message is delivered, if: 1. server single-sidedly
 * closes the data delivery and reports this to the client, 2.
 * if the client destroys the data delivery and server does ACK,
 * 3. if server does not support ACKs, the engine must send this
 * when it determines that the data delivery is broken or timeouts.
 *
 */
public void invocationDestroyed() {}
/**
 */
public void logEntryReceived(LogEntry logEntry) {
	System.out.println(logEntry);
}
/**
 * reportStatus method comment.
 */
public void reportStatus(java.lang.String status) {
	System.out.println("Status report: " + status);
}
}
