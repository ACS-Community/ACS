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
package com.cosylab.logging.engine;

import com.cosylab.logging.engine.log.LogEntryXML;

/**
 * This class defines the interface for classes (LCEngine, LCRemoteResponseCallbackBean) 
 * that support custom callbacks for receiving entries 
 * submitted to the logging mechanism and for processing them.
 */
public interface RemoteResponseCallback {
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
void invocationDestroyed();
/**
 */
void logEntryReceived(LogEntryXML logEntry);
	public void reportStatus(String status);
}
