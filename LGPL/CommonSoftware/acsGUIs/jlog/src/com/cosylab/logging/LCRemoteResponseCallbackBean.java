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

/**
 * This class implements RemoteResponseCallback as a bean and its methods logEntryReceived 
 * and reportStatus. It support custom callbacks for receiving entries submitted to 
 * the logging mechanism. Used in LoggingClient. 
 * Creation date: (1/22/02 10:22:42 AM)
 * @author: 
 */
public class LCRemoteResponseCallbackBean implements com.cosylab.logging.engine.RemoteResponseCallback {
//	private javax.swing.JTextArea textArea = null;
//	private LogTableDataModel ltdm = null;
	private javax.swing.JTextArea textArea = null;
	private LogEntryTable logEntryTable;
/**
 * LCRemoteResponseCallback constructor comment.
 */
public LCRemoteResponseCallbackBean() {
	super();
	/*	lce = new com.cosylab.logging.LCEngine(accessType);
	lce.setRemoteResponseCallback(this);
	isConnected = true;
	lce.connect();
	this.ltdm = ltdm;
	this.textArea = jta;*/
}
/**
 * Gets the corresponding LogEntryTable.
 * Creation date: (2/8/2002 2:21:44 PM)
 * @return com.cosylab.logging.LogEntryTable
 */
public LogEntryTable getLogEntryTable() {
	return logEntryTable;
}
/**
 * Insert the method's description here.
 * Creation date: (2/8/2002 2:21:17 PM)
 * @return javax.swing.JTextArea
 */
public javax.swing.JTextArea getTextArea() {
	return textArea;
}
/**
 * A message identifying the destruction of the remote process
 * that delivers the responses for the given invocation.
 * This message is delivered, if: 
 * 1. server single-sidedly closes the data delivery and reports this to the client, 
 * 2. if the client destroys the data delivery and server does ACK,
 * 3. if server does not support ACKs, the engine must send this
 * when it determines that the data delivery is broken or timeouts.
 *
 */
public void invocationDestroyed() {}
/**
 */
public void logEntryReceived(com.cosylab.logging.engine.log.LogEntryXML logEntry) {
	logEntryTable.getLCModel().appendLog(logEntry);
}
/**
 * reportStatus method comment.
 */
public void reportStatus(String status) {
	textArea.append(status+"\n");
}
/**
 * Sets LogEntryTable.
 * Creation date: (2/8/2002 2:21:44 PM)
 * @param newLogEntryTable com.cosylab.logging.LogEntryTable
 */
public void setLogEntryTable(LogEntryTable newLogEntryTable) {
	logEntryTable = newLogEntryTable;
}
/**
 * Insert the method's description here.
 * Creation date: (2/8/2002 2:21:17 PM)
 * @param newTextArea javax.swing.JTextArea
 */
public void setTextArea(javax.swing.JTextArea newTextArea) {
	textArea = newTextArea;
}
}
