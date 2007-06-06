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

package com.cosylab.logging.engine.ACS;

import java.util.Vector;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * Dispatches messages to listeners
 * 
 * @author acaproni
 *
 */
public class ACSListenersDispatcher {
	/**
	 * The log listeners for this connection
	 */
	private Vector<ACSRemoteLogListener> logListeners = new Vector<ACSRemoteLogListener>();
	// The number of listeners (it is the same of listeners.size() but It avoids
	// executing a method)
	private int logListenersNum=0;
	
	/**
	 * The listeners of the status of the connection and report messages
	 */
	private Vector<ACSLogConnectionListener> connectionListeners = new Vector<ACSLogConnectionListener>();
	private int connListenersNum=0;
	
	/**
	 * The listeners of the XML strings representing a log
	 */
	private Vector<ACSRemoteRawLogListener> rawLogListeners = new Vector<ACSRemoteRawLogListener>();
	private int rawLogListenersNum=0;
	
	/**
	 * Add a log listener
	 * 
	 * @param listener The listener to add
	 */
	public void addLogListener(ACSRemoteLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		synchronized(logListeners) {
			logListeners.add(listener);
			logListenersNum=logListeners.size();
		}
	}
	
	/**
	 * Add a RAW log listener
	 * 
	 * @param listener The listener to add
	 */
	public void addRawLogListener(ACSRemoteRawLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		synchronized(rawLogListeners) {
			rawLogListeners.add(listener);
			rawLogListenersNum=rawLogListeners.size();
		}
	}
	
	/**
	 * Add a RAW log listener
	 * 
	 * @param listener The listener to add
	 */
	public void addLogConnectionListener(ACSLogConnectionListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		synchronized(connectionListeners) {
			connectionListeners.add(listener);
			connListenersNum=connectionListeners.size();
		}
	}
	
	/**
	 * Publish a report string to the listeners (if any)
	 * 
	 * @param message The message to publish
	 */
	public void publishReport(String message){
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
					listener.reportStatus(message);
				}
			}
		}
	}
	
	/**
	 * Publish the connection status to the listeners
	 * The connection status means connected/disconnected
	 * 
	 * @param connected
	 */
	public void publishConnected(boolean connected) {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
						if (connected) {
							listener.acsLogConnEstablished();
						} else {
							listener.acsLogConnDisconnected();
						}
				}
			}
		}
	}
	
	/**
	 * Notify the listeners that the connection has been lost
	 *
	 */
	public void publishConnectionLost() {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
						listener.acsLogConnLost();
				}
			}
		}
	}
	
	/**
	 * Notify the listeners that an attempt to connect is in progress
	 */
	public void publishConnecting() {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
					listener.acsLogConnConnecting();
				}
			}
		}
	}
	
	/**
	 * Publish a log to the listeners (if any)
	 * 
	 * @param newLog The log to send to the listeners
	 */
	public void publishLog(ILogEntry newLog) {
		synchronized(logListeners) {
			for (int t=0; t<logListenersNum; t++) {
				ACSRemoteLogListener listener = logListeners.get(t);
				listener.logEntryReceived(newLog);
			}
		}
	}
	
	/**
	 * Publish a RAW log to the listeners (if any)
	 * 
	 * @param newLog The XML string to send to the listeners
	 */
	public void publishRawLog(String xmlStr) {
		synchronized(rawLogListeners) {
			for (int t=0; t<rawLogListenersNum; t++) {
				ACSRemoteRawLogListener listener = rawLogListeners.get(t);
				listener.xmlEntryReceived(xmlStr);
			}
		}
	}
	
	public void publishSuspended() {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
					listener.acsLogConnSuspended();
				}
			}
		}
	}
	
	public void publishDiscarding() {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
					listener.acsLogsDelay();
				}
			}
		}
	}
	
	/**
	 * Remove a connection status listener
	 * 
	 * @param listener The listener to remove
	 * @return true if the listener has been effectively removed
	 * 
	 */
	public boolean removeLogListener(ACSRemoteLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		boolean ret;
		synchronized(logListeners) {
			ret=logListeners.remove(listener);
			logListenersNum=logListeners.size();
		}
		return ret;
	}
	
	/**
	 * Remove a connection status listener
	 * 
	 * @param listener The listener to remove
	 * @return true if the listener has been effectively removed
	 * 
	 */
	public boolean removeRawLogListener(ACSRemoteRawLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		boolean ret;
		synchronized(rawLogListeners) {
			ret=rawLogListeners.remove(listener);
			rawLogListenersNum=rawLogListeners.size();
		}
		return ret;
	}
	
	/**
	 * Remove a connection status listener
	 * 
	 * @param listener The listener to remove
	 * @return true if the listener has been effectively removed
	 * 
	 */
	public boolean removeConnectionListener(ACSRemoteLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		boolean ret;
		synchronized(connectionListeners) {
			ret=connectionListeners.remove(listener);
			connListenersNum=connectionListeners.size();
		}
		return ret;
	}
	
	/**
	 * 
	 * @return true is there are registerd log listeners
	 */
	public boolean hasLogListeners() {
		return logListenersNum>0;
	}
	
	/**
	 * 
	 * @return true is there are registerd raw log listeners
	 */
	public boolean hasRawLogListeners() {
		return rawLogListenersNum>0;
	}
	
	/**
	 * 
	 * @return true is there are registerd connection listeners
	 */
	public boolean hasConnectionListeners() {
		return connListenersNum>0;
	}
}
