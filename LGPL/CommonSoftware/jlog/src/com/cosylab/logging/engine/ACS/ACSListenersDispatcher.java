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
 * Dispatches messages to listeners.
 * 
 * At least one error listener must be present.
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
	private volatile int logListenersNum=0;
	
	/**
	 * The listeners of the status of the connection and report messages
	 */
	private Vector<ACSLogConnectionListener> connectionListeners = new Vector<ACSLogConnectionListener>();
	private volatile int connListenersNum=0;
	
	/**
	 * The listeners of the XML strings representing a log
	 */
	private Vector<ACSRemoteRawLogListener> rawLogListeners = new Vector<ACSRemoteRawLogListener>();
	private volatile int rawLogListenersNum=0;
	
	/**
	 * The listeners for the errors generated when a generating logs (parsing an XML or building a log
	 * from a cache string)
	 * 
	 */
	private Vector<ACSRemoteErrorListener> errorListeners = new Vector<ACSRemoteErrorListener>();
	private volatile int errorListenersNum=0;
	
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
	 * Add an error listener
	 * 
	 * @param listener The listener to add
	 */
	public void addErrorListener(ACSRemoteErrorListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The error listener can't be null");
		}
		synchronized(errorListeners) {
			errorListeners.add(listener);
			errorListenersNum=errorListeners.size();
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
					try {
					listener.reportStatus(message);
					} catch (Throwable thr) {
						System.err.println("Exception publishing to listener: "+thr.getMessage());
						thr.printStackTrace(System.err);
					}
				}
			}
		}
	}
	
	/**
	 * Notify the listener that an error happened parsing a log.
	 * If there are no listeners then prints a message in the stderr
	 * 
	 * @param error The string that generated the error
	 */
	public void publishError(String error){
		synchronized(errorListeners) {
			if (errorListeners==null || errorListeners.size()==0) {
				StringBuilder str = new StringBuilder("Error parsing the following log: \n");
				str.append(error);
				str. append("\n The log has been lost.");
				System.err.println(str.toString());
			} else {
				for (ACSRemoteErrorListener errorListener: errorListeners) {
					try {
						errorListener.errorReceived(error);
					} catch (Throwable thr) {
						System.err.println("Exception publishing to listener: "+thr.getMessage());
						thr.printStackTrace(System.err);
					}
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
							try {
								listener.acsLogConnEstablished();
							} catch (Throwable thr) {
								System.err.println("Exception publishing to listener: "+thr.getMessage());
								thr.printStackTrace(System.err);
							}
						} else {
							try {
								listener.acsLogConnDisconnected();
							} catch (Throwable thr) {
								System.err.println("Exception publishing to listener: "+thr.getMessage());
								thr.printStackTrace(System.err);
							}
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
					try {
						listener.acsLogConnLost();
					} catch (Throwable thr) {
						System.err.println("Exception publishing to listener: "+thr.getMessage());
						thr.printStackTrace(System.err);
					}
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
					try {
						listener.acsLogConnConnecting();
					} catch (Throwable thr) {
						System.err.println("Exception publishing to listener: "+thr.getMessage());
						thr.printStackTrace(System.err);
					}
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
				try {
					listener.logEntryReceived(newLog);
				} catch (Throwable thr) {
					System.err.println("Exception publishing to listener: "+thr.getMessage());
					thr.printStackTrace(System.err);
				}
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
				try {
					listener.xmlEntryReceived(xmlStr);
				} catch (Throwable thr) {
					System.err.println("Exception publishing to listener: "+thr.getMessage());
					thr.printStackTrace(System.err);
				}
			}
		}
	}
	
	public void publishSuspended() {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
					try {
						listener.acsLogConnSuspended();
					} catch (Throwable thr) {
						System.err.println("Exception publishing to listener: "+thr.getMessage());
						thr.printStackTrace(System.err);
					}
				}
			}
		}
	}
	
	public void publishDiscarding() {
		synchronized(connectionListeners) {
			for (int t=0; t<connListenersNum; t++) {
				ACSLogConnectionListener listener = connectionListeners.get(t);
				if (listener!=null) {
					try {
						listener.acsLogsDelay();
					} catch (Throwable thr) {
						System.err.println("Exception publishing to listener: "+thr.getMessage());
						thr.printStackTrace(System.err);
					}
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
	 * Remove an error listener.
	 * 
	 * @param listener The listener to remove
	 * @return true if the listener has been effectively removed
	 * 
	 */
	public boolean removeErrorListener(ACSRemoteErrorListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null ACSRemoteErrorListener");
		}
		boolean ret=false;
		synchronized(errorListeners) {
			ret=errorListeners.remove(listener);
			errorListenersNum=errorListeners.size();
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
