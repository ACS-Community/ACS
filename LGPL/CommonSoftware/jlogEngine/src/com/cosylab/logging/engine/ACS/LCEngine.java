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

import javax.swing.JOptionPane;

import org.omg.CORBA.ORB;

import si.ijs.maci.Manager;

import com.cosylab.logging.engine.RemoteAccess;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * LCEngine defines output messages to the status panel whenever 
 * any of the control system components reports a message or an error occurs. 
 * This class also applies filters (criteria) to the logs.
 * 
 * Creation date: (10/31/2001 9:20:37 AM)
 * @author: 
 */
public class LCEngine implements Runnable {
	/**
	 * The connection is checked every CHECK_INTERVAL seconds
	 */
	private final int CHECK_INTERVAL = 15;
	
	// The boolean remeber if the client was connected before
	// checking for the connection (needed to understand if the
	// connection has been lost or never happened)
	private boolean wasConnected=false;
	
	private RemoteAccess remoteAccess = null;
	
	/**
	 * The listeners for this connection
	 */
	private Vector<ACSRemoteLogListener> listeners = new Vector<ACSRemoteLogListener>();

	/** 
	 * A thread used to set and initialize RemoteAccess
	 *
	 * Constructor parameters:
	 *
	 * @accessType (String) name of the RemoteAccessClass e.g. ACS
	 * class has to be in the package com.cosylab.logging.engine."accessType"
	 * and has to be named "accessType"RemoteAccess
	 * e.g. com.cosylab.logging.engine.ACS.ACSRemoteAccess
	 */
	private class AccessSetter extends Thread {
		
		public void run() {
			disconnect();
			publishConnectionStatus(false);
			publishConnecting();
			publishReport("Connecting to " + accessType + " remote access...");
			try {
				
				Class[] parameters = {LCEngine.this.getClass()};
				String raName = accessType;
				if (accessType.indexOf(".") == -1)
					raName = "com.cosylab.logging.engine."
							+ accessType
							+ "."
							+ accessType
							+ "RemoteAccess"; // com.cosylab.logging.engine.ACS.ACSRemoteAccess
				//remoteAccess = (RemoteAccess) Class.forName(raName).getConstructors()[0].newInstance(parameters);
				remoteAccess = (RemoteAccess) Class.forName(raName).getConstructor(parameters).newInstance(LCEngine.this);
				//if (remoteAccess == null)
				//	System.out.println("RemoteAccess == null");
				remoteAccess.initialize(orb,manager);
			} catch (Throwable e) {
				publishReport("Exception occurred when initializing " + accessType + " remote access.");
				publishConnectionStatus(false);
				System.out.println("Exception in LCEngine$AccessSetter::run(): " + e);
				return;
			}
			if (remoteAccess.isInitialized()) {
				publishReport("Connected to " + accessType + " remote access.");
				publishConnectionStatus(true);
				LCEngine.this.wasConnected=true;
			} else {
				publishConnectionStatus(false);
			}
		}
	}

	/** accessType, orb and manager are used to connect to
	 * the logging channel
	 * 
	 * @see AccessSetter.run
	 */ 
	private String accessType = "ACS";
	private ORB orb = null; // Can be null
	private Manager manager = null; // Can be null
	
	/**
	 * LCEngine constructor comment.
	 * 
	 * @param logEventListener The lister for log events
	 */
	public LCEngine(ACSRemoteLogListener logEventListener) {
		addLogRemoteConnListener(logEventListener);
		Thread thread = new Thread(this);
		thread.start();
	}
	
	/**
	 * LCEngine starts an attempt to connect to the remote system.
	 * Connection is handled in a separate Thread
	 * @see LCEngine$AccessSetter
	 */
	public void connect() {
		new AccessSetter().start();
	}
	
	
	/**
	 * LCEngine starts an attempt to connect to the remote system.
	 * Connection is handled in a separate Thread
	 * 
	 * @param accessTyp The access type
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public void connect(String newAccessType) {
		setAccessType(newAccessType);
		connect();
	}
	
	/**
	 * LCEngine starts an attempt to connect to the remote system.
	 * Connection is handled in a separate Thread
	 * 
	 * @param theORB The ORB (can be null) 
	 * @param mgr The reference to the manager (can be null)
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public void connect(ORB theORB, Manager mgr) {
		setConnectionParams(theORB,mgr);
		connect();
	}
	
	/**
	 * LCEngine starts an attempt to connect to the remote system.
	 * Connection is handled in a separate Thread
	 * @see LCEngine$AccessSetter
	 */
	public void disconnect() {
		if (remoteAccess != null && remoteAccess.isInitialized()) {
			try {
				publishReport("Disconnecting from " + accessType + " remote access...");
				remoteAccess.destroy();
			} catch (Exception e) {
				publishReport("Exception occurred when destroying " + accessType + " remote access.");
				System.out.println("Exception in LCEngine$AccessDestroyer::run(): " + e);
			}
			publishReport("Disonnected from " + accessType + " remote access.");
	//		System.out.println("Disonnected from " + accessType + " remote access.");
		}
		remoteAccess = null;
		publishConnectionStatus(false);
		LCEngine.this.wasConnected=false;
	}
	
	/**
	 * Insert the method's description here.
	 * Creation date: (2/9/2002 1:29:32 PM)
	 */
	public void exit() {
		disconnect();
		System.exit(0);	
	}
	
	/**
	 * Insert the method's description here.
	 * Creation date: (2/18/2002 9:58:30 AM)
	 * @return java.lang.String
	 */
	public java.lang.String getAccessType() {
		return accessType;
	}
	
	/**
	 * Insert the method's description here.
	 * Creation date: (2/18/2002 9:58:30 AM)
	 * @param newAccessType java.lang.String
	 */
	public void setAccessType(String newAccessType) {
		accessType = newAccessType;
	}
	
	/**
	 * Set the connection params fro this
	 * 
	 * @param theORB The ORB. It can't be null if the manager is not null
	 * @param mgr The reference to the Manager 
	 */
	public void setConnectionParams(ORB theORB, Manager mgr) {
		orb = theORB;
		manager = mgr;
	}
	
	
	/**
	 * 
	 * @return ture if the engine is connected to the notification channel
	 */
	public boolean isConnected() {
		if (remoteAccess==null) {
			return false;
		} else {
			return remoteAccess.isConnected();
		}
	}
	
	
	/**
	 * The thread that monitors the status of the connection
	 */
	public void run() {
		int attempts = 0;
		while (true) {
			// wait for CHECK_INTERVAL secs..
			try {
				Thread.sleep(CHECK_INTERVAL*1000);
				attempts=0;
			} catch (InterruptedException ie) {
				// If it not possible to sleep than it is better to stop 
				// the thread after 5 attempts
				System.err.println("Error in sleep: "+ie.getMessage());
				ie.printStackTrace(System.err);
				if (++attempts>5) {
					break;
				}
			}
			// Check the connection!
			boolean connected = isConnected();
			publishConnectionStatus(connected);
			if (wasConnected && !connected) {
				publishReport("Connection lost");
				wasConnected=false;
				// Better otherwise it tries to reconnect every time
				disconnect();
				JOptionPane.showMessageDialog(null,"Connection lost!","LoggingClient error",JOptionPane.ERROR_MESSAGE);
			}
		}
	}
	
	/**
	 * Add a connection status listener
	 * 
	 * @param listener The listener to add
	 */
	public void addLogRemoteConnListener(ACSRemoteLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		synchronized(listeners) {
			listeners.add(listener);
		}
	}
	
	/**
	 * Publish a report string to the listeners (if any)
	 * 
	 * @param message The message to publish
	 */
	public synchronized void publishReport(String message){
		if (listeners==null) {
			return;
		}
		synchronized(listeners) {
			for (int t=0; t<listeners.size(); t++) {
				ACSRemoteLogListener listener = listeners.get(t);
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
	public synchronized void publishConnectionStatus(boolean connected) {
		if (listeners==null) {
			return;
		}
		for (int t=0; t<listeners.size(); t++) {
			ACSRemoteLogListener listener = listeners.get(t);
			if (listener!=null) {
				if (connected) {
					listener.acsLogConnEstablished();
				} else {
					listener.acsLogConnLost();
				}
			}
		}
	}
	
	/**
	 * Notify the listeners that an attempt to connect is in progress
	 */
	public synchronized void publishConnecting() {
		if (listeners==null) {
			return;
		}
		for (int t=0; t<listeners.size(); t++) {
			ACSRemoteLogListener listener = listeners.get(t);
			if (listener!=null) {
				listener.acsLogConnConnecting();
			}
		}
	}
	
	/**
	 * Publish a log to the listeners (if any)
	 * 
	 * @param newLog The log to send to the listeners
	 */
	public synchronized void publishLog(ILogEntry newLog) {
		if (listeners==null) {
			return;
		}
		synchronized(listeners) {
			for (int t=0; t<listeners.size(); t++) {
				ACSRemoteLogListener listener = listeners.get(t);
				if (listener!=null) {
					listener.logEntryReceived(newLog);
				}
			}
		}
	}
	
	/**
	 * Remove a connection status listener
	 * 
	 * @param listener The listener to remove
	 * @return true if the listener is effectively removed
	 * 
	 */
	public boolean removeLogRemoteConnListener(ACSRemoteLogListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("Invalid null listener");
		}
		return listeners.remove(listener);
	}
}
