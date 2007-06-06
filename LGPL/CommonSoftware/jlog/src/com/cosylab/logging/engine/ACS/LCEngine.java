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

import org.omg.CORBA.ORB;

import si.ijs.maci.Manager;

import com.cosylab.logging.engine.RemoteAccess;
import com.cosylab.logging.engine.log.ILogEntry;

/**
 * LCEngine connects to the logging NC and sends
 * messages to the listeners
 * 
 * There are three type of listeners supported:
 *   - ACSLogConnectionListener: listens events related to the conenction
 *                                with the logging NC and reportMessages
 *   - ACSRemoteLogListener: listens for LogEntries
 *   - ACSRemoteRawLogListener: listens for XML strings representing logs
 *   
 * It there are no ACSRemoteLogLiestenersRegistered then the string received
 * from the NC is not parsed
 * 
 * @see ACSRemoteLogListener
 * @see ACSRemoteRawLogListener
 * @see ACSLogConnectionListener
 *  
 */
public class LCEngine {
	/**
	 * The connection is checked every CHECK_INTERVAL seconds
	 */
	private final int CHECK_INTERVAL = 15;
	
	// The boolean remeber if the client was connected before
	// checking for the connection (needed to understand if the
	// connection has been lost or never happened)
	private boolean wasConnected=false;
	
	// Signal the thread to terminate
	private AccessChecker connCheckerThread=null;
	private boolean terminateThread=false;
	
	private RemoteAccess remoteAccess = null;

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
		
		/**
		 * Constructor
		 *
		 */
		public AccessSetter() {
			super("AccessSetter");
			setDaemon(true);
		}
		
		/**
		 * Connect to the NC.
		 * It starts the thread to check the status of the connection (LCEngine.run) 
		 * if it is not already alive (this last case can happen if the connection has been lost)
		 */
		public void run() {
			disconnectRA();
			listenersDispatcher.publishConnecting();
			listenersDispatcher.publishReport("Connecting to " + accessType + " remote access...");
			try {
				
				Class[] parameters = {listenersDispatcher.getClass()};
				String raName = accessType;
				if (accessType.indexOf(".") == -1)
					raName = "com.cosylab.logging.engine."
							+ accessType
							+ "."
							+ accessType
							+ "RemoteAccess"; // com.cosylab.logging.engine.ACS.ACSRemoteAccess
				//remoteAccess = (RemoteAccess) Class.forName(raName).getConstructors()[0].newInstance(parameters);
				remoteAccess = (RemoteAccess) Class.forName(raName).getConstructor(parameters).newInstance(listenersDispatcher);
				//if (remoteAccess == null)
				//	System.out.println("RemoteAccess == null");
				remoteAccess.initialize(orb,manager);
			} catch (Throwable e) {
				listenersDispatcher.publishReport("Exception occurred when initializing " + accessType + " remote access.");
				listenersDispatcher.publishConnected(false);
				System.out.println("Exception in LCEngine$AccessSetter::run(): " + e);
				return;
			}
			if (remoteAccess!=null && remoteAccess.isInitialized()) {
				listenersDispatcher.publishReport("Connected to " + accessType + " remote access.");
				listenersDispatcher.publishConnected(true);
				LCEngine.this.wasConnected=true;
			} else {
				listenersDispatcher.publishConnected(false);
			}
		}
	}

	/**
	 * The thread to check the status of the connection
	 * 
	 * @author acaproni
	 *
	 */
	private class AccessChecker extends Thread {
		
		/**
		 * Constructor
		 *
		 */
		public AccessChecker() {
			super("AccessChecker");
		}
		
		/**
		 * The thread that 
		 * 1- monitors the status of the connection
		 * 2- reconnect if the autoReconnect option is activated
		 * 
		 * The thread is started when the connection is activated and terminated
		 * when disconnects from the NC.
		 * 
		 */
		public void run() {
			int currentSec=0;
			while (!terminateThread) {
				// wait for CHECK_INTERVAL secs..
				while (currentSec<CHECK_INTERVAL) {
					try {
						Thread.sleep(1000);
						currentSec++;
						if (terminateThread) {
							return;
						}
					} catch (InterruptedException ie) {
						continue;
					}
				}
				
				currentSec=0;
				// Check the connection!
				boolean connected = isConnected();
				//publishConnected(connected);
				if (wasConnected && !connected) {
					listenersDispatcher.publishReport("Connection lost");
					wasConnected=false;
					disconnectRA();
					listenersDispatcher.publishConnectionLost();
					// Better otherwise it tries to reconnect every time
					if (!autoReconnect) {
						return; // Terminate the thread
					}
					
				}
				if (autoReconnect && !connected) {
					connect();
				}
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
	 * If true the engine tries to reconnect automatically
	 */
	private boolean autoReconnect = false;
	
	// Dispatches messages to the listeners
	private ACSListenersDispatcher listenersDispatcher = new ACSListenersDispatcher();
	
	/**
	 * LCEngine constructor.
	 *
	 */
	public LCEngine() {
	}
	
	/**
	 * LCEngine constructor.
	 *
	 * @param autoReconn If true the engine automatically reconnects
	 */
	public LCEngine(boolean autoReconn) {
		autoReconnect=autoReconn;
	}
	
	/**
	 * LCEngine starts an attempt to connect to the remote system.
	 * Connection is handled in a separate Thread
	 * @see LCEngine$AccessSetter
	 */
	public void connect() {
		new AccessSetter().start();
		if (connCheckerThread==null || !connCheckerThread.isAlive()) {
			terminateThread=false;
			connCheckerThread = new AccessChecker();
			connCheckerThread.setName("LCEngine");
			connCheckerThread.start();
		} 
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
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public void disconnect() {
		disconnectRA();
		// Stop the thread to check the status of the connection
		if (connCheckerThread!=null) {
			terminateThread=true;
			while (connCheckerThread!=null && connCheckerThread.isAlive()) {
				try {
					Thread.sleep(250);
				} catch (InterruptedException ie) {
					continue;
				}
			}
			connCheckerThread=null;
		}
	}
	
	/**
	 * Disconnect the remote access
	 *
	 */
	private void disconnectRA() {
		if (remoteAccess != null && remoteAccess.isInitialized()) {
			try {
				listenersDispatcher.publishReport("Disconnecting from " + accessType + " remote access...");
				remoteAccess.destroy();
			} catch (Exception e) {
				listenersDispatcher.publishReport("Exception occurred when destroying " + accessType + " remote access.");
				System.out.println("Exception in LCEngine$AccessDestroyer::run(): " + e);
			}
			listenersDispatcher.publishReport("Disconnected from " + accessType + " remote access.");
			if (remoteAccess!=null) {
				remoteAccess.close(false);
			}
		}
		remoteAccess = null;
		listenersDispatcher.publishConnected(false);
		LCEngine.this.wasConnected=false;
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
		} 
		return remoteAccess.isConnected();
	}
	
	/**
	 * Suspend resume the notification of logs
	 * NOTE: When suspended the log discarded are lost forever
	 * 
	 * @param suspended If true suspend the notification of the logs
	 */
	public void setSupended(boolean suspended) {
		if (remoteAccess instanceof ACSRemoteAccess) {
			((ACSRemoteAccess)remoteAccess).setSuspended(suspended);
		}
	}
	
	/**
	 * Enable/disable the auto reconnection
	 * 
	 * @param autoRec If true the engine tries to reconnect automatically
	 */
	public void enableAutoReconnection(boolean autoRec) {
		autoReconnect=autoRec;
	}
	
	/**
	 * Pause/unpause the publishing of logs to the listener
	 * The difference between pause and suspended is that
	 * when the engine is suspended all the received logs are
	 * discarded. When it is paused, the received logs are
	 * cached and published when the engine will be unpaused.
	 * 
	 * @param pause
	 */
	public void setPaused(boolean pause) {
		remoteAccess.pause(pause);
	}
	
	/**
	 * Add a log listener
	 * 
	 * @param listener The listener to add
	 */
	public void addLogListener(ACSRemoteLogListener listener) {
		listenersDispatcher.addLogListener(listener);
	}
	
	/**
	 * Add a RAW log listener
	 * 
	 * @param listener The listener to add
	 */
	public void addRawLogListener(ACSRemoteRawLogListener listener) {
		listenersDispatcher.addRawLogListener(listener);
	}
	
	/**
	 * Add a RAW log listener
	 * 
	 * @param listener The listener to add
	 */
	public void addLogConnectionListener(ACSLogConnectionListener listener) {
		listenersDispatcher.addLogConnectionListener(listener);
	}
}
