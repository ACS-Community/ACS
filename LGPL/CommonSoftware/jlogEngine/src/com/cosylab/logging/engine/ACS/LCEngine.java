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

import java.lang.reflect.Constructor;

import org.omg.CORBA.ORB;

import si.ijs.maci.Manager;

import com.cosylab.logging.engine.Filter;
import com.cosylab.logging.engine.Filterable;
import com.cosylab.logging.engine.FiltersVector;
import com.cosylab.logging.engine.RemoteAccess;
import com.cosylab.logging.engine.audience.Audience;
import com.cosylab.logging.engine.cache.ILogQueueFileHandler;
import com.cosylab.logging.engine.log.LogTypeHelper;

/**
 * LCEngine connects to the logging NC and sends messages to the listeners.
 * 
 * A LCEngine object can have an audience (as defined in the log_audience IDL
 * module). If an audience is in use, a special set of filters will be applied.
 * 
 * It is possible to define custom filters to apply to the incoming logs. In this
 * case, only the logs that pass all the filters are sent to the listeners. The
 * filters do not apply to RAW (i.e. XML) log listeners.
 * The custom filters are applied after the audience filtering.
 * 
 * There are three type of listeners supported: - ACSLogConnectionListener:
 * listens events related to the connection with the logging NC and
 * reportMessages - ACSRemoteLogListener: listens for LogEntries -
 * ACSRemoteRawLogListener: listens for XML strings representing logs
 * 
 * It there are no ACSRemoteLogLiestenersRegistered then the string received
 * from the NC is not parsed
 * 
 * The logs read from the NC are sent to the <code>logRetrieval</code> that, 
 * in turn, send them to the registered listeners.
 * Audience and filtering are implemented by <code>ACSLogRetrieval</code>.
 * 
 * @see ACSRemoteLogListener
 * @see ACSRemoteRawLogListener
 * @see ACSLogConnectionListener
 * 
 */
public class LCEngine implements Filterable {
	
	/**
	 * The connection is checked every CHECK_INTERVAL seconds
	 */
	private final int CHECK_INTERVAL = 15;

	// The boolean remember if the client was connected before
	// checking for the connection (needed to understand if the
	// connection has been lost or never happened)
	private boolean wasConnected = false;

	// Signal the thread to terminate
	private AccessChecker connCheckerThread = null;
	private boolean terminateThread = false;

	private RemoteAccess remoteAccess = null;
	
	/**
	 * The filters to apply to the incoming logs.
	 * 
	 * The filters are applied by <code>ACSLogReceiver.run()</code>
	 */
	private FiltersVector filters = null;

	/**
	 * A thread used to set and initialize RemoteAccess
	 * 
	 * Constructor parameters:
	 * 
	 * @accessType (String) name of the RemoteAccessClass e.g. ACS class has to
	 *             be in the package com.cosylab.logging.engine."accessType" and
	 *             has to be named "accessType"RemoteAccess e.g.
	 *             com.cosylab.logging.engine.ACS.ACSRemoteAccess
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
		 * Connect to the NC. It starts the thread to check the status of the
		 * connection (LCEngine.run) if it is not already alive (this last case
		 * can happen if the connection has been lost)
		 */
		public void run() {
			if (listenersDispatcher==null || logRetrieval==null) {
				throw new IllegalStateException("The listener and the log retrieval can't be null");
			}
			disconnectRA();
			listenersDispatcher.publishConnecting();
			listenersDispatcher.publishReport("Connecting to " + accessType
					+ " remote access...");
			try {

				Class[] parameters = { listenersDispatcher.getClass(), logRetrieval.getClass() };
				String raName = accessType;
				if (accessType.indexOf(".") == -1)
					raName = "com.cosylab.logging.engine." + accessType + "."
							+ accessType + "RemoteAccess"; // com.cosylab.logging.engine.ACS.ACSRemoteAccess
				Class theClass = Class.forName(raName);
				Constructor ctor = theClass.getConstructor(parameters);
				remoteAccess = (RemoteAccess)ctor.newInstance(listenersDispatcher,logRetrieval);
				remoteAccess.initialize(orb, manager);
			} catch (Throwable e) {
				listenersDispatcher
						.publishReport("Exception occurred when initializing "
								+ accessType + " remote access.");
				listenersDispatcher.publishConnected(false);
				System.out.println("Exception in LCEngine$AccessSetter::run(): "+ e);
				e.printStackTrace();
				return;
			}
			if (remoteAccess != null && remoteAccess.isInitialized()) {
				listenersDispatcher.publishReport("Connected to " + accessType
						+ " remote access.");
				listenersDispatcher.publishConnected(true);
				LCEngine.this.wasConnected = true;
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
		 * The thread that 1- monitors the status of the connection 2- reconnect
		 * if the autoReconnect option is activated
		 * 
		 * The thread is started when the connection is activated and terminated
		 * when disconnects from the NC.
		 * 
		 */
		public void run() {
			int currentSec = 0;
			while (!terminateThread) {
				// wait for CHECK_INTERVAL secs..
				while (currentSec < CHECK_INTERVAL) {
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

				currentSec = 0;
				// Check the connection!
				boolean connected = isConnected();
				// publishConnected(connected);
				if (wasConnected && !connected) {
					listenersDispatcher.publishReport("Connection lost");
					wasConnected = false;
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

	/**
	 * accessType, orb and manager are used to connect to the logging channel
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
	
	// The log retrieval i,e. the object receiving logs and dispatching
	// to the listeners
	private final ACSLogRetrieval logRetrieval;

	/**
	 * LCEngine constructor
	 * 
	 */
	public LCEngine() {
		logRetrieval=new ACSLogRetrieval(listenersDispatcher, Boolean.parseBoolean(ACSRemoteAccess.LOGGING_BINARY_FORMAT));
	}

	/**
	 * LCEngine constructor.
	 * 
	 * @param autoReconn
	 *            If true the engine automatically reconnects
	 */
	public LCEngine(boolean autoReconn) {
		this();
		autoReconnect = autoReconn;
	}

	/**
	 * LCEngine constructor.
	 * <P>
	 * This constructor allows to define a set of filters to apply to incoming
	 * logs: only the logs passing the filters are sent to the listeners.
	 * <P>
	 * 
	 * @param autoReconn
	 *            If true the engine automatically reconnects
	 * @param filters
	 *            The filters to apply to the incoming logs.
	 *            <code>filters</code> can be null or empty.
	 */
	public LCEngine(boolean autoReconn, FiltersVector filters) {
		this(autoReconn);
		setFilters(filters,false);
		
	}
	
	/**
	 * Build the <code>LCEngine</code> by setting an handler for
	 * the files of the cache.
	 *  
	 * @param cacheFileHandler The handler of the files of the cache
	 * 
	 * @see ILogQueueFileHandler
	 */
	public LCEngine(ILogQueueFileHandler cacheFileHandler) {
		logRetrieval=new ACSLogRetrieval(
				listenersDispatcher, 
				Boolean.parseBoolean(ACSRemoteAccess.LOGGING_BINARY_FORMAT),
				cacheFileHandler);
	}
	
	/**
	 * <code>LCEngine</code> constructor.
	 * 
	 * @param autoReconn If <code>true</code> the engine automatically reconnects
	 * @param cacheFileHandler The handler of the files of the cache
	 * 
	 * @see ILogQueueFileHandler
	 */
	public LCEngine(boolean autoReconn, ILogQueueFileHandler cacheFileHandler) {
		this(cacheFileHandler);
		autoReconnect = autoReconn;
	}

	/**
	 * <code>LCEngine</code> constructor.
	 * <P>
	 * This constructor allows to define a set of filters to apply to incoming
	 * logs: only the logs passing the filters are sent to the listeners.
	 * <P>
	 * 
	 * @param autoReconn If <code>true</code> the engine automatically reconnects
	 * @param filters
	 *            The filters to apply to the incoming logs.
	 *            <code>filters</code> can also be <code>null</code> or empty.
	 * @param cacheFileHandler The handler of the files of the cache
	 * 
	 * @see ILogQueueFileHandler
	 */
	public LCEngine(
			boolean autoReconn,
			FiltersVector filters,
			ILogQueueFileHandler cacheFileHandler) {
		this(autoReconn,cacheFileHandler);
		setFilters(filters,false);
	}

	/**
	 * LCEngine starts an attempt to connect to the remote system. Connection is
	 * handled in a separate Thread
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public void connect() {
		new AccessSetter().start();
		if (connCheckerThread == null || !connCheckerThread.isAlive()) {
			terminateThread = false;
			connCheckerThread = new AccessChecker();
			connCheckerThread.setName("LCEngine");
			connCheckerThread.setDaemon(true);
			connCheckerThread.start();
		}
	}

	/**
	 * LCEngine starts an attempt to connect to the remote system. Connection is
	 * handled in a separate Thread
	 * 
	 * @param accessTyp
	 *            The access type
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public void connect(String newAccessType) {
		setAccessType(newAccessType);
		connect();
	}

	/**
	 * LCEngine starts an attempt to connect to the remote system. Connection is
	 * handled in a separate Thread
	 * 
	 * @param theORB
	 *            The ORB (can be null)
	 * @param mgr
	 *            The reference to the manager (can be null)
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public void connect(ORB theORB, Manager mgr) {
		setConnectionParams(theORB, mgr);
		connect();
	}
	
	/**
	 * Close the engine and free the resources.
	 * 
	 * @param sync If <code>true</code> the closing is made in a synchronized way.
	 */
	public synchronized void close(boolean sync) {
		logRetrieval.close(sync);
		disconnect();
	}

	/**
	 * LCEngine starts an attempt to connect to the remote system. Connection is
	 * handled in a separate Thread
	 * 
	 * @see LCEngine$AccessSetter
	 */
	public synchronized void disconnect() {
		
		// Stop the thread to check the status of the connection
		if (connCheckerThread != null) {
			terminateThread = true;
			while (connCheckerThread != null && connCheckerThread.isAlive()) {
				try {
					Thread.sleep(250);
				} catch (InterruptedException ie) {
					continue;
				}
			}
			connCheckerThread = null;
		}
		disconnectRA();
	}

	/**
	 * Disconnect the remote access
	 * 
	 */
	private void disconnectRA() {
		if (remoteAccess != null && remoteAccess.isInitialized()) {
			try {
				listenersDispatcher.publishReport(
						"Disconnecting from "+accessType+" remote access...");
				remoteAccess.close(true);
			} catch (Exception e) {
				listenersDispatcher.publishReport(
						"Exception occurred when destroying "+ accessType + " remote access.");
				System.out.println("Exception in LCEngine$AccessDestroyer::run(): "+ e);
				e.printStackTrace();
			}
			listenersDispatcher.publishReport("Disconnected from " + accessType	+ " remote access.");
		}
		if (remoteAccess != null) {
			remoteAccess.destroy();
		}
		remoteAccess = null;
		listenersDispatcher.publishConnected(false);
		LCEngine.this.wasConnected = false;
	}

	/**
	 * Insert the method's description here. Creation date: (2/18/2002 9:58:30
	 * AM)
	 * 
	 * @return java.lang.String
	 */
	public java.lang.String getAccessType() {
		return accessType;
	}

	/**
	 * Insert the method's description here. Creation date: (2/18/2002 9:58:30
	 * AM)
	 * 
	 * @param newAccessType
	 *            java.lang.String
	 */
	public void setAccessType(String newAccessType) {
		accessType = newAccessType;
	}

	/**
	 * Set the connection params fro this
	 * 
	 * @param theORB
	 *            The ORB. It can't be null if the manager is not null
	 * @param mgr
	 *            The reference to the Manager
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
		if (remoteAccess == null) {
			return false;
		}
		return remoteAccess.isConnected();
	}

	/**
	 * Suspend resume the notification of logs NOTE: When suspended the log
	 * discarded are lost forever
	 * 
	 * @param suspended
	 *            If true suspend the notification of the logs
	 */
	public void setSupended(boolean suspended) {
		if (remoteAccess instanceof ACSRemoteAccess) {
			((ACSRemoteAccess) remoteAccess).setSuspended(suspended);
		}
	}

	/**
	 * Enable/disable the auto reconnection
	 * 
	 * @param autoRec
	 *            If true the engine tries to reconnect automatically
	 */
	public void enableAutoReconnection(boolean autoRec) {
		autoReconnect = autoRec;
	}

	/**
	 * Pause/unpause the publishing of logs to the listener The difference
	 * between pause and suspended is that when the engine is suspended all the
	 * received logs are discarded. When it is paused, the received logs are
	 * cached and published when the engine will be unpaused.
	 * 
	 * @param pause
	 */
	public void setPaused(boolean pause) {
		logRetrieval.pause(pause);
	}

	/**
	 * Add a log listener
	 * 
	 * @param listener
	 *            The listener to add
	 * 
	 * @see ACSRemoteLogListener
	 */
	public void addLogListener(ACSRemoteLogListener listener) {
		listenersDispatcher.addLogListener(listener);
	}

	/**
	 * Add a RAW log listener
	 * 
	 * @param listener
	 *            The listener to add
	 * 
	 * @see ACSRemoteRawLogListener
	 */
	public void addRawLogListener(ACSRemoteRawLogListener listener) {
		listenersDispatcher.addRawLogListener(listener);
	}

	/**
	 * Add an error listener
	 * 
	 * @param listener
	 *            The error listener to add
	 * 
	 * @see ACSRemoteErrorListener
	 */
	public void addLogErrorListener(ACSRemoteErrorListener listener) {
		listenersDispatcher.addErrorListener(listener);
	}

	/**
	 * Add a connection listener
	 * 
	 * @param listener
	 *            The listener to add
	 * 
	 * @see ACSLogConnectionListener
	 * 
	 */
	public void addLogConnectionListener(ACSLogConnectionListener listener) {
		listenersDispatcher.addLogConnectionListener(listener);
	}

	/**
	 * Set the filters to apply before sending the log entries to the listener.
	 * By setting a null or empty filters disable the filtering.
	 * 
	 * @param newFilters
	 *            The new filters to apply If <code>null</code> or empty the
	 *            filtering is disabled
	 * @param append
	 *            If <code>true</code> the new filters are added to the
	 *            existing filters (if any).
	 */
	public void setFilters(FiltersVector newFilters, boolean append) {
		if (newFilters==null || newFilters.isEmpty()) {
			filters=null;
			return;
		}
		if (filters==null) {
			filters=new FiltersVector();
		}
		
		if (!append) {
			filters.clear();
		}
		for (int t=0; t<newFilters.size(); t++) {
			filters.addFilter(newFilters.get(t), newFilters.isActive(t));
		}
		logRetrieval.setFilters(filters);
	}
	
	/**
	 * Set the discard level for filtering.
	 * <P>
	 * <I>Note</I>: if the dynamic change of the discard
	 * 				level depending on the memory usage has been
	 * 				activated, then the discard level effectively
	 * 				used by the engine might be greater then
	 * 				<code>newDiscardlevel</code>.
	 * 
	 * @param newDiscardlevel The discard level
	 *                        Not applied if <code>null</code>.
	 * @see {@link ACSLogRetrieval}
	 * 
	 */
	public void setDiscardLevel(LogTypeHelper newDiscardLevel) {
		logRetrieval.setDiscardLevel(newDiscardLevel);
	}
	
	/**
	 * Set the audience for the engine.
	 * If an audience is defined, a special set of filters is applied.
	 * 
	 * @param newAudience The not <code>null</code >audience
	 */
	public void setAudience(Audience newAudience) {
		if (newAudience==null) {
			throw new IllegalArgumentException("The audience can't be null");
		}
		logRetrieval.setAudience(newAudience);
	}

	/**
	 * Add a new filters to the vector of filters to apply to the incoming logs
	 * before sending to the listeners.
	 * <P>
	 * The filter is applied as active.
	 * 
	 * @param filter
	 *            The not null filter to add
	 */
	public void addFilter(Filter filter) {
		if (filter == null) {
			throw new IllegalArgumentException("The filter can't be null");
		}
		if (filters == null) {
			filters = new FiltersVector();
		}
		filters.addFilter(filter, true);
		logRetrieval.setFilters(filters);
	}

	/**
	 * Remove all the filters.
	 * 
	 * After calling this method, all the received logs are forwarded to the
	 * listeners.
	 */
	public void clearFilters() {
		filters = null;
		logRetrieval.setFilters(null);
	}

	/**
	 * Return the filters to filter the incoming logs before sending to the
	 * listeners
	 * 
	 * @return The filters (can be <code>null</code>)
	 */
	public FiltersVector getFilters() {
		return filters;
	}
	
	/**
	 * Return the discard level in use by the engine.
	 * <P>
	 * <I>Note</I>: the actual discard level may or may not be the same
	 * 				discard level set by the user. 
	 * 				They differ if the user enabled the dynamic change of 
	 * 				the discard level depending on the available memory .
	 * 
	 * @return The actual discard level (can be <code>null</code>)
	 * 
	 * @see {@link ACSLogRetrieval.getDiscardlevel()}, {@link LogMatcher.getActualDiscardLevel()}
	 * @see <code>getDiscardLevel()</code>
	 */
	public LogTypeHelper getActualDiscardLevel() {
		return logRetrieval.getActualDiscardLevel();
	}
	
	/**
	 * Return the discard level set by the user.
	 * <P>
	 * <I>Note</I>: the discard level in use can be different if the
	 * 				dynamic change of the discard level depending
	 * 				on the available memory in the engine has been
	 * 				activated.
	 * 
	 * @return The discard level (can be <code>null</code>)
	 * 
	 * @see {@link ACSLogRetrieval.getDiscardlevel()}, {@link LogMatcher.getActualDiscardLevel()}
	 * @see <code>getActualDiscardLevel()</code>
	 */
	public LogTypeHelper getDiscardLevel() {
		return logRetrieval.getDiscardLevel();
	}
	
	
	/**
	 * 
	 * @return The discard level (can be <code>null</code>)
	 */
	public Audience getAudience() {
		return logRetrieval.getAudience();
	}

	/**
	 * Return <code>true</code> if the engine is applying filters.
	 * If a filter is in place but not enabled then it is ignored.
	 * 
	 * @return <code>true</code> if there are filters active in the engine
	 * @see FiltersVector.hasActiveFilters()
	 */
	public boolean isFiltered() {
		if (filters==null) {
			return false;
		} else {
			return filters.hasActiveFilters();
		}
	}
	
	/**
	 * 
	 * @return A description of the active filters
	 * @see FiltersVector.getFilterString()
	 */
	public String getFiltersString() {
		if (filters==null) {
			return "Not filtered";
		} else {
			return filters.getFilterString();
		}
	}
	
	/**
	 * Set the max number of logs per second to accept from the 
	 * <code>RemoteAccess</code>, typically the logging NC.
	 * <P>
	 * All the logs arriving after the max number has been reached will be discarded,
	 * regardless of their level.
	 * <P>
	 * See {@link ACSLogRetrieval} for further details.
	 * 
	 * @param rate The max number of logs per second to accept
	 */
	public void setMaxInputRate(int rate) {
		logRetrieval.setMaxInputRate(rate);
	}
	
	/**
	 * @return The actual max input rate
	 * 
	 * See {@link ACSLogRetrieval} for further details.
	 */
	public int getMaxInputRate() {
		return logRetrieval.getMaxInputRate();
	}
	
	
	/**
	 * 
	 * @return The actual input rate 
	 * 
	 * See {@link ACSLogRetrieval} for further details.
	 */
	public int getActualInputRate() {
		return logRetrieval.getInputRate();
	}
	
	/**
	 * Set the max number of logs to publish to listeners.
	 * <P>
	 * When this number has been reached, no more logs are sent to the
	 * listeners.
	 * <P>
	 * See {@link ACSLogRetrieval} for further details.
	 * 
	 * @param rate The max number of logs per second to accept
	 */
	public void setMaxOutputRate(int rate) {
		logRetrieval.setMaxOutputRate(rate);
	}
	
	/**
	 * @return The actual max input rate
	 * 
	 * See {@link ACSLogRetrieval} for further details.
	 */
	public int getMaxOutputRate() {
		return logRetrieval.getMaxOutputRate();
	}
	
	/**
	 * 
	 * @return The actual input rate 
	 * 
	 * See {@link ACSLogRetrieval} for further details.
	 */
	public int getActualOutputRate() {
		return logRetrieval.getOutputRate();
	}
	
	/**
	 * Return the number of logs waiting in the cache i.e. the logs
	 * received from the <code>RemoteAcess</code> and not yet sent to
	 * the listeners.
	 * @return the number of logs waiting in the cache
	 */
	public int waitingLogsNumber() {
		return logRetrieval.size();
	}

	/**
	 * Enable or disable the dynamic change of the discard level
	 * depending on the amount of available memory.
	 *  
	 * @param threashold The discard level is increased when the available
	 * 					memory for the application is less then the <code>threshold</code>
	 * 					(in bytes).
	 * 					<code>Integer.MAX_VALUE</code> disables this feature.
	 * @param damping The damping factor is used to avoid oscillations
	 * 					The discard level is decreased when the free memory is
	 * 					is greater then the <code>threshold</code> plus the <code>dumping</code>.
	 * @param interval The time (in seconds) between two adjustments of the 
	 * 					dynamic discard level.  
	 * 					<code>interval</code> defaults to <code>10</code>.
	 * 
	 * @see {@link ACSLogRetrieval}
	 */
	public void enableDynamicDiscarding(int threshold, int damping, int interval) {
		logRetrieval.enableDynamicDiscarding(threshold, damping, interval);
	}
	
}
