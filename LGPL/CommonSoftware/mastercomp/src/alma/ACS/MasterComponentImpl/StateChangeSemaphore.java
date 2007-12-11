/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
package alma.ACS.MasterComponentImpl;

import java.util.logging.Logger;

/**
 * Helper class that allows to synchronize the sending of events with the 
 * state changes due to previous events. 
 * Note that state changes occur asynchronously when triggered by activity states.
 * <p>
 * This class is abstracted from concrete state change notification APIs so that
 * subclasses can be implemented as such listeners; they must call {@link #stateChangedNotify()} 
 * upon notification by the state machine.
 * 
 * @author hsommer
 * created Apr 30, 2004 9:50:34 AM
 */
public class StateChangeSemaphore
{
	private volatile int notifCount = 0;
	private volatile int minCount;
	
	private final Logger logger;
    
	
	public StateChangeSemaphore(Logger logger) {
		if (logger == null) {
			throw new IllegalArgumentException("logger must not be null");
		}
		this.logger = logger;
	}

	/**
	 * To be called from state change listener method of subclass.
	 * <p>
	 * This call is expected to come from a different thread than the one that calls <code>waitForStateChanges</code>.
	 * Both methods are synchronized, but there will not be a deadlock, since <code>waitForStateChanges</code>
	 * yields ownership of the monitor when calling {@link Object#wait() wait}.  
	 */
	protected synchronized void stateChangedNotify() {
	    
        logger.finest("stateChangedNotify: called in thread '" + Thread.currentThread().getName() + "'.");
	    
		notifCount++;
		if (notifCount >= minCount) {
			// release thread that blocks on waitForStateChanges
			notifyAll();
		}
	}

	
	/**
	 * Blocks until the specified number of state change notifications have been received 
	 * since last call to {@link #reset()}.
	 * <p> 
	 * Does not block at all if at least <code>count</code> state change notifications 
	 * have been received since the last call to <code>reset</code>. Otherwise only blocks until 
	 * the missing number of notifications has arrived.
	 * Before returning, this method subtracts <code>count</code> from the internal counter for state change notifications, 
	 * which allows a client to catch up with fast firing event notifications by calling this method several times.
	 * <p>
	 * Note that the "synchronized" modifier is required in order for the client thread
	 * to obtain ownership of this semaphore (i.e. its monitor), 
	 * without which the thread synchronization would not work.
	 * See {@link Object#notify()} for an explanation.
	 *  
	 * @param count  number of state change notifications that must be received 
	 *               since last call to {@link #reset()} 
	 *               so that this call will return
	 * @throws InterruptedException
	 */
	public synchronized void waitForStateChanges(int count) throws InterruptedException {
    	logger.finest("waitForStateChanges: called in thread '" + Thread.currentThread().getName() + "'.");
	    
		minCount = count;
		// was stateChangedNotify called sufficiently often already? 
		while (notifCount < count) {
	    	logger.finest("waitForStateChanges: waiting for " + (count - notifCount) + " state change notification(s)");
		    // releases current thread's ownership of this StateChangeSemaphore's monitor, and blocks until other thread calls notify
			wait(); 
			// now our thread has re-obtained ownership of this StateChangeSemaphore's monitor, so the field notifCount is again ours. 
		}
		notifCount = notifCount - count;
    	logger.finest("waitForStateChanges: state change notification counter down at " + notifCount);
	}

	
	/**
	 * Resets the state change notification count.
	 * Must be called prior to sending a state event.
	 */
	public synchronized void reset() {
        notifCount = 0;
	}

}
