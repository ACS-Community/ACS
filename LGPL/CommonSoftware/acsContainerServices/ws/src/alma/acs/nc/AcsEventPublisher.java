/*
* ALMA - Atacama Large Millimiter Array
* (c) European Southern Observatory, 2009 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/

package alma.acs.nc;

import alma.acs.exceptions.AcsJException;

/**
 * This interface provides an abstraction for an Event Publisher.
 * It can be used both for Corba NC events and in the future also for DDS events.
 * 
 * @author rtobar
 */
public interface AcsEventPublisher<T> {

	/**
	 * Publishes an event through the Notification Channel to which
	 * this publisher is connected to.
	 * (In the future, ACS may use other mechanisms than Corba NC for pub/sub.)
	 *
	 * @param customStruct The structure to send through the Notification Channel
	 * @throws AcsJException In case of any failure, including if the publisher
	 *   is not yet connected (or has been disconnected) to the Notification Channel.
	 */
	public void publishEvent(T customStruct) throws AcsJException;

	/**
	 * Disconnect this Publisher from the Notification Channel.
	 * This method must be called when the publisher object is no longer needed.
	 *
	 * @throws IllegalStateException If the current Publisher is already
	 * disconnected
	 */
	public void disconnect() throws IllegalStateException;

	
	/**
	 * Handler for optional callbacks to the event publishing client,
	 * which allows notifying the client of success or failure with publishing the event(s).
	 * <p>
	 * This handler makes sense only in conjunction with using an event queue in the publisher,
	 * because without a queue the user notices directly the successful or failed sending of an event 
	 * by either a normal return or an exception from method {@link AcsEventPublisher#publishEvent(Object)}.
	 * <p>
	 * The user is responsible to implement the handler methods.
	 * The handler methods must return quickly and should not throw exceptions.
	 * 
	 * @see AcsEventPublisher#registerEventProcessingCallback(EventProcessingHandler)
	 */
	public static interface EventProcessingHandler<U> {
		/**
		 * Notification that an event was sent.
		 * Depending on the underlying pub-sub framework and its configuration, this may or may not 
		 * imply that the subscriber(s) will also receive the event.
		 * @param event The event that was sent.
		 */
		public void eventSent(U event);
		
		/**
		 * Notification that an event was stored in a local queue
		 * after some problem occurred when trying to send the event immediately.
		 * @param event The event that was stored in the queue.
		 */
		public void eventStoredInQueue(U event);
		
		/**
		 * Notification that an event was dropped because of failures and a full queue.
		 * @param event The event that was dropped.
		 */
		public void eventDropped(U event);
	}

	/**
	 * Enables using a queue for published events. When using a queue, a handler must be registered
	 * so that the user learns about success or failure.
	 * <p>
	 * As of ACS 10.1, only a missing NotifyService as diagnosed through 
	 * a <code>org.omg.CORBA.TRANSIENT</code> exception will cause the publisher
	 * to store an event in this queue for later re-sending.
	 * @param queueSize Number of events that this queue should store.
	 *        The choice is a tradeoff between memory use and data loss.
	 * @param handler  The handler that should be notified by the publisher.
	 */
	public void enableEventQueue(int queueSize, EventProcessingHandler<T> handler);
	
}

