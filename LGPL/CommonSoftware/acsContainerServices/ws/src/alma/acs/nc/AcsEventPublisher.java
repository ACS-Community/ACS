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
 *
 * @TODO Discuss this interface, and also the exceptions. For some more details, check
 *  the TODOs described in {@link AcsEventSubscriber}.
 * 
 * @author rtobar
 */
public interface AcsEventPublisher {

	/**
	 * Publishes a structured event through the Notification Channel to which
	 * this publisher is connected to.
	 *
	 * @param customStruct The structure to send through the Notification Channel
	 * @throws AcsJException In case of any failure, including if the publisher
	 *   is not yet connected (or has been disconnected) to the Notification Channel.
	 */
	public void publishEvent(Object customStruct) throws AcsJException;

	/**
	 * Disconnect this Publisher from the Notification Channel.
	 *
	 * @throws IllegalStateException If the current Publisher is already
	 * disconnected
	 */
	public void disconnect() throws IllegalStateException;

}