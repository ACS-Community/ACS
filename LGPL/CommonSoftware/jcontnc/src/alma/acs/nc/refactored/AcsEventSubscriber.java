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

package alma.acs.nc.refactored;

import org.omg.CORBA.portable.IDLEntity;
import org.omg.CosNotifyComm.InvalidEventType;
import org.omg.CosNotifyFilter.FilterNotFound;

import alma.acs.exceptions.AcsJException;
import alma.acsnc.EventDescription;

/**
 * This interface provides an event subscriber, 
 * that can be implemented over Corba NC or over DDS, or even other technologies.
 * <p>
 * Discussion:
 * <ul>
 *   <li>One AcsEventSubscriber object may subscribe to different types of events,
 *       which for a Corba NC is normal, but for DDS requires tricks with topics and partitions.
 *   <li>Alternatively, we could make this class generic and fix a single event type for the entire object (interface),
 *       so that the java compiler would not allow to add a subscription for a different type.
 *   <li>TODO: The declared exceptions must be cleaned up. Rather no generic AcsJException, and no Corba-NC-specific exceptions.
 *   <li>TODO: We need to check if this API actually works for DDS.
 *   <li>An in-memory shortcutting (simulated channel/topic) could be useful, for testing without external processes running.
 * </ul>
 * @author jslopez
 */
public interface AcsEventSubscriber {
	
	public void addSubscription(Callback<? extends IDLEntity> receiver) 
		throws AcsJException;

	public void removeSubscription(Class<? extends IDLEntity> structClass) 
		throws AcsJException, FilterNotFound, InvalidEventType;

	public void addGenericSubscription(GenericCallback receiver);
	
	public void removeGenericSubscription() throws AcsJException;

	public void startReceivingEvents() throws AcsJException;
	
	public void disconnect();

	public void suspend();

	public void resume();

	
	/**
	 * This ACS-defined interface replaces the runtime search for the
	 * "receive(...)" method that works with Java introspection.
	 */
	public static interface Callback<T extends IDLEntity> {
		
		public void receive(T event, EventDescription eventDescrip);
		
		public Class<T> getEventType();
	}
	
	/**
	 * This ACS-defined interface replaces the runtime search for the
	 * "receive(...)" method that works with Java introspection.
	 */
	public static interface GenericCallback {
		public void receive(IDLEntity event, EventDescription eventDescrip);
	}

}