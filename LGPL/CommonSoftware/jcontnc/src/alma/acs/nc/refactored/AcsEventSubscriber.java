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
 * This interface provides an Event Subscriber.
 * @TODO Discuss this interface
 * 
 * @author jslopez
 */
public interface AcsEventSubscriber <T extends IDLEntity> {
	public void addSubscription(Class<T> structClass, Callback<T> receiver) throws AcsJException;

	public void removeSubscription(Class<T> structClass) 
		throws AcsJException, FilterNotFound, InvalidEventType;

	public void startReceivingEvents() throws AcsJException;
	
	public void addGenericSubscription(GenericCallback receiver);
	
	public void removeGenericSubscription() throws AcsJException;

	public void disconnect();

	public void suspend();

	public void resume();

	/**
	 * This ACS-defined interface replaces the runtime search for the
	 * "receive(...)" method that works with Java introspection.
	 */
	public static interface Callback<T> {
		public void receive(T event, EventDescription eventDescrip);
	}
	
	/**
	 * This ACS-defined interface replaces the runtime search for the
	 * "receive(...)" method that works with Java introspection.
	 */
	public static interface GenericCallback {
		public void receive(IDLEntity event, EventDescription eventDescrip);
	}

}