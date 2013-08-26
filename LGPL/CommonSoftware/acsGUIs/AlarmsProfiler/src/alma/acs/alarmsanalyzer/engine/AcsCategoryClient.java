/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2010
 * 
 * This library is free software; you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation; either version 2.1 of the License, or (at your option)
 * any later version.
 * 
 * This library is distributed in the hope that it will be useful, but WITHOUT
 * ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 * FOR A PARTICULAR PURPOSE. See the GNU Lesser General Public License for more
 * details.
 * 
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library; if not, write to the Free Software Foundation, Inc.,
 * 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
 * 
 */
package alma.acs.alarmsanalyzer.engine;

import java.util.Vector;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserSelectionException;
import alma.acs.container.ContainerServices;
import alma.alarmsystem.clients.CategoryClient;
import alma.alarmsystem.clients.alarm.AlarmClientException;

/**
 * Extend {@link CategoryClient} with a set of useful methods
 * 
 * @author acaproni
 *
 */
public class AcsCategoryClient extends CategoryClient implements AlarmSelectionListener {
	
	/**
	 * The listeners of alarms
	 */
	private Vector<AlarmCategoryListener> listeners = new Vector<AlarmCategoryListener>();
	
	/**
	 * Constructor
	 * 
	 * @param contServices The container services
	 */
	public AcsCategoryClient(ContainerServices contServices) throws AlarmClientException {
		super(contServices);
	}
	
	/**
	 * Add a new listener 
	 * 
	 * @param listener The listener to add
	 */
	public synchronized void addListener(AlarmCategoryListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		if (!listeners.contains(listener)) {
			listeners.add(listener);
		}
	}
	
	/**
	 * Remove a listener 
	 * 
	 * @param listener The listener to add
	 */
	public synchronized void removeListener(AlarmCategoryListener listener) {
		if (listener==null) {
			throw new IllegalArgumentException("The listener can't be null");
		}
		listeners.remove(listener);
	}
	
	/**
	 * Connect to the alarm service
	 */
	public void connect() throws Exception {
		super.connect(this);
		System.out.println("Category ClientConnected");
	}

	/**
	 * Receive and dispatch alarms
	 */
	@Override
	public synchronized void onAlarm(Alarm alarm) {
		for (AlarmCategoryListener listener: listeners) {
			listener.onAlarm(alarm);
		}
	}

	@Override
	public void onException(LaserSelectionException e) {
		// TODO Auto-generated method stub
		
	}
	
	
	
}
