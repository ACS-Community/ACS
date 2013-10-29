/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
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

package alma.alarmsystem.clients;

import org.omg.CORBA.ORB;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionHandler;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.Filter;
import cern.laser.client.services.selection.LaserSelectionException;
import alma.acs.container.ContainerServices;
import alma.acs.logging.AcsLogger;
import alma.alarmsystem.Category;
import alma.alarmsystem.clients.alarm.AlarmCategoryStatListener;
import alma.alarmsystem.clients.alarm.AlarmClientException;
import alma.alarmsystem.clients.alarm.AlarmFilter;
import alma.alarmsystem.clients.alarm.AlarmListenersContainer;
import alma.alarmsystem.clients.alarm.AlarmListenersContainer.AlarmListener;
import alma.alarmsystem.clients.alarm.AlarmStatistics;
import alma.maciErrType.wrappers.AcsJCannotGetComponentEx;

/**
 * {@link AlarmCategoryClient} listens to alarms produced by the alarm server,
 * by means of a {@link CategoryClient} and offers greater functionalities to listeners like:
 * <UL>
 * 	<LI>Filtering of alarms based on the value of their triplets
 * 	<LI>Notification about the number of active alarms for each priority
 * </UL>
 * <P>
 * After instantiating a object of this class, the clients should 
 * register in order to be notified with callbacks and only after
 * call {@link #connect()} or {@link #connect(Category[])}.
 * <BR>{@link #close()}must be called when finished using this object 
 * to ensure the correct cleanup of the allocated resources.
 * <P>
 * {@link AlarmCategoryClient} has been introduced after ICT-547 offering 
 * more functionality of what has been requested in the ticket to be ready 
 * for next generation of user needs. 
 * As a such, its API will most likely expanded in the future
 * to cope with new requirements.
 * <P>
 * Note that it is possible to add filtering capabilities while
 * connecting to the categories by properly setting {@link Filter} to 
 * the {@link AlarmSelectionHandler}. However a quick look to filtering
 * seems to suggest that it is based on SQL because CERN AS originally
 * persisted alarms in a database. In out implementation all the alarms are
 * in memory and therefore we can't get advantage of SQL for filtering.
 *  
 * @author  acaproni
 * @since   ACS-12.2
 */
public class AlarmCategoryClient implements AlarmSelectionListener {

	/**
	 * The {@link CategoryClient} to listen to alarms from the alarm server
	 * and on top of which, objects of this class offer more functionalities
	 */
	private final CategoryClient client;
	
	/**
	 * Signal that the object has been closed
	 */
	private volatile boolean closed=false;
	
	/**
	 * The container with all the listeners
	 */
	private final AlarmListenersContainer listenersContainer = new AlarmListenersContainer();
	
	/**
	 * The statistics elaborated on the category alarms processed
	 */
	private final AlarmStatistics statistics = new AlarmStatistics();

	/**
	 * Constructor
	 * 
	 * @param contServices The containerServices
	 * @throws AlarmClientException
	 * @see {{@link #AlarmCategoryClient(ContainerServices)}
	 */
	public AlarmCategoryClient(ContainerServices contServices)
			throws AlarmClientException {
		this(contServices.getAdvancedContainerServices().getORB(),contServices.getLogger());
	}

	/**
	 * Contructor
	 * 
	 * @param orb The orb
	 * @param logger The logger
	 * @see {{@link #AlarmCategoryClient(ORB, AcsLogger)}
	 */
	public AlarmCategoryClient(ORB orb, AcsLogger logger) {
		client = new CategoryClient(orb,logger);
	}
	
	/**
	 * Connects to all the categories of the alarm system.
	 * 
	 * It is equivalent to <code>connect(null)</code>. 
	 *  
	 * @throws AlarmClientException In case of failure connecting the client
	 * @throws AcsJCannotGetComponentEx If the alarm service component is not available
	 * 
	 * @see CategoryClient#connect(AlarmSelectionListener, Category[])
	 */
	public void connect() throws AlarmClientException, AcsJCannotGetComponentEx {
		connect(null);
	}
	
	/**
	 * Connects to the passed categories of the alarm system
	 * 
	 * @param categories The categories to connect to
	 * 
	 * @throws AcsJCannotGetComponentEx In case the AlarmService is not available
	 * @throws AlarmClientException In case of failure connecting the client
	 * 
	 * @see CategoryClient#connect(AlarmSelectionListener, Category[])
	 */
	public void connect(Category[] categories) throws AlarmClientException, AcsJCannotGetComponentEx {
		client.connect(this, categories);
	}
	
	/**
	 * Release all the resource,
	 * 
	 * @throws AlarmClientException 
	 */
	public void close() throws AlarmClientException {
		closed=true;
		listenersContainer.close();
		client.close();
	}
	
	/**
	 * This method is executed whenever a new alarm is received.
	 * 
	 * @see AlarmSelectionListener
	 */
	@Override
	public void onAlarm(Alarm alarm) {
		if (closed) {
			return;
		}
		listenersContainer.dispatchAlarm(alarm);
		statistics.update(alarm);
		listenersContainer.dispatchStatistics(statistics);
	}

	/**
	 * This method is executed whenever a new exception happens.
	 * 
	 * 
	 * @see AlarmSelectionListener
	 */
	@Override
	public void onException(LaserSelectionException e) {
		if (closed) {
			return;
		}
		listenersContainer.dispatchException(e);
	}

	/**
	 * @see {@link CategoryClient#getParents(String, boolean)}
	 */
	public Alarm[] getParents(String id, boolean node)
			throws AlarmClientException {
		return client.getParents(id, node);
	}

	/**
	 * @see {@link CategoryClient#getChildren(String, boolean)} 
	 */
	public Alarm[] getChildren(String id, boolean node)
			throws AlarmClientException {
		return client.getChildren(id, node);
	}

	/**
	 * @see {@link CategoryClient#getMultiplicityThreshold(String)} 
	 */
	public int getMultiplicityThreshold(String id) throws AlarmClientException {
		return client.getMultiplicityThreshold(id);
	}

	/**
	 * @see {@link CategoryClient#getActiveChildren(String, boolean)} 
	 */
	public Alarm[] getActiveChildren(String id, boolean node)
			throws AlarmClientException {
		return client.getActiveChildren(id, node);
	}

	/**
	 * @see AlarmListenersContainer#addAlarmListener(AlarmListener)
	 */
	public boolean addAlarmListener(AlarmListener listener) {
		return listenersContainer.addAlarmListener(listener);
	}

	/**
	 * @see AlarmListenersContainer#addAlarmListener(AlarmSelectionListener, AlarmFilter)
	 */
	public AlarmListener addAlarmListener(AlarmSelectionListener listener,
			AlarmFilter filter) {
		return listenersContainer.addAlarmListener(listener, filter);
	}

	/**
	 * @see AlarmListenersContainer#addAlarmListener(AlarmSelectionListener)
	 */
	public AlarmListener addAlarmListener(AlarmSelectionListener listener) {
		return listenersContainer.addAlarmListener(listener);
	}

	/**
	 * @see AlarmListenersContainer#removeAlarmListener(AlarmListener)
	 */
	public boolean removeAlarmListener(AlarmListener listenerToRemove) {
		return listenersContainer.removeAlarmListener(listenerToRemove);
	}

	/**
	 * @see AlarmListenersContainer#addStatsListener(AlarmCategoryStatListener)
	 */
	public boolean addStatsListener(AlarmCategoryStatListener listener) {
		return listenersContainer.addStatsListener(listener);
	}

	/**
	 * @see AlarmListenersContainer#removeStatListener(AlarmCategoryStatListener)
	 */
	public boolean removeStatListener(AlarmCategoryStatListener listenerToRemove) {
		return listenersContainer.removeStatListener(listenerToRemove);
	}
	
}
