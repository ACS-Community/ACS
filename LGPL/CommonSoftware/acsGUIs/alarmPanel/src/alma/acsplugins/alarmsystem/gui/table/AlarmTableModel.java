/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2007
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

/** 
 * @author  acaproni   
 * @version $Id: AlarmTableModel.java,v 1.25 2009/10/13 09:17:50 acaproni Exp $
 * @since    
 */

package alma.acsplugins.alarmsystem.gui.table;

import javax.swing.JComponent;
import javax.swing.JOptionPane;
import javax.swing.table.AbstractTableModel;

import alma.acs.util.IsoDateFormat;
import alma.acsplugins.alarmsystem.gui.ConnectionListener;
import alma.acsplugins.alarmsystem.gui.toolbar.Toolbar.ComboBoxValues;
import alma.alarmsystem.clients.CategoryClient;

import cern.laser.client.data.Alarm;
import cern.laser.client.services.selection.AlarmSelectionListener;
import cern.laser.client.services.selection.LaserHeartbeatException;
import cern.laser.client.services.selection.LaserSelectionException;

import java.awt.Color;
import java.text.SimpleDateFormat;
import java.util.HashMap;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.Semaphore;

/** 
 * 
 * The table model for the table alarms
 *
 */
public class AlarmTableModel extends AbstractTableModel implements AlarmSelectionListener, Runnable {
	
	/**
	 * The title of each column.
	 * 
	 * To change the initial order of the columns, change the order of the
	 * declaration of this enumerated.
	 * 
	 * @author acaproni
	 *
	 */
	public enum AlarmTableColumn {
		HIDES_CHILDREN("","Parent node flag",true), // The entry hides children because of RR
		REDUCED("","Reduced flag",true), // The entry is reduced
		ICON("","Not acknowledged flag",true), // The flag
		TIME("Time",null,true),
		COMPONENT("Component",null,true),
		FAMILY("Family",null,true),
		CODE("Code",null,false),
		CAUSE("Cause",null,true),
		DESCRIPTION("Description",null,true),
		ACTION("Action",null,true),
		PRIORITY("Priority",null,true),
		CONSEQUENCE("Consequence",null,false),
		URL("URL",null,false),
		CONTACT("Contact",null,false),
		EMAIL("email",null,false),
		GSM("GSM",null,false),
		TRIPLET("Triplet",null,false);
		
		/**
		 *  The title of the column as it appears in the table header
		 */
		public final String title;
		
		/**
		 *  The title of the column as it appears in the table header popup menu
		 */
		public final String popupTitle;
		
		/**
		 *  If <code>true</code> the column is shown at startup
		 */
		public boolean visibleAtStartup;
		
		/**
		 * Constructor
		 * 
		 * @param title The name of the column in the table header
		 * @param popupTitle The name of the column in the popup menu;
		 * 					 if it is <code>null</code> then it is set to be equal to <code>title</code>
		 * @param initiallyVisible if <code>true</code> the column is visible at startup
		 */
		private AlarmTableColumn(String title, String popupTitle, boolean initiallyVisible) {
			this.title=title;
			this.popupTitle= (popupTitle==null) ? title : popupTitle;
			this.visibleAtStartup=initiallyVisible;
		}
			
	};
	
	public static enum PriorityLabel {
		VERY_HIGH("VERY HIGH",Color.red),
		HIGH("HIGH", new Color(255,165,31)),
		MEDIUM("MEDIUM",Color.yellow),
		LOW("LOW",new Color(188,255,188));
		
		/**
		 * The description label
		 */
		public final String description;
		
		/**
		 * The color of the entry in the alarm table
		 */
		public final Color color;
		
		/**
		 * Constructor
		 * 
		 * @param desc The description
		 * @param col The color
		 */
		private PriorityLabel(String desc, Color col) {
			description=desc;
			color=col;
		}
		
		/**
		 * 
		 * @param priority The priority
		 * @return The {@link PriorityLabel} of the given priority
		 */
		public static PriorityLabel fromPriorityNumber(int priority) {
			switch (priority) {
			case 0: return VERY_HIGH;
			case 1: return HIGH;
			case 2: return MEDIUM;
			case 3: return LOW;
			default:
				throw new IndexOutOfBoundsException("Invalid priority");
			}
		}
		
		/**
		 * @param n The number of the priority
		 * @return The description of the priority
		 */
		public static String fromPriorityDesc(int n) {
			return fromPriorityNumber(n).description;
		}
	}
	
	/** 
	 * The date format
	 */
	private SimpleDateFormat dateFormat = new IsoDateFormat();
	
	/**
	 *  The counter for the alarms
	 */
	private HashMap<AlarmGUIType, AlarmCounter> counters = new HashMap<AlarmGUIType, AlarmCounter>();
	
	/**
	 * The listener about the status of the connection
	 * 
	 * @see <code>onException</code>
	 */
	private ConnectionListener connectionListener=null;
	
	/** 
	 * The queue of alarms received from the <code>CategoryClient</code> that will be
	 * injected in the table
	 */
	private LinkedBlockingQueue<AlarmTableEntry> queue = new LinkedBlockingQueue<AlarmTableEntry>(QUEUE_SIZE);
	
	/**
	 * The semaphore used to pause the thread
	 * <P>
	 * When the application is not paused, the thread acquire the semaphore
	 * before getting an alarm from the queue and release it when done.
	 * <BR>
	 * The pause method acquires the semaphore blocking the thread.
	 * When the application is upaused then the semaphore is released and the thread restarts.
	 * 
	 */
	private Semaphore paused=new Semaphore(1);
	
	/**
	 * Signal the thread to terminate
	 */
	private volatile boolean terminateThread=false;
	
	/**
	 * The thread
	 */
	private final Thread thread;
	
	/**
	 * Constructor
	 * 
	 * @param owner The component that owns the table
	 * @param reduce <code>true</code> if the reduction rules must be applied
	 * @param panel The <code>AlarmPanel</code>
	 */
	public AlarmTableModel(JComponent owner, boolean reduce) {
		if (owner==null) {
			throw new IllegalArgumentException("The owner component can't be null");
		}
		this.owner=owner;
		this.applyReductionRules=reduce;
		this.items = new AlarmsReductionContainer(MAX_ALARMS);
		// Put each alarm type in the has map of the counters
		for (AlarmGUIType alarmType: AlarmGUIType.values()) {
			counters.put(alarmType, new AlarmCounter());
		}
		// Start the thread
		thread = new Thread(this,"AlarmTableModel");
		thread.setDaemon(true);
		thread.start();
	}
	
	/**
	 * Add an alarm in the queue.
	 * The thread will get the alarm from the queue and update the model.
	 * 
	 * @param alarm The alarm to add to the table.
	 * @see AlarmSelectionListener
	 */
	public synchronized void onAlarm(Alarm alarm) {
		//System.out.println("Alarm received: <"+alarm.getAlarmId()+">");
		//System.out.println("\tisNodeChild="+alarm.isNodeChild()+". isNodeParent="+alarm.isNodeParent());
		AlarmTableEntry tableEntry = new AlarmTableEntry(alarm);
		// Add the alarm to the queue
		if (waitIfQueueFull) {
			// Wait if the queue is full
			while (!terminateThread) {
				try {
					queue.put(tableEntry);
				} catch (InterruptedException e) {
					continue;
				}
				break;
			}
		} else {
			// Does not care if the queue is full
			queue.offer(tableEntry);
		}
	}
	
	/**
	 * @param alarm The alarm to add
	 */
	private void addAlarm(AlarmTableEntry alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		if (!alarm.getStatus().isActive()) {
			// do not add inactive alarms
			return;
		}
		
		try {
			items.add(alarm);
		} catch (Exception e) {
			System.err.println("Error adding an alarm: "+e.getMessage());
			e.printStackTrace(System.err);
			JOptionPane.showInternalMessageDialog(
					owner, 
					e.getMessage(), 
					"Error adding alarm", 
					JOptionPane.ERROR_MESSAGE);
			return;
		}
		counters.get(alarm.getAlarmType()).incCounter();
	}
	
	/**
	 * Automatically acknowledge an alarm depending on its 
	 * priority and the selected priority level
	 * 
	 * @param alarm The alarm to acknowledge if its priority
	 *              if greater the the selected priority level
	 */
	private void autoAcknowledge(AlarmTableEntry alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		if (alarm.getStatus().isActive()) {
			throw new IllegalArgumentException("Trying to acknowledge an active alarm");
		}
		if (autoAckLvl==ComboBoxValues.NONE) {
			return;
		}
		
		int priority=999999; // Big enough 
		switch (autoAckLvl) {
		case PRIORITY1: {
			priority = 1;
			break;
		}
		case PRIORITY2: {
			priority = 2;
			break;
		}
		case PRIORITY3: {
			priority = 3;
			break;
		}
		}
		if (alarm.getPriority()>=priority) {
			acknowledge(alarm);
		}
	}
	
	/**
	 * Acknowledge an alarm that in this version ends up to removing
	 * from the table
	 * 
	 * @param alarm The inactive alarm to acknowledge
	 */
	public synchronized void acknowledge(AlarmTableEntry alarm) {
		if (alarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		if (alarm.getStatus().isActive()) {
			throw new IllegalArgumentException("Trying to acknowledge an active alarm");
		}
		// Remove the alarm from the table
		try {
			synchronized (items) {
				items.remove(alarm);	
			}
		} catch (Exception e) {
			System.err.println("Error removing an alarm: "+e.getMessage());
			e.printStackTrace(System.err);
			JOptionPane.showInternalMessageDialog(
					owner, 
					e.getMessage(), 
					"Error removing alarm", 
					JOptionPane.ERROR_MESSAGE);
			return;
		}
		counters.get(AlarmGUIType.fromAlarm(alarm)).decCounter();
		fireTableDataChanged();
	}
	
	/**
	 * Replace an alarm already in the table
	 * 
	 * @param newAlarm The alarm to put in the table
	 */
	private void replaceAlarm(AlarmTableEntry newAlarm) {
		if (newAlarm==null) {
			throw new IllegalArgumentException("The alarm can't be null");
		}
		AlarmTableEntry oldAlarmEntry = items.get(newAlarm.getAlarmId());
		AlarmGUIType oldAlarmType = oldAlarmEntry.getAlarmType();
		boolean oldAlarmStatus = oldAlarmEntry.getStatus().isActive();
		try {
			synchronized (items) {
				items.replace(newAlarm);
			}
		} catch (Exception e) {
			System.err.println("Error replacing an alarm: "+e.getMessage());
			e.printStackTrace(System.err);
			JOptionPane.showInternalMessageDialog(
					owner, 
					e.getMessage(), 
					"Error replacing alarm", 
					JOptionPane.ERROR_MESSAGE);
			return;
		}
		if (oldAlarmStatus==newAlarm.getStatus().isActive()) {
			return;
		}
		// Update the counters
		counters.get(oldAlarmEntry.getAlarmType()).incCounter();
		counters.get(oldAlarmType).decCounter();
		if (!newAlarm.getStatus().isActive()) {
			// The alarm became INACTIVE
			autoAcknowledge(newAlarm);
		}
	}

	/**
	 * Get exception from the client.
	 * A message is notified to the listener or written in the
	 * standard output if the listener is <code>null</code>.
	 * 
	 * @see AlarmSelectionListener
	 */
	public void onException(LaserSelectionException e) {
		if (connectionListener==null) {
			System.err.println("Exception: "+e.getCode());
			return;
		}
		if (e.getCode().equals(LaserHeartbeatException.HEARTBEAT_LOST)) {
			connectionListener.heartbeatLost();
		} else if (e.getCode().equals(LaserHeartbeatException.HEARTBEAT_RECONNECTED)) {
			connectionListener.connected();
		} else if (e.getCode().equals(LaserHeartbeatException.CONNECTION_DROPPED)) {
			connectionListener.disconnected();
		} else if (e.getCode().equals(LaserHeartbeatException.CONNECTION_REESTABILISHED)) {
			connectionListener.connected();
		} else {
			// Unrecognized code
			System.err.println("Exception: "+e.getCode());
			e.printStackTrace();
		}
	}

	/**
	 * The max number of alarms in the table
	 * When the max has been reach, the oldest alarm is removed 
	 * before adding a new one
	 */
	public static final int MAX_ALARMS=20000;
	
	/**
	 * The max alarm in queue when the table is paused
	 */
	public static final int QUEUE_SIZE=15000;
	
	/**
	 * The behavior if the queue is full.
	 * 
	 * If it is <code>true</code> when a new alarm arrives and the queue is full then 
	 * it waits until there is one free place in the queue.
	 * 
	 * If it is <code>false</code> and the queue is full then the incoming alarm will be discarded.
	 */
	private boolean waitIfQueueFull = false;
	
	/**
	 * The owner component (used to show dialog messages) 
	 */
	private JComponent owner;
	
	/**
	 * The alarms in the table
	 */
	private AlarmsReductionContainer items=null;
	
	/**
	 *	If <code>true</code> applies the reduction rules hiding reduced alarms 
	 */
	private boolean applyReductionRules;
	
	/**
	 * The auto acknowledge level
	 */
	private ComboBoxValues autoAckLvl = ComboBoxValues.NONE ;

	public int getRowCount() {
		synchronized (items) {
			return items.size(applyReductionRules);
		}
	}

	public int getColumnCount() {
		return AlarmTableColumn.values().length;
	}
	
	/**
	 * Return the alarm shown at the rowIndex row of the table.
	 * 
	 * @param rowIndex The index of the alarm in the model
	 * @return the alarm shown at the rowIndex row of the table
	 */
	public AlarmTableEntry getAlarmAt(int rowIndex) {
		if (rowIndex<0) {
			return null;
		}
		AlarmTableEntry alarm;
		synchronized (items) {
			alarm = items.get(rowIndex,applyReductionRules);
		}
		return alarm;
	}
	
	/**
	 * Return the text to display in a cell as it is read by the alarm
	 * without any formatting (the table add some formatting for
	 * example the color)
	 * 
	 * @param rowIndex The row of the cell
	 * @param columnIndex The col of the cell
	 * @return The string to display in the cell
	 */
	public Object getCellContent(int rowIndex, int columnIndex) {
		AlarmTableEntry alarm=getAlarmAt(rowIndex);
		
		AlarmTableColumn col = AlarmTableColumn.values()[columnIndex];
		switch (col) {
		case TIME: {
			return dateFormat.format(alarm.getStatus().getSourceTimestamp());
		}
		case COMPONENT: {
			return alarm.getTriplet().getFaultMember();
		}
		case CODE: {
			return alarm.getTriplet().getFaultCode();
		}
		case PRIORITY: {
			int priority = alarm.getPriority().intValue();
			return PriorityLabel.fromPriorityDesc(priority);
		}
		case DESCRIPTION: {
			return alarm.getProblemDescription();
		}
		case CAUSE: {
			return alarm.getCause();
		}
		case ACTION: {
			return alarm.getAction();
		}
		case CONSEQUENCE: {
			return alarm.getConsequence();
		}
		case GSM: {
			return alarm.getResponsiblePerson().getGsmNumber();
		}
		case CONTACT: {
			return alarm.getResponsiblePerson().getFirstName();
		}
		case EMAIL: {
			return alarm.getResponsiblePerson().getEMail();
		}
		case URL: {
			return alarm.getHelpURL();
		}
		case TRIPLET: {
			return "["+alarm.getTriplet().getFaultFamily()+", "+alarm.getTriplet().getFaultMember()+", "+alarm.getTriplet().getFaultCode()+"]";
		}
		case FAMILY: {
			return alarm.getTriplet().getFaultFamily();
		}
		default: {
				return "N/A";
			}
		}
	}
	
	/**
	 * Set the auto acknowledge level
	 * i.e. All the inactive alarms having a level equal or lower
	 * the the passed level automatically disappear from the table
	 * (i.e. with no user intervention)
	 * 
	 * @param lvl The new auto acknowledge level
	 */
	public void setAutoAckLevel(ComboBoxValues lvl) {
		if (lvl==null) {
			throw new IllegalArgumentException("The level can't be null");
		}
		autoAckLvl=lvl;
	}

	/**
	 * @see javax.swing.table.AbstractTableModel
	 */
	public Object getValueAt(int rowIndex, int columnIndex) {
		return getCellContent(rowIndex, columnIndex);
	}
	
	@Override
	public String getColumnName(int col) {
		return AlarmTableColumn.values()[col].title;
	}
	
	/**
	 * Return the alarm whose content fills the given row
	 * 
	 * @param row The number of the row showing the alarm
	 * @return The alarm shown in the row
	 */
	public AlarmTableEntry getRowAlarm(int row) {
		return getRowEntry(row);
	}
	
	/**
	 * Return the entry the given row
	 * 
	 * @param row The number of the row showing the alarm
	 * @return The entry 
	 */
	public AlarmTableEntry getRowEntry(int row) {
		synchronized (items) {
			if (row<0 || row>=items.size(applyReductionRules)) {
				throw new IllegalArgumentException("Invalid row: "+row+" not in [0,"+items.size(applyReductionRules)+"[");
			}
			return items.get(row,applyReductionRules);
		}
	}
	
	/**
	 * 
	 * @param row Return true if the alarm is new
	 */
	public boolean isRowAlarmNew(int row) {
		synchronized (items) {
			if (row<0 || row>=items.size(applyReductionRules)) {
				throw new IllegalArgumentException("Invalid row: "+row+" not in [0,"+items.size(applyReductionRules)+"[");
			}
			return items.get(row,applyReductionRules).isNew();
		}
	}
	
	/**
	 * Return the counter for the given alarm type
	 * 
	 * @param type The type of the alarm
	 * @return The counter for the alarm type
	 */
	public AlarmCounter getAlarmCounter(AlarmGUIType type) {
		if (type==null) {
			throw new IllegalArgumentException("The alarm type can't be null");
		}
		AlarmCounter ret = counters.get(type);
		if (ret==null) {
			throw new IllegalStateException("A counter for the type "+type+"does not exist");
		}
		return ret;
	}
	
	/**
	 * The user pressed one mouse button over a row
	 */
	public void alarmSelected(int row) {
		synchronized (items) {
			items.get(row,applyReductionRules).alarmSeen();
		}
		fireTableRowsUpdated(row, row);
	}
	
	/**
	 * Remove all the inactive alarms of a given type
	 * delegating to the AlarmsContainer.
	 * If the type is <code>INACTIVE</code> all inactive alarms are deleted
	 * regardless of their priority
	 * 
	 * @param type The type of the inactive alarms
	 * 
	 * @see AlarmsContainer.removeInactiveAlarms
	 */
	public synchronized void removeInactiveAlarms(AlarmGUIType type) {
		if (type==null) {
			throw new IllegalArgumentException("The type can't be null");
		}
		System.out.println("removing inactive alarms of type "+type);
		int removed=0;
		try {
			removed=items.removeInactiveAlarms(type);
		} catch (Exception e) {
			System.out.println("Error removing inactive alarms "+e.getMessage());
			e.printStackTrace(System.err);
		}
		if (removed>0) {
			for (int t=0; t<removed; t++) {
				counters.get(AlarmGUIType.INACTIVE).decCounter();
			}
			fireTableDataChanged();
		}
	}
	
	/**
	 * Set the connection listener
	 * 
	 * @param listener The listener
	 */
	public void setConnectionListener(ConnectionListener listener) {
		connectionListener=listener;
	}
	
	/**
	 * Enable/disable the applying of reduction rules in the table.
	 * <P>
	 * by applying reduction rules, the table will not show reduced alarms.
	 * 
	 * @param reduce if <code>true</code> apply the reduction rules hiding reduced alarms;
	 * 				if <code>reduce</code> is <code>false</code> all the alarms are shown 
	 * 				by the table independently of the reduction rules
	 */
	public void applyReductions(boolean reduce) {
		applyReductionRules=reduce;
		fireTableDataChanged();
	}
	
	/**
	 * Set the <code>CategoryClient</code> in the <code>AlarmsContainer</code>
	 * 
	 * @param client The <code>CategoryCLient</code>; it can be <code>null</code>.
	 */
	public void setCategoryClient(CategoryClient client) {
		items.setCategoryClient(client);
	}
	
	/**
	 * Get the <code>CategoryClient</code> from the <code>AlarmsContainer</code>
	 * 
	 * @param client The <code>CategoryCLient</code>; it can be <code>null</code>.
	 */
	public CategoryClient getCategoryClient() {
		return items.getCategoryClient();
	}
	
	/**
	 * Clear the content of the model
	 */
	public synchronized void clear() {
		items.clear();
	}
	
	/**
	 * Pause/un-pause the update of the table
	 * <P>
	 * If it is paused then the alarms received in <code>onAlarm</code>
	 * are not added in the model but queued until the application is unpaused.
	 * 
	 * @param pause if <code>true</code> no new alarms are added in the table
	 */
	public void pause(boolean pause) {
		if (pause) {
			while (!terminateThread) {
				try {
					paused.acquire();
					break;
				} catch (InterruptedException e) {
					continue;
				}
			}
			
		} else {
			paused.release();
		}
	}

	/**
	 * The thread getting alarms from the queue and injecting in the model.
	 * <P>
	 * If an alarm with the same triplet is already in the table it is replaced.
	 * 
	 * @see java.lang.Runnable#run()
	 */
	@Override
	public void run() {
		// Get an alarm out of the queue
		while (!terminateThread) {
			AlarmTableEntry alarm;
			
			// Get an alarm from the queue waiting until an alarm
			// is available
			try { 
				alarm=queue.take();
			} catch (InterruptedException e) {
				continue;
			}
			
			// Wait if the application is paused by getting the semaphore
			try {
				paused.acquire();
			} catch (InterruptedException e) {
				continue;
			}
			// The semaphore is immediately released to avoid blocking
			// pause() for a long time
			paused.release();
		
			// Debug messages for investigating reductions
//			System.out.println("Adding "+alarm.getIdentifier());
//			System.out.println("\tMultiplicity parent: "+alarm.isMultiplicityParent());
//			System.out.println("\tMultiplicity child: "+alarm.isMultiplicityChild());
//			System.out.println("\tNode parent: "+alarm.isNodeParent());
//			System.out.println("\tNodechild: "+alarm.isNodeChild());
//			System.out.println("Status:");
//			System.out.println("\t\tMasked: "+alarm.getStatus().isMasked());
//			System.out.println("\t\tReduced: "+alarm.getStatus().isReduced());
//			System.out.println("\t\tActive: "+alarm.getStatus().isActive());
			
			synchronized (items) {
				if (items.size(applyReductionRules)==MAX_ALARMS && !items.contains(alarm.getAlarmId())) {
					AlarmTableEntry removedAlarm=null;
					try {
						removedAlarm = items.removeOldest(); // Remove the last one
					} catch (Exception e) {
						System.err.println("Error removing the oldest alarm: "+e.getMessage());
						e.printStackTrace(System.err);
						JOptionPane.showInternalMessageDialog(
								owner, 
								e.getMessage(), 
								"Error removing the oldest alarm", 
								JOptionPane.ERROR_MESSAGE);
						return;
					}
					counters.get(removedAlarm.getAlarmType()).decCounter();
				}
				if (items.contains(alarm.getAlarmId())) {
					replaceAlarm(alarm);
				} else {
					addAlarm(alarm);
				}
			}
			fireTableDataChanged();
		}
	}
	
	/**
	 * Terminate the thread and free the resources.
	 */
	public void close() {
		terminateThread=true;
		thread.interrupt();
	}
}
