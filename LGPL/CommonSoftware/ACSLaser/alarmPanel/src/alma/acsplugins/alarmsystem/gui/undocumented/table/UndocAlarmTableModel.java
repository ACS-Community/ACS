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
package alma.acsplugins.alarmsystem.gui.undocumented.table;

import java.sql.Timestamp;
import java.text.SimpleDateFormat;
import java.util.Collections;
import java.util.List;
import java.util.Vector;

import javax.swing.table.AbstractTableModel;

import cern.laser.client.data.Alarm;

import alma.acs.util.IsoDateFormat;
import alma.acsplugins.alarmsystem.gui.CernAlSysTabbedPane;

public class UndocAlarmTableModel extends AbstractTableModel {
	
	/**
	 * The titles of the columns
	 * 
	 * @author acaproni
	 *
	 */
	public enum ColumnTitles {
		TIME("Time",String.class),
		COMPONENT("Component",String.class),
		FAMILY("Family",String.class),
		CODE("Code",Integer.class);
		
		/**
		 *  The title of the column as it appears in the table header
		 */
		public final String title;
		
		/**
		 * The class of each column
		 */
		public final Class theClass;
		
		/**
		 * Constructor
		 * 
		 * @param title The title of the column
		 */
		private ColumnTitles(String title, Class theClass) {
			this.title=title;
			this.theClass=theClass;
		}
	}
	
	/**
	 * No point to store a whole {@link Alarm} data as in
	 * this case we need only a few items.
	 * 
	 * @author acaproni
	 *
	 */
	public class AlarmData implements Comparable<AlarmData >{
		
		public  final String component;
		
		public final String family;
		
		public final int code;
		
		public final Timestamp timestamp;
		
		public final boolean active;
		
		/**
		 * Constructor
		 * 
		 * @param FF FaultFamily
		 * @param FM FaultMember
		 * @param FC FaultCode
		 * @param timestamp Timestamp
		 * @param active <code>true</code> if the alarm is active
		 */
		public AlarmData(String FF, String FM, int FC, Timestamp timestamp,boolean active) {
			if (FF==null || FF.isEmpty()) {
				throw new IllegalArgumentException("Invalid FF");
			}
			if (FM==null || FM.isEmpty()) {
				throw new IllegalArgumentException("Invalid FM");
			}
			if (timestamp==null ) {
				throw new IllegalArgumentException("Invalid timestamp");
			}
			this.family=FF;
			this.component=FM;
			this.code=FC;
			this.timestamp=timestamp;
			this.active = active;
		}

		@Override
		public int compareTo(AlarmData o) {
			String alarmID=family+":"+component+":"+code;
			String otherAlarmID=o.family+":"+o.component+":"+o.code;
			return alarmID.compareTo(otherAlarmID);
		}

		@Override
		public boolean equals(Object obj) {
			if (obj==null) {
				return false;
			}
			if (!(obj instanceof AlarmData)) {
				return false;
			}
			String alarmID=family+":"+component+":"+code;
			AlarmData o =(AlarmData)obj;
			String otherAlarmID=o.family+":"+o.component+":"+o.code;
			return alarmID.equals(otherAlarmID);
		}
		
		
	}
	
	private static final int QUEUE_SIZE= 30000;
	
	/** 
	 * The date format
	 */
	private final SimpleDateFormat dateFormat = new IsoDateFormat();
	
	/** 
	 * The queue of alarms received from the <code>CategoryClient</code> that will be
	 * injected in the table
	 */
	private final List<AlarmData> items = Collections.synchronizedList(new Vector<AlarmData>());
	
	/**
	 * The tabbed pane to hide show this tab depending if there are undocumented
	 * alarms or not.
	 */
	private final CernAlSysTabbedPane tabbedPane;
	
	/**
	 * The number of active alarms in the table
	 */
	private volatile int numOfActiveAlarms=0;
	
	/**
	 * Canstructor
	 * 
	 * @param tabbedPane The tabbed pane to hide/show the undocumented alarm tab
	 */
	public UndocAlarmTableModel(CernAlSysTabbedPane tabbedPane) {
		if (tabbedPane==null) {
			throw new IllegalArgumentException("The tabbed pane can't be null");
		}
		this.tabbedPane=tabbedPane;
	}

	@Override
	public int getRowCount() {
		return items.size();
	}

	@Override
	public int getColumnCount() {
		return ColumnTitles.values().length;
	}

	@Override
	public Object getValueAt(int rowIndex, int columnIndex) {
		AlarmData data = getRowEntry(rowIndex);
		ColumnTitles col = ColumnTitles.values()[columnIndex];
		switch (col) {
		case TIME: return dateFormat.format(data.timestamp);
		case COMPONENT: return data.component;
		case FAMILY: return data.family;
		case CODE: return data.code;
		}
		return null;
	}
	
	@Override
	public String getColumnName(int col) {
		return ColumnTitles.values()[col].title;
	}
	
	/**
	 * Add a undocumented alarm in the model.
	 * The thread will get the alarm from the queue and update the model.
	 * 
	 * @param alarm The alarm to add to the table.
	 * @see AlarmSelectionListener
	 */
	public synchronized void onAlarm(Alarm alarm) {
		String FF=alarm.getTriplet().getFaultFamily();
		String FM=alarm.getTriplet().getFaultMember();
		int code =alarm.getTriplet().getFaultCode();
		Timestamp timestamp = alarm.getStatus().getSourceTimestamp();
		boolean active = alarm.getStatus().isActive();
		
		AlarmData data = new AlarmData(FF, FM, code, timestamp,active);
		
		// Calc. the number of active alarms in table
		int idx=items.indexOf(data);
		AlarmData old=null;
		if (idx!=-1) {
			old=items.get(idx);
		}
		if (old==null) {
			if (data.active) {
				numOfActiveAlarms++;
			}
		} else {
			if (old.active && !data.active) {
				numOfActiveAlarms--;
			} else if (!old.active && data.active) {
				numOfActiveAlarms++;
			}
		}
		// Remove old instance if any
		if (old!=null) {
			items.remove(data);
		}
		
		// too many itemes in table?
		while (items.size()>=QUEUE_SIZE) {
			items.remove(items.size()-1);
		}
		items.add(data);
		// Show the tab as there is at least one alarm in the table
		tabbedPane.undocTabVisible(true);
		fireTableDataChanged();
	}
	
	/**
	 * Return the item at the given row
	 * 
	 * @param row The row of the item to get
	 * @return the item at row position
	 */
	public AlarmData getRowEntry(int row) {
		if (row<0 || row>=items.size()) {
			throw new IllegalArgumentException("Invalid row index");
		}
		return items.get(row);
	}
	
	/**
	 * Clear all the alarms from the table
	 */
	public synchronized void clearAll() {
		items.clear();
		numOfActiveAlarms=0;
		fireTableDataChanged();
	}
	
	/**
	 * Remove all the inactive alarms from the table
	 */
	public synchronized void clearInactiveAlarms() {
		int t=0;
		while(t<items.size()) {
			AlarmData al = items.get(t);
			if (!al.active) {
				items.remove(al);
			} else {
				t++;
			}
		}
		fireTableDataChanged();
	}

	/**
	 * Getter
	 * 
	 * @return the number of active alarms in the table
	 */
	public int getNumOfActiveAlarms() {
		return numOfActiveAlarms;
	}

	/**
	 * Override to hide/show the tab depending on the number
	 * of alarms in table
	 */
	@Override
	public void fireTableDataChanged() {
		super.fireTableDataChanged();
		tabbedPane.undocTabVisible(getRowCount()>0);
	}
}
