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
package alma.alarmsystem.clients.category;

import java.util.Date;
import java.text.SimpleDateFormat;

/**
 * The view of an alarm in the table
 * 
 * @author acaproni
 *
 */
public class AlarmView {
	
	// The format for the timestamp
	private final String TIME_FORMAT = "yyyy'-'MM'-'dd'T'HH':'mm':'ss";
	private final SimpleDateFormat dateFormat = new SimpleDateFormat(TIME_FORMAT);
	
	public String alarmID=null; // Triplet
	public Integer priority = null; // Priority
	public Date sourceTimestamp=null; // Source timestamp
	public String description=null; // Problem description
	public String cause=null; // The cause
	public Boolean active=null; // Active
	public String hostName; // The name of the host of the source that sent the alarm
	
	/**
	 * Constructor
	 * 
	 * @param id The ID (triplet)
	 * @param pri Priority
	 * @param timestamp Timestamp
	 * @param desc Problem description
	 * @param act Active
	 */
	public AlarmView(String id, String pri, String timestamp, String desc, String cause,String act,String hostName) {
		if (id==null || id.length()==0) {
			throw new IllegalArgumentException("Invalid triplet in constructor");
		}
		alarmID=id;
		description=desc;
		this.cause=cause;
		this.hostName=hostName;
		// Get the date
		try {
			long nsec=Long.parseLong(timestamp);
			 sourceTimestamp=new Date(nsec);
		} catch (Exception e) {
			// Invalid timestamp
			sourceTimestamp=null;
			System.err.println("Invalid timestamp for "+alarmID+": "+timestamp);
		}
		// Get the status
		try {
			active=Boolean.parseBoolean(act);
		} catch (Exception e) {
			System.err.println("Invalid priority for "+alarmID+": "+pri);
			active=true;
		}
		// Get the priority
		try {
			priority=new Integer(pri);
		} catch (Exception e) {
			System.err.println("Invalid priority for "+alarmID+": "+pri);
			priority=0;
		}
	}

	/**
	 * Override the Object.equals because we know
	 * that 2 alarms are equals if they have the same ID
	 *
	 * This method is used to check if a given alarm is already
	 * in the vector of the alarms in the table.
	 * The same alarm is shown only once but its color must change
	 * depending of its stata (active/inactive)
	 * 
	 * @param obj The objct to comapre
	 * @return true if the 2 objects are equal
	 */
	@Override
	public boolean equals(Object obj) {
		if (obj==null || !(obj instanceof AlarmView)) {
			return false;
		} else {
			AlarmView alarm = (AlarmView)obj;
			return alarmID.equals(alarm.alarmID);
		}
	}
	
	/**
	 * Dump the alarm
	 * @see Object.toString()
	 */
	@Override
	public String toString() {
		StringBuilder ret = new StringBuilder("Triplet: ");
		ret.append(alarmID);
		ret.append(", Time: ");
		if (sourceTimestamp!=null) {
			ret.append(dateFormat.format(sourceTimestamp));
		} else {
			ret.append("undefined");
		}
		ret.append(", Priority: ");
		ret.append(priority);
		ret.append(", Status: ");
		ret.append(active);
		ret.append(", Description: ");
		ret.append(description);
		return ret.toString();
	}
	
	/**
	 * Return the formatted timestamp
	 * 
	 * @return The timestamp
	 */
	public String getTimestamp() {
		if (sourceTimestamp!=null) {
			return dateFormat.format(sourceTimestamp);
		}
		return "N/A";
	}
}