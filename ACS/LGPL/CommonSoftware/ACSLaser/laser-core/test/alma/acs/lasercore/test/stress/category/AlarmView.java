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
package alma.acs.lasercore.test.stress.category;

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
	
	public final String alarmID; // Triplet
	public final Integer priority; // Priority
	public final Date sourceTimestamp; // Source timestamp
	public final String description; // Problem description
	public final String cause; // The cause
	public final Boolean active;// Active
	public final String hostName; // The name of the host of the source that sent the alarm
	public final Boolean nodeParent;
	public final Boolean nodeChild;
	public final Boolean multiplicityParent;
	public final Boolean multiplicityChild;
	public final Boolean masked;
	public final Boolean reduced;
	
	/**
	 * Constructor
	 * 
	 * @param id The ID (triplet)
	 * @param pri Priority
	 * @param timestamp Timestamp
	 * @param desc Problem description
	 * @param act Active
	 */
	public AlarmView(
			String id, 
			String pri, 
			String timestamp, 
			String desc, 
			String cause,
			String act,
			String hostName, 
			String nodeParent,
			String nodeChild, 
			String multiplicityParent, 
			String multiplicityChild,
			String reduced,
			String masked) 
	throws Exception {
		if (id==null || id.length()==0) {
			throw new IllegalArgumentException("Invalid triplet in constructor");
		}
		alarmID=id;
		description=desc;
		this.cause=cause;
		this.hostName=hostName;
		// Get the date
		long nsec=Long.parseLong(timestamp);
		 sourceTimestamp=new Date(nsec);
		// Get the status
		active=Boolean.parseBoolean(act);
		// Get the priority
		priority=new Integer(pri);
		
		this.nodeParent=Boolean.parseBoolean(nodeParent);
		this.nodeChild=Boolean.parseBoolean(nodeChild);
		this.multiplicityParent=Boolean.parseBoolean(multiplicityParent);
		this.multiplicityChild=Boolean.parseBoolean(multiplicityChild);
		this.masked=Boolean.parseBoolean(masked);
		this.reduced=Boolean.parseBoolean(reduced);
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