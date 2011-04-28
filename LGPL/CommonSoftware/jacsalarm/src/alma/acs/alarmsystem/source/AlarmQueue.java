/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free Software
 *    Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307  USA
 */
package alma.acs.alarmsystem.source;

import java.util.Properties;
import java.util.Vector;

/**
 * A queue of alarms.
 * <P>The alarms in the vector are ordered following their arrival time
 * i.e. the oldest item is in the head.
 * 
 * @author acaproni
 *
 */
public class AlarmQueue extends Vector<Vector> {
	/**
	 * An alarm to be queued
	 * 
	 * @author acaproni
	 *
	 */
	public class AlarmToQueue {
		public final String faultFamily;
		public final String faultMember;
		public final int faultCode;
		public final Properties properties;
		public final boolean active;

		/**
		 * Constructor
		 * 
		 * @param faultFamily
		 * @param faultMember
		 * @param faultCode
		 * @param properties
		 * @param active
		 */
		public AlarmToQueue(
				String faultFamily, 
				String faultMember,
				int faultCode, 
				Properties properties, 
				boolean active) {
			super();
			this.faultFamily = faultFamily;
			this.faultMember = faultMember;
			this.faultCode = faultCode;
			this.properties = properties;
			this.active = active;
		}
	}

	/**
	 * The queue of alarms
	 */
	private final Vector<AlarmToQueue> queue = new Vector<AlarmToQueue>();
	
	/**
	 * 
	 * @param faultFamily The fault family
	 * @param faultMember The fault member
	 * @param faultCode The fault code
	 * @param properties The user properties
	 * @param active The state active/terminate
	 */
	public synchronized void add(String faultFamily, 
			String faultMember,
			int faultCode, 
			Properties properties, 
			boolean active) {
		queue.add(new AlarmToQueue(faultFamily, faultMember, faultCode, properties, active));
	}
}
