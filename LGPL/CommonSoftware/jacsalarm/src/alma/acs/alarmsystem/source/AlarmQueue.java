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
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

/**
 * A queue of alarms.
 * <P>The alarms are stored in a map because we want to storein the vector are ordered following their arrival time
 * i.e. the oldest item is in the head.
 * <P>
 * The class is thread safe.
 * <P>
 * For the implementation a Set would have been enough but the ConcurrentHashMap
 * is thread safe and there should not be big perfromance difference between
 * this and a concrete Set.
 * 
 * @author acaproni
 *
 */
public class AlarmQueue extends ConcurrentHashMap<String,AlarmQueue.AlarmToQueue> {
	/**
	 * An alarm to be queued.
	 * <P>
	 * The object is immutable.
	 * 
	 * @author acaproni
	 *
	 */
	public class AlarmToQueue {
		public final String faultFamily;
		public final String faultMember;
		public final int faultCode;
		private final Properties properties;
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
			this.active = active;
			if (properties==null) {
				this.properties=null;
			} else {
				this.properties=new Properties();
				copyProperties(properties, this.properties);
			}
		}
		
		/**
		 * Return a copy of the properties to avoid publishing {@link #properties}, keeping
		 * the object immutable.
		 *  
		 * @return A copy of the properties; It can be <code>null</code>
		 */
		public Properties getProperties() {
			if (properties==null) {
				return null;
			}
			Properties ret = new Properties();
			copyProperties(properties, ret);
			return ret;
		}
		
		private void copyProperties(Properties src, Properties dest) {
			if (src==null) {
				throw new NullPointerException("Invalid null source properties");
			}
			if (dest==null) {
				throw new NullPointerException("Invalid null destintaion properties");
			}
			Set<String> names=src.stringPropertyNames(); 
			for (String propertyName: names) {
				String value=src.getProperty(propertyName);
				dest.setProperty(propertyName, value);
			}
		}
	}
	
	/**
	 * Stores the alarm, overwriting any previous settings for the same triplet.
	 * 
	 * @param faultFamily The fault family
	 * @param faultMember The fault member
	 * @param faultCode The fault code
	 * @param properties The user properties
	 * @param active The state active/terminate
	 */
	public void add(String faultFamily, 
			String faultMember,
			int faultCode, 
			Properties properties, 
			boolean active) {
		put(
				faultFamily+":"+faultMember+":"+faultCode,
				new AlarmToQueue(faultFamily, faultMember, faultCode, properties, active));
	}
}
