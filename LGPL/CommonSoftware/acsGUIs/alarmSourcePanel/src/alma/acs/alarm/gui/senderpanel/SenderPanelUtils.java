/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2012
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
package alma.acs.alarm.gui.senderpanel;

import java.security.InvalidParameterException;
import java.util.Properties;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import cern.laser.source.alarmsysteminterface.FaultState;

/**
 * A collection of general purpose utilities.
 * 
 * @author acaproni
 *
 */
public class SenderPanelUtils {
	
	/**
	 * A triplet composed of  FF,FM, FC
	 * 
	 * @author acaproni
	 *
	 */
	public static class Triplet implements Comparable<Triplet> {
		
		/**
		 * FF
		 */
		public final String faultFamily;
		
		/**
		 * FM
		 */
		public final String faultMember;
		
		/**
		 * FC
		 */
		public final int faultCode;
		
		public Triplet(String ff, String fm, int code) {
			if (ff==null || ff.trim().isEmpty()) {
				throw new IllegalArgumentException("Infalid FF ["+ff+"]");
			}
			if (fm==null || fm.trim().isEmpty()) {
				throw new IllegalArgumentException("Infalid FM ["+fm+"]");
			}
			this.faultFamily=ff.trim();
			this.faultMember=fm.trim();
			this.faultCode=code;
		}
		
		public String toString() {
			return "<"+faultFamily+","+faultMember+","+faultCode+">";
		}

		@Override
		public int compareTo(Triplet o) {
			if (o==null) {
				return this.toString().compareTo(null);
			}
			return this.toString().compareTo(o.toString());
		}

		@Override
		public boolean equals(Object obj) {
			if (obj==null) {
				return this.toString().equals(null);
			}
			return this.toString().equals(obj.toString());
		}
	}
	
	/**
	 * The type (descriptor) set while sending the alarm to the 
	 * alarm service.
	 * 
	 * @author acaproni
	 *
	 */
	public enum AlarmDescriptorType {
		ACTIVE(FaultState.ACTIVE),
		TERMINATE(FaultState.TERMINATE),
		INSTANT(FaultState.INSTANT),
		CHANGE(FaultState.CHANGE);
		
		/**
		 * The descriptor of the type
		 * 
		 * @see FaultState
		 */
		public final String descriptor;
		
		private AlarmDescriptorType(String type) {
			if (type==null || type.isEmpty()) {
				throw new IllegalArgumentException("Invalid alarm type");
			}
			descriptor=type;
		}
		
		/**
		 * Return a {@link AlarmDescriptorType} from it descriptor string
		 * 
		 * @param descriptor The descriptor
		 * @return The <CODE>AlarmDescriptorType</CODE> matching the descriptor
		 * @throws Exception if the descriptor does not match any {@link FaultState} descriptor.
		 * 
		 */
		public static AlarmDescriptorType fromDescriptor(String descriptor) throws Exception {
			if (ACTIVE.descriptor.equals(descriptor)) {
				return ACTIVE;
			} else if (TERMINATE.descriptor.equals(descriptor)) {
				return TERMINATE;
			}  else if (INSTANT.descriptor.equals(descriptor)) {
				return INSTANT;
			}  else if (CHANGE.descriptor.equals(descriptor)) {
				return CHANGE;
			} else {
				throw new Exception("The descriptor does not match!");
			}
		}
		
		@Override
		public String toString() {
			return descriptor;
		}
	}
	
	/**
	 * The regular expression representing a triplet of the form
	 * <code>FF,FM,FC</code> including whitespaces.
	 */
	public final static Pattern tripletRegExp = Pattern.compile("\\s*\\S+\\s*,\\s*(\\S+|\\*)\\s*,\\s*\\d+");
	
	/**
	 * A property has the format 
	 * <code>key=val</code>
	 */
	public final static Pattern propertyRegExp = Pattern.compile("\\S+\\s*=\\s*\\S+");
			
	/**
	 * The list of properties has the format 
	 * <code>key1=val1, key2=val2...</code>
	 */
	public final static Pattern propertiesRegExp =Pattern.compile(propertyRegExp.pattern()+"(\\s*,\\s*"+propertyRegExp.pattern()+")*");
	
	/**
	 * A alarm is a tripled optionally followed by a string of properties.
	 * 
	 */
	public final static Pattern alarmRegExp = Pattern.compile(tripletRegExp.pattern()+"|"+tripletRegExp.pattern()+"\\s+"+propertiesRegExp.pattern());

	/**
	 * Build the triplet from the passed string
	 * 
	 * @param triplet The not {@link NullPointerException} not empty triplet in the form FF,FM,FC
	 * @throws Exception in case of syntax error in the passed string
	 */
	public static Triplet tripletFromString(String triplet) {
		if (triplet==null || triplet.trim().isEmpty()) {
			throw new IllegalArgumentException("Infalid empty/null triplet ["+triplet+"]");
		}
		if (!SenderPanelUtils.isATriplet(triplet)) {
			throw new IllegalArgumentException("Invalid formatted triplet ["+triplet+"]");
		}
		triplet=triplet.trim();
		String[] vals=triplet.split(",");
		if (vals.length!=3) {
			throw new InvalidParameterException("Invalid triplet "+triplet);
		}
		int code;
		try {
			code=Integer.parseInt(vals[2].trim());
		} catch (NumberFormatException nfe) {
			throw new InvalidParameterException("Invalid fault code "+vals[2]);
		}
		return new SenderPanelUtils.Triplet(vals[0].trim(),vals[1].trim(),code);
	}
	
	/**
	 * Build the user properties from the passed string of properties 
	 * 
	 * @return the user properties or <code>null</code> if the passed string is empty/null
	 * @throws Exception In case of syntax error in the passed string
	 */
	public static Properties propertiesFromString(String props) throws Exception {
		if (props==null || props.isEmpty()) {
			return null;
		}
		props=props.trim();
		Properties ret = new Properties();
		String[] properties=props.split(",");
		for (String str: properties) {
			if (!str.contains("=")) {
				throw new Exception("Invalid user properties\nUse: key=val, key2=val2,...");
			}
			String[] pair = str.trim().split("=");
			if (pair.length!=2) {
				throw new Exception("Invalid user properties\nUse: key=val, key2=val2,...");
			}
			ret.put(pair[0], pair[1]);
		}
		if (ret.isEmpty()) {
			return null;
		}
		return ret;
	}
	
	/**
	 * Check if the passed string is a triplet
	 * 
	 * @param str The string to check
	 * @return <code>true</code> if the string contains a triplet
	 */
	public static boolean isATriplet(String str) {
		if (str==null ||str.isEmpty()) {
			return false;
		}
		Matcher m = SenderPanelUtils.tripletRegExp.matcher(str.trim());
		return m.matches();
	}
	
	/**
	 * Check if the passed string is a alarm
	 * 
	 * @param str The string to check
	 * @return <code>true</code> if the string contains a alarm
	 */
	public static boolean isAnAlarm(String str) {
		if (str==null || str.isEmpty()) {
			return false;
		}
		Matcher m = SenderPanelUtils.alarmRegExp.matcher(str);
		return m.matches();
	}
	
	/**
	 * Check if the passed string is a set of properties
	 * 
	 * @param str The string to check
	 * @return <code>true</code> if the string contains properties
	 */
	public static boolean isAStringOfProperties(String str) {
		if (str==null || str.isEmpty()) {
			return false;
		}
		Matcher m = SenderPanelUtils.propertiesRegExp.matcher(str);
		return m.matches();
	}
}
