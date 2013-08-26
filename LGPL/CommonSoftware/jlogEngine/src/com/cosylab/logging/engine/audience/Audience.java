/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2010
 *    Copyright by ESO
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
package com.cosylab.logging.engine.audience;

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * The interface for the audience
 * 
 * @author acaproni
 * @since ACS 8.1.0
 *
 */
public interface Audience {
	
	/**
	 * The known audiences
	 * 
	 * @author acaproni
	 *
	 */
	public enum AudienceInfo{
		ENGINEER("Engineer","Engineer"),
		OPERATOR("Operator","Operator"),
		SCILOG("Science Log","SciLog");
		
		/**
		 * The name of the audience as it appears in the menu 
		 * items and so on
		 */
		public final String name;
		
		/**
		 * The short name used for example in the command line.
		 * >
		 * @see AudienceInfo#fromShortName(String)
		 */
		public final String shortName;
		
		/**
		 * The audience of each type is a singleton
		 */
		private Audience audience;
		
		private AudienceInfo(String name,String shortName) {
			this.name=name;
			this.shortName=shortName;
		}
		
		/**
		 * @return The audience of the given type
		 */
		public Audience getAudience() {
			if (audience!=null) {
				return audience;
			}
			switch (this) {
			case ENGINEER: {
				audience= new EngineerAudience();
				return audience;
			}
			case OPERATOR: {
				audience= new OperatorAudience();
				return audience;
			}
			case SCILOG: {
				audience = new SciLogAudience();
				return audience;
			}
			default: {
				throw new IllegalStateException("Unknown audience "+name);
			}
			}
		}
		
		@Override
		public String toString() {
			return name;
		}
		
		/**
		 * 
		 * @return the defined audiences types
		 */
		public static AudienceInfo[] getTypes() {
			return AudienceInfo.values();
		}
		
		/**
		 * @return The names of the audiences
		 */
		public static String[] getNames() {
			String[] ret = new String[AudienceInfo.values().length];
			for (int t=0; t<ret.length; t++) {
				ret[t]=AudienceInfo.values()[t].name;
			}
			return ret;
		}
		
		/**
		 * Return the names of the audiences that can be used
		 * as command line params or as java proprties.
		 * 
		 * @return The short names of the audiences
		 */
		public static String[] getShortNames() {
			String[] ret = new String[AudienceInfo.values().length];
			for (int t=0; t<ret.length; t++) {
				ret[t]=AudienceInfo.values()[t].shortName;
			}
			return ret;
		}
		
		/**
		 * Return the audience corresponding to the passed short name.
		 * <P>
		 * This method can be useful when the user write the name of the
		 * audience for example as a parameter in the command line or in
		 * a java property.
		 * <P>
		 * The comparison between the passed string and the short name
		 * of the audience is case insensitive.
		 * 
		 * @param name The short name of the audience
		 * @return The audience having this short name or
		 * 			<code>null</code> if an audience with that short
		 * 			name does not exist
		 * @see AudienceInfo#shortName
		 */
		public static AudienceInfo fromShortName(String name) {
			if (name==null || name.isEmpty()) {
				throw new IllegalArgumentException("Invalid audience short name");
			}
			for (AudienceInfo aInfo: AudienceInfo.values()) {
				if (name.equalsIgnoreCase(aInfo.shortName)) {
					return aInfo;
				}
			}
			return null;
		}
	}
	
	/**
	 * 
	 * @return The AudienceInfo related to this audience
	 */
	public AudienceInfo getInfo();
	
	/**
	 * Check if the passed log matches with the criteria of 
	 * the audience.
	 * 
	 * @param log The log to check
	 * @return <code>true</code> if the log can be accepted
	 */
	public boolean matches(ILogEntry log);
}
