/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2011
 *    Copyright by ESO (in the framework of the ALMA collaboration)
 *    and Cosylab 2009, All rights reserved
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
package alma.acs.logging.table.reduction;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

/**
 * The antennae whose pattern has to be searched in the logs.
 * 
 * @author acaproni
 *
 */
public enum Antennae {
	NO_ANTENNA(null),
	DV("DV\\d\\d"),
	DA("DA\\d\\d"),
	PM("PM\\d\\d"),
	CM("CM\\d\\d");
	
	/**
	 * The pattern of an antenna to match in a message
	 */
	public final Pattern pattern;

	/**
	 * Constructor
	 */
	private Antennae(String pattern) {
		if (pattern!=null) {
			this.pattern=Pattern.compile(pattern);
		} else {
			this.pattern=null;
		}
	}
	/**
	 * Check if the message contains a reference to an antenna
	 * 
	 * @param messageToMatch The message to check against an antenna name
	 * @return The antenna that matches with the message
	 */
	public static Antennae matchAntenna(String messageToMatch) {
		if (messageToMatch==null || messageToMatch.isEmpty()) {
			return NO_ANTENNA;
		}
		for (Antennae ant: Antennae.values()) {
			if (ant==NO_ANTENNA) {
				continue;
			}
			Matcher m = ant.pattern.matcher(messageToMatch);
			if (m.find()) {
				return ant;
			}
		}
		return NO_ANTENNA;
	}

	
	/**
	 * Check if the message contains a reference to an antenna and replace
	 * the antenna name with a place holder. The method returns the antenna
	 * name that matched.
	 * 
	 * @param messageToMatch The message to check against an antenna name
	 * @param placeHolder The place holder for the matched pattern
	 * @return The antenna name that matches with the message
	 * 		   or <code>null</code> if the string does not match with any antenna name
	 */
	public static String matchAndReplaceAntenna(StringBuilder messageToMatch, String placeHolder) {
		for (Antennae ant: Antennae.values()) {
			if (ant==NO_ANTENNA) {
				continue;
			}
			Matcher m = ant.pattern.matcher(messageToMatch);
			if (m.find()) {
				int start = m.start();
				int end = m.end();
				String ret=messageToMatch.substring(start, end);
				messageToMatch.replace(start, end, placeHolder);
				return ret;
			}
		}
		return null;
	}

}
