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

import java.util.HashSet;
import java.util.Set;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogField;

/**
 * The reduction rule for the antenna.
 * All the logs with the same message but for different antennae are 
 * replaced by a single log in the table that reports all the antennae that 
 * generate the messages.
 * <BR>
 * For example the 2 following messages
 * <UL>
 * 	<LI>DV01 in position
 * 	<LI>DA41 in position
 * </UL>
 * Are replaced by
 * <UL>
 * 	<LI>DV01 in position <I>and also DA41</I>
 * </UL>
 * <P>
 * Note that the reduction applies only to the first occurrence of an
 * antenna name in the log message. If the log message contains more then
 * one antenna name, then the reduction does not work.
 * 
 * @author acaproni
 *
 */
public class AntennaRule extends ReductionRule {

	/**
	 * The antennae whose pattern has to be searched in the log messages.
	 * 
	 * @author acaproni
	 *
	 */
	private enum Antennae {
		NO_ANTENNA(null),
		DV("DV\\d\\d"),
		DA("DA\\d\\d"),
		PM("PM\\d\\d"),
		CM("CM\\d\\d");
		
		/**
		 * The pattern of an antenna to match in a message
		 */
		private final Pattern pattern;
	
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
	
	/**
	 * This is the initial message with the antenna name replaced by 
	 * a place holder.
	 * baseMessage will be used to compare with the other log message to check
	 *     		   if they can be reduced or not
	 */
	private final String baseMessage;
	
	/**
	 * The name of the antenna in the first message
	 */
	private final String initialAntenna;
	
	/**
	 * The names of the antennae reduced by this rule
	 */
	private final Set<String> antennaNames=new HashSet<String>();
	
	/**
	 * The place holder for comparison
	 */
	public static final String placeHolder="\0";
	
	/**
	 * Constructor
	 * 
	 * @param initialMessage The message to be compared with other logs for reduction
	 * 
	 */
	public AntennaRule(String initialMessage) {
		super(initialMessage);
		if (reducible) {
			StringBuilder temp= new StringBuilder(initialMessage);
			initialAntenna=Antennae.matchAndReplaceAntenna(temp, placeHolder);
			baseMessage=temp.toString();
		} else {
			baseMessage=null;
			initialAntenna=null;
		}
	}
	
	/**
	 * @Override
	 */
	protected boolean checkReducibility() {
		return Antennae.matchAntenna(initialMessage)!=Antennae.NO_ANTENNA;
	}
	
	/**
	 * @Override
	 */
	public boolean applyRule(final ILogEntry logToReduce) {
		if (!reducible) {
			return false;
		}
		StringBuilder msg = new StringBuilder(logToReduce.getField(LogField.LOGMESSAGE).toString());
		
		String antName=Antennae.matchAndReplaceAntenna(msg, "\0");

		if (antName==null) { // No antenna in the string
			return false;
		}
		if (baseMessage.compareTo(msg.toString())==0) {
			if (!antName.equals(initialAntenna)) {
				antennaNames.add(antName);
			}
			return true;
		} 
		
		return false;
	}
	
	/**
	 * Format and return the names of all the antenna names matched
	 * by this reduction rule
	 * 
	 * @return A string with all the antenna mames
	 */
	public String getReducedItems() {
		StringBuilder ret = new StringBuilder();
		boolean first = true;
		for (String antName: antennaNames) {
			if (!first) {
				ret.append(',');
				ret.append(' ');
			} else {
				first=false;
			}
			ret.append(antName);
		}
		return ret.toString();
	}
	
}
