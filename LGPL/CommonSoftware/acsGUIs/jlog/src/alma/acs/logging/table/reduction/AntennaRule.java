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
import java.util.Vector;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogEntry;
import com.cosylab.logging.engine.log.LogField;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.ILogEntry.AdditionalData;

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
	 * The message of the log used as a base for reducing
	 */
	private final String initialMessage;
	
	/**
	 * <code>reducible</code> is <code>true</code> if the initial message
	 * contains an antenna name and therefore can reduce other log
	 * messages. 
	 */
	protected final boolean reducible;
	
	/**
	 * Constructor
	 * 
	 * @param initialMessage The message to be compared with other logs for reduction
	 * 
	 */
	public AntennaRule(ILogEntry logEntry) {
		super(logEntry);
		initialMessage=(String)logEntry.getField(LogField.LOGMESSAGE);
		reducible=Antennae.matchAntenna(initialMessage)!=Antennae.NO_ANTENNA;
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
	public boolean applyRule(final ILogEntry logToReduce) {
		if (
				!reducible || logToReduce==null) {
			return false;
		}
		String msgStr=logToReduce.getField(LogField.LOGMESSAGE)!=null?(String)logToReduce.getField(LogField.LOGMESSAGE):"";
		StringBuilder msg = new StringBuilder(msgStr);
		
		String antName=Antennae.matchAndReplaceAntenna(msg, placeHolder);

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
	 * @return A string with all the antenna names
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

	@Override
	public ILogEntry getReducedLog() {
		String reducedItems = getReducedItems();
		if (reducedItems==null || reducedItems.isEmpty()) {
			return initialLog;
		}
		Long milliseconds=(Long)initialLog.getField(LogField.TIMESTAMP);
		Integer entrytype=((LogTypeHelper)initialLog.getField(LogField.ENTRYTYPE)).ordinal();
		String file=(String)initialLog.getField(LogField.FILE);
		Integer line=(Integer)initialLog.getField(LogField.LINE);
		String routine=(String)initialLog.getField(LogField.ROUTINE);
		String host=(String)initialLog.getField(LogField.HOST);
		String process=(String)initialLog.getField(LogField.PROCESS);
		String context=(String)initialLog.getField(LogField.CONTEXT);
		String thread=(String)initialLog.getField(LogField.THREAD);
		String logid=(String)initialLog.getField(LogField.LOGID);
		Integer priority=(Integer)initialLog.getField(LogField.PRIORITY);
		String uri=(String)initialLog.getField(LogField.URI);
		String stackid=(String)initialLog.getField(LogField.STACKID);
		Integer stacklevel=(Integer)initialLog.getField(LogField.STACKLEVEL);
		String logmessage=(String)initialLog.getField(LogField.LOGMESSAGE)+" and also "+reducedItems;
        String srcObject=(String)initialLog.getField(LogField.SOURCEOBJECT);
        String audience=(String)initialLog.getField(LogField.AUDIENCE);
        String array=(String)initialLog.getField(LogField.ARRAY);
        String antenna=(String)initialLog.getField(LogField.ANTENNA);
        Vector<AdditionalData> addDatas=initialLog.getAdditionalData();
		
		LogEntry ret = new LogEntry(
				milliseconds, 
				entrytype, 
				file, 
				line, 
				routine, 
				host, 
				process, 
				context, 
				thread, 
				logid, 
				priority, 
				uri, 
				stackid, 
				stacklevel, 
				logmessage, 
				srcObject, 
				audience, 
				array, 
				antenna, 
				addDatas);
		
		return ret;
	}

	@Override
	public boolean isReducingLogs() {
		return !antennaNames.isEmpty();
	}
	
	/**
	 * @return the reducible
	 * 
	 */
	@Override
	public boolean isReducible() {
		return reducible;
	}
}
