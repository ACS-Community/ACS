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

import com.cosylab.logging.engine.log.ILogEntry;

/**
 * The objects that implement this interface are those that
 * reduces the logs.
 * <P>
 * The idea is that the message of a log entry is compared
 * with a base message and <code>true</code> is returned if the log entry can be reduced.  
 * <P>
 * For example suppose that we want to reduce the log entries that
 * differ only for the name of an antenna.
 * <code>baseMsg</code> should be something like "Antenna xxxx in position" 
 * where xxxx is a place holder.
 * Then if <code>logToReduce</code> message is "Antenna DA41 in position",
 * the value returned by <code>applyRule</code> <code>true</code>.
 * <P>
 * The reduction rule is applied to a set of logs. It says if ech log 
 * can be reduced and finally a string of reduced strings is produced.
 * <BR>
 * At the end of the process only one log has to be preserved by adding
 * the reduced string. The reduced logs can be removed from the table.
 * 
 * @author acaproni
 */
public abstract class ReductionRule {
	
	/**
	 * The message to be compared with other logs for reduction
	 */
	protected final String initialMessage;
	
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
	public ReductionRule(String initialMessage) {
		if (initialMessage==null || initialMessage.isEmpty()) {
			throw new IllegalArgumentException("The message can't be null nor empty");
		}
		this.initialMessage=initialMessage;
		reducible=checkReducibility();
	}
	
	/**
	 * 
	 * @return <code>true</code> if the initial message is suitable 
	 *         for reducing other log messages
	 */
	protected abstract boolean checkReducibility();
	
	/**
	 * @return the reducible
	 */
	public boolean isReducible() {
		return reducible;
	}
	
	/**
	 * applyRule compares the log and the message and check if the log can be reduced.
	 * If it is the case, the method returns a String representing the relevant part of 
	 * the message to reduce
	 * 
	 * @param logToReduce The log to reduce
	 * @return <code>true</code> if the logs is reduced by this rule
	 *         <code>false</code> otherwise
	 */
	public abstract boolean applyRule(final ILogEntry logToReduce);
	
	/**
	 * Return a comma separated list of the reduced items after running the
	 * reduction rule over a set of logs.
	 * 
	 * @return A formatted String with all the reduced items
	 */
	public abstract String getReducedItems();
}
