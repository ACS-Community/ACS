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
 * The log passed in the constructor is compared with other logs 
 * with {@link ReductionRule#applyRule(ILogEntry)}.
 * The log in the constructor is the base for the
 * reduction that depends on the criteria implemented in a 
 * specific reduction rule.
 * <P>
 * If the rule is able to reduce logs (for example the message
 * contains an antenna name) then other logs are passed to it
 * through {@link ReductionRule#applyRule(ILogEntry)}: the rule keeps
 * track of the logs it reduces in order to generate a new log replacing 
 * the log passed in the constructor and all the other logs.
 * Such a log is returned by {@link ReductionRule#getReducedLog()}.
 * 
 * @author acaproni
 */
public abstract class ReductionRule {
	
	/**
	 * The log to be compared with other logs for reduction.
	 */
	protected final ILogEntry initialLog;
	
	/**
	 * The place holder for comparison
	 */
	protected static final String placeHolder="\0\0\0\0";
	
	/**
	 * Constructor
	 * 
	 * @param initialMessage The message to be compared with other logs for reduction
	 * 
	 */
	public ReductionRule(ILogEntry initialLog) {
		if (initialLog==null) {
			throw new IllegalArgumentException("The log can't be null");
		}
		this.initialLog=initialLog;
	}
	
	/**
	 * @return the reducible
	 */
	public abstract boolean isReducible();
	
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
	public abstract ILogEntry getReducedLog();
	
	/**
	 * 
	 * @return <code>true</code> if this rule is actually reducing logs
	 */
	public abstract boolean isReducingLogs();
}
