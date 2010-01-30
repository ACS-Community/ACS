/*
 * ALMA - Atacama Large Millimiter Array (c) European Southern Observatory, 2006
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
package com.cosylab.logging.engine;

import com.cosylab.logging.engine.audience.Audience;
import com.cosylab.logging.engine.log.ILogEntry;
import com.cosylab.logging.engine.log.LogTypeHelper;
import com.cosylab.logging.engine.log.LogField;

/**
 * Objects from this class checks if a log matches with the constraints.
 * <P>
 * The purpose of this object is to have a centralized way to check each log 
 * against the given set of constraints before deciding it can be injected
 * in the system.
 * <P>
 * In particular it checks a log against the followings:
 * <UL>
 * 	<LI>Log (discard) level
 *  <LI>filters
 *  <LI>Audience
 * </UL>
 * 
 * @author acaproni
 *
 */
public class LogMatcher {
	
	/**
	 * The discard level
	 */
	protected volatile LogTypeHelper actualDiscardLevel=null;
	
	/**
	 * The audience.
	 * 
	 * Only the logs for the defined audience will be forwarded to the listeners.
	 * 
	 * @see <code>LCEngine.setFilters()</code>
	 */
	private Audience audience=Audience.AudienceInfo.ENGINEER.getAudience(); 
	
	/**
	 * The filters to apply before publishing logs to the listeners.
	 * The filters are not applied to XML listeners.
	 * These filters are applied after the audience.
	 */
	private FiltersVector filters=null;
	
	/**
	 * Set the filters to apply to incoming logs before sending to
	 * the listeners
	 * 
	 * @param filters The filters to apply
	 *                If <code>null</code> or empty the filtering is disabled
	 */
	public void setFilters(FiltersVector filters) {
		this.filters=filters;
	}
	
	/**
	 * Set the audience
	 * 
	 * @param newAudience The new audience as defined in log_audience IDL module
	 * @see <code>LCEngine.setFilters()</code>
	 */
	public void setAudience(Audience newAudience) {
		if (newAudience==null) {
			throw new IllegalArgumentException("The audience can't be null");
		}
		audience=newAudience;
	}

	/**
	 * @return the audience
	 */
	public Audience getAudience() {
		return audience;
	}

	/**
	 * @param discardLevel the discardLevel to set
	 */
	public void setDiscardLevel(LogTypeHelper discardLevel) {
		actualDiscardLevel = discardLevel;
	}

	/**
	 * @return the filters
	 */
	public FiltersVector getFilters() {
		return filters;
	}
	
	/**
	 * Check if the passed log matches with the constraints.
	 * <P>
	 * The log is checked in the following order:
	 * <OL>
	 * 	<LI>the discard level
	 *  <LI>the audience
	 *  <LI>the filters
	 * </OL>
	 * 
	 * @param log The not <code>null</code> log to check
	 * @return <code>true</code> if the log matches the criteria
	 */
	public final boolean match(ILogEntry log) {
		if (log==null) {
			throw new IllegalArgumentException("The log can't be null");
		}
		if (actualDiscardLevel!=null && log.getType().ordinal()<=actualDiscardLevel.ordinal()) {
				return false;
		}
		// Check the log against the audience
		if (!audience.matches(log)) {
			return false;
		}
		if (filters==null) {
			return true;
		} else {
			return filters.applyFilters(log);
		}
	}
	
	/**
	 * Return the discard level used to filter out logs.
	 * 
	 * @return The discard level in use
	 */
	public LogTypeHelper getActualDiscardLevel() {
		return actualDiscardLevel;
	}

}
