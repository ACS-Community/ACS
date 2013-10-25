/*
ALMA - Atacama Large Millimiter Array
* Copyright (c) European Southern Observatory, 2013 
* 
* This library is free software; you can redistribute it and/or
* modify it under the terms of the GNU Lesser General Public
* License as published by the Free Software Foundation; either
* version 2.1 of the License, or (at your option) any later version.
* 
* This library is distributed in the hope that it will be useful,
* but WITHOUT ANY WARRANTY; without even the implied warranty of
* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
* Lesser General Public License for more details.
* 
* You should have received a copy of the GNU Lesser General Public
* License along with this library; if not, write to the Free Software
* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
*/
package alma.alarmsystem.clients.alarm;

/**
 * A interface to pass statistical information as well as 
 * alarms and exceptions.
 * 
 * @author acaproni
 * @since ACS-12.2
 */
public interface AlarmCategoryStatListener {
	
	/**
	 * The number of active alarms
	 * <P>
	 * <EM>Note</EM>: if the statistic for one of the value is available then its value is set to <code>null</code>.
	 * 
	 * @param active Global number of active alarms
	 * @param priority1 Number of active alarms whose priority is 1
	 * @param priority2 Number of active alarms whose priority is 2
	 * @param priority3 Number of active alarms whose priority is 3
	 * @param priority4 Number of active alarms whose priority is 4
	 */
	public void activationAlarmsStats(
			Integer active, 
			Integer priority1, 
			Integer priority2, 
			Integer priority3, 
			Integer priority4);
	
	/**
	 * The number of alarms having the following reduction properties set.
	 * <P>
	 * <EM>Note</EM>: if the statistic for one of the value is available then its value is set to <code>null</code>.
	 * @param reduced The number of reduced alarms
	 * @param masked The number of masked alarms
	 * @param multiplicityParent  The number of alarms that are parent in a multiplicity reduction rule
	 * @param nodeParent  The number of alarms that are parent in a node reduction rule
	 * @param multiplicityChild  The number of alarms that are child in a multiplicity reduction rule
	 * @param nodeChield  The number of alarms that are child in a node reduction rule
	 */
	public void reductionAlarmsStat(
			Integer reduced, 
			Integer masked, 
			Integer multiplicityParent, 
			Integer nodeParent, 
			Integer multiplicityChild, 
			Integer nodeChield);
}
