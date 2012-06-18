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

/**
 * A listener for long lasting operations like for example the sending of a lot of
 * alarms read from a file.
 * <P>
 * If the task is composed of a definite number of steps, then number is
 * notified to the listener by {@link #slowTaskStarted(Integer)} and the progress
 * notified by {@link #slowTaskProgress(Integer)}.
 * <BR>
 * If the number of task is indefinite, as could happen by running a loop that
 * must be interrupted by the user, then {@link #slowTaskStarted(Integer)} is 
 * executed with a <code>null</code> parameter and {@link #slowTaskProgress(Integer)}
 * is not executed.
 * 
 * @author acaproni
 *
 */
public interface SlowTaskListener {
	
	/**
	 * The long lasting task just task started.
	 * 
	 * @param nSteps The number of steps to perform. 
	 * 				 <code>null</code> means that the number is undefined
	 * @param source The source of this message
	 */
	public void slowTaskStarted(Object source, Integer nSteps);
	
	/**
	 * The long lasting task just task terminated
	 * 
	 * @param source The source of this message
	 */
	public void slowTaskFinished(Object source);
	
	/**
	 * Inform about the progress of the task.
	 * The parameter tells the number of the current steps
	 * whose max value has been passed in {@link #slowTaskStarted(Integer)}.
	 * <P>
	 * Note that if the amount of steps is unknown, this method is never called.
	 * 
	 * @param progess The current step
	 * @param source The source of this message
	 */
	public void slowTaskProgress(Object source, int progess);
	
	/**
	 * The alarms have been read from the file or the TM/CDB
	 * 
	 * @param source The source of this message
	 * @param numOfAlarmsRead The number of alarms read
	 */
	public void alarmsRead(Object source, int numOfAlarmsRead);
}
