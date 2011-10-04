/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
 *    Copyright by ESO (in the framework of the ALMA collaboration),
 *    All rights reserved
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
 *
 *    Created on May 24, 2007
 *
 */

package alma.acs.logging;

import java.util.concurrent.TimeUnit;

/**
 * This class can be used to reduce the repeated execution of any kind of code, 
 * by keeping track of the number of intended executions and the time passed since the last actual execution,
 * and comparing these against configured values (see details below).
 * <p>
 * A reduction of the number of executions can be useful if that code is resource intensive (e.g. DB access, remote calls, logging)
 * or has other unwanted side effects if executed too often (e.g. annoying the user with too frequent error or confirmation dialogues). 
 * Of course this only makes sense if the nature of the problem allows skipping some of these executions.
 * <p>
 * This class is so general that it does not actually execute any code; it simply keeps track of counters and timer.
 * It can be used directly to manage decisions about skipping or executing some code.
 * For example, the application can instantiate a <code>RepeatGuard</code> with the timer set to one second. 
 * Then whenever the repetitive action would be called, the application first calls {@link #check()};
 * for every timer interval, only the first call to <code>check()</code> will return <code>true</code>, 
 * while the subsequent calls will return <code>false</code>, and the application should skip the repeated block of code.
 * Alternatively this class can be extended or wrapped to become easier to use for specific purposes.
 * Then typically the code to be executed (e.g. logging a message) becomes part of the specialized repeat guard class, 
 * as in {@link RepeatGuardLogger}.
 * <p>
 * The concept of repeat guards in ACS is discussed at http://almasw.hq.eso.org/almasw/bin/view/ACS/LoggingRepetitionControl
 * <p> 
 * Repetitions can be reduced using
 * <ol>
 *   <li>the number of times that code execution should be skipped before the code can be executed again
 *   <li>a timer which allows only one execution per time interval,
 *       no matter how many execution attempts have been made in the meantime,
 *   <li><code>OR</code> combination of the above: either enough attempts were made or enough time has passed, whatever happens first
 *   <li><code>AND</code> combination: enough skipped execution attempts, and enough time passed. 
 * </ol>
 * see also the {@link Logic} enum.
 */
public class RepeatGuard {

	/*** Evaluation logic: TIMER (time based guard), COUNTER (count based guard), AND/OR (conjunction/disjunction or both) */
	public enum Logic { AND, OR, TIMER, COUNTER }
	
	private Logic evaluationMethod;

	private int maxRepetitions;

	private long endTimeNs, intervalNs;

	private int counter;

	/**
	 * @see #counterAtLastExecution()
	 */
	private int counterAtLastExecution;

	private boolean firstTime = true;

	/**
	 * Constructor.
	 * 
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 * @param logic Evaluation logic for <code>interval</code> and <code>maxRepetitions</code>. 
	 *        The logic will be "reduced" automatically if <code>interval</code> or <code>maxRepetitions</code> 
	 *        have a value <= 0, so as to be based only on the other positive value.
	 * @throws IllegalArgumentException if maxRepetitions <= 0 && interval <= 0
	 */
	public RepeatGuard(long interval, TimeUnit timeUnit, int maxRepetitions, Logic logic) {
		reset(interval, timeUnit, maxRepetitions, logic);
	}

	/**
	 * Constructor, convenience for the above, using {@link Logic.OR} evaluation method
	 * if both <code>interval</code> and <code>maxRepetitions</code> are positive values, 
	 * otherwise {@Logic.TIMER} or {@Logic.COUNTER} to make sure that only the respective parameter with a positive value gets used.
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 * @throws IllegalArgumentException if maxRepetitions <= 0 && interval <= 0
	 */
	public RepeatGuard(long interval, TimeUnit timeUnit, int maxRepetitions) {
		this(interval, timeUnit, maxRepetitions, Logic.OR);
	}

	/**
	 * This method checks if the guarded activity is due for execution, or if it should be skipped instead.
	 * <p>
	 * For the first call, it always returns true.
	 * Later it returns <code>true</code> if the last call to <code>check()</code>was longer ago than the <code>interval</code> 
	 * given in the constructor or in the <code>reset</code> methods, 
	 * and/or if the internal counter has been incremented more than <code>maxRepetitions</code> 
	 * by calls to {@link #increment()} or {@link #checkAndIncrement()}.
	 * @return <code>true</code> if guarded activity should be run, <code>false</code> if it should be skipped.
	 */
	public synchronized boolean check() {
		
		long now = System.nanoTime();

		// first time check always returns true, regardless of counter and timer. Then resets timer and counter.
		if (firstTime) {
			firstTime = false;
			
			counterAtLastExecution = counter;
			counter = 0;
			endTimeNs = now + intervalNs;
			
			return true;
		}

		switch (evaluationMethod) {
			case AND:
				if ((now >= endTimeNs) && (counter >= maxRepetitions)) {
					counterAtLastExecution = counter;
					counter = 0;
					endTimeNs = now + intervalNs;
					return true;
				}
				return false;
				
			case OR:
				if ((now >= endTimeNs) || (counter >= maxRepetitions)) {
					counterAtLastExecution = counter;
					counter = 0;
					endTimeNs = now + intervalNs;
					return true;
				}
				return false;
				
			case TIMER:
				if (now >= endTimeNs) {
					counterAtLastExecution = counter;
					counter = 0;
					while (endTimeNs <= now) { // we may have to "catch up" several timer intervals during which nothing was logged
						endTimeNs += intervalNs; //endTime + interval instead of now + interval to prevent drift
					}
					return true;
				}
				return false;
				
			case COUNTER:
				if (counter >= maxRepetitions) {
					counterAtLastExecution = counter;
					counter = 0;
					endTimeNs = now + intervalNs;
					return true;
				}
				return false;
		}
		return false;
	}

	/**
	 * Increments the counter and checks (see {@link #check()}).
	 * @return <code>true</code> if OK, <code>false</code> if should be guarded
	 * @see #check()
	 */
	public synchronized boolean checkAndIncrement() {
		counter++;
		return check();
	}

	/**
	 * Resets and reconfigures this guard using the given interval, maxRepetitions, and {@link Logic.OR} logic.
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of skipped repetitions.
	 * @throws IllegalArgumentException if maxRepetitions <= 0 && interval <= 0
	 * @see #RepeatGuard(long, TimeUnit, int, Logic)
	 */
	public void reset(long interval, TimeUnit timeUnit, int maxRepetitions) {
		reset(interval, timeUnit, maxRepetitions, Logic.OR);
	}

	/**
	 * Resets and reconfigures logic of guard.
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of skipped repetitions.
	 * @param logic Evaluation logic for <code>interval</code> and <code>maxRepetitions</code>. 
	 *        The logic will be "reduced" automatically if <code>interval</code> or <code>maxRepetitions</code> 
	 *        have a value <= 0, so as to be based only on the other positive value.
	 * @throws IllegalArgumentException if <code>maxRepetitions <= 0 && interval <= 0</code> or required arg == null
	 */
	public synchronized void reset(long interval, TimeUnit timeUnit, int maxRepetitions, Logic logic) {

		// check parameters
		if (maxRepetitions <= 0 && interval <= 0) {
			throw new IllegalArgumentException("maxRepetitions <= 0 && interval <= 0 not allowed.");
		}
		
		if (timeUnit == null && interval > 0) {
			throw new IllegalArgumentException("A timeUnit must be be specified.");
		}
		
		if (logic == null) {
			throw new IllegalArgumentException("A 'logic' enum value must be specified.");
		}
		
		// use only maxRepetitions if interval <= 0
		if (interval <= 0) {
			evaluationMethod = Logic.COUNTER;
		}
		// use only interval if maxRepetitions <= 0
		else if (maxRepetitions <= 0) {
			evaluationMethod = Logic.TIMER;
		}
		else {
			// use interval and/or maxRepetitions as given by the logic parameter.
			this.evaluationMethod = logic;
		}

		this.intervalNs = ( timeUnit != null ? timeUnit.toNanos(interval) : 0 );
		this.maxRepetitions = maxRepetitions;

		reset();
	}

	/**
	 * Resets this guard without changing the configuration for timer, counter and logic.
	 */
	public synchronized void reset() {
		counter = 0;
		counterAtLastExecution = 0;
		firstTime = true;
	}

	/**
	 * Increase counter value.
	 */
	public synchronized void increment() {
		counter++;
	}
		
	/**
	 * Get current counter value.
	 * @return current counter value.
	 */
	public synchronized int counter() {
		return counter;
	}

	/**
	 * Gets the value of the counter that it had when the {@link #check()} or
	 * {@link #checkAndIncrement()} method returned <code>true</code> the last time,
	 * which corresponds to the number of times the activity was skipped before it got executed.
	 * <p>
	 * Calling this method does not make sense if only timer logic was used (no counters configured nor incremented)
	 */
	public synchronized int counterAtLastExecution() {
		return counterAtLastExecution;
	}
	
}
