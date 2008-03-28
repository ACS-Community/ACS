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

import java.security.InvalidParameterException;
import java.util.concurrent.TimeUnit;

/**
 * This is a copy from Nicolas' C++ RepeatGuard implementation. See there all
 * the description, comments and documentation.
 * 
 * @author hmeuss
 */
public class RepeatGuard {

	/*** Evaluation logic: TIMER (time based guard), COUNTER (count based guard), AND/OR (conjunction/disjunction or both) */
	public enum Logic { AND, OR, TIMER, COUNTER }

	private Logic evaluationMethod;

	private int maxRepetitions;

	private long endTimeNs, intervalNs;

	private int counter;

	private int counterAtLastCheck;

	private boolean firstTime;

	/**
	 * Constructor.
	 * 
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 * @param logic Evaluation logic.
	 */
	public RepeatGuard(long interval, TimeUnit timeUnit, int maxRepetitions, Logic logic) {
		reset(interval, timeUnit, maxRepetitions, logic);
    }

	/**
	 * Constructor, convenience for the above, using OR evaluation method.
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 */
	public RepeatGuard(long interval, TimeUnit timeUnit, int maxRepetitions) {
		this(interval, timeUnit, maxRepetitions, Logic.OR);
	}

	/**
	 * Check returns true, if the last call for check was longer ago than
	 * interval and/or increment has been called more than maxRepetitions.
	 * @return <code>true</code> if OK, <code>false</code> if should be guarded
	 */
    public synchronized boolean check() {
        long now = System.nanoTime();

        // first time log
        if (firstTime)
        {
	        firstTime = false; 
	        counterAtLastCheck = counter;
	        counter = 0;
	        endTimeNs = now + intervalNs;
	        return true;
        }

        switch (evaluationMethod) {
			case AND:
				if ((now >= endTimeNs) && (counter >= maxRepetitions)) {
	                counterAtLastCheck = counter;
					counter = 0;
					endTimeNs = now + intervalNs;
					return true;
				}
				return false;
				
			case OR:
				if ((now >= endTimeNs) || (counter >= maxRepetitions)) {
					counterAtLastCheck = counter;
					counter = 0;
					endTimeNs = now + intervalNs;
					return true;
				}
				return false;
			case TIMER:
				if (now >= endTimeNs) {
					counterAtLastCheck = counter;
					counter = 0;
					endTimeNs = endTimeNs + intervalNs; //endTime + interval instead of now + interval to prevent drift
					return true;
				}
				return false;
			case COUNTER:
				if (counter >= maxRepetitions) {
					counterAtLastCheck = counter;
					counter = 0;
					endTimeNs = now + intervalNs;
					return true;
				}
				return false;
		}

        return false;
	}

    /**
     * Increments and checks (see {@link #check()}).
	 * @return <code>true</code> if OK, <code>false</code> if should be guarded
     * @see #check()
     */
	public synchronized boolean checkAndIncrement() {
		counter++;
		return check();
	}

	/**
	 * Reset and reconfigure logic of guard using OR logic.
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 */
	public void reset(long interval, TimeUnit timeUnit, int maxRepetitions) {
		reset(interval, timeUnit, maxRepetitions, Logic.OR);
	}

	/**
	 * Reset and reconfigure logic of guard.
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 * @param logic Evaluation logic.
	 */
	public synchronized void reset(long interval, TimeUnit timeUnit, int maxRepetitions, Logic logic) {
		this.evaluationMethod = logic;

		// check paramaters
		if (maxRepetitions <= 0 && interval <= 0) {
			throw new InvalidParameterException("maxRepetitions <= 0 && interval <= 0");
		}
		// check interval parameter, counter fallback
		else if (interval <= 0) {
			evaluationMethod = Logic.COUNTER;
		}
		// check max repetitions parameter, timer fallback
		else if (maxRepetitions <= 0) {
			evaluationMethod = Logic.TIMER;
		}
 
		this.intervalNs = timeUnit.toNanos(interval);
		this.maxRepetitions = maxRepetitions;

		reset();
	}

	/**
	 * Reset and reconfigure logic of guard.
	 */
	public synchronized void reset() {
		counter = 0;
		counterAtLastCheck = 0;
		firstTime = true;
		this.endTimeNs = System.nanoTime() + intervalNs;
	}

	/**
	 * Increase couter value.
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
	 * Get count at last check.
	 * @return count at last check.
	 */
	public synchronized int count() {
		return counterAtLastCheck;
	}
	
}
