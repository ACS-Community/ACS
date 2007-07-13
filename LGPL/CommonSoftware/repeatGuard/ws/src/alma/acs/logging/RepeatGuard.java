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

// $Author: cparedes $
// $Date: 2007/07/13 07:21:45 $
// $Log: RepeatGuard.java,v $
// Revision 1.2  2007/07/13 07:21:45  cparedes
// Changing the 2nd ctor, to work fine
//
// Revision 1.1  2007/07/10 14:59:18  hmeuss
// Added Java implementation, but for some reason TAT does not work for the test here. Needs repair!
// 
package alma.acs.logging;

/**
 * This is a copy from Nicolas' C++ RepeatGuard implementation. See there all
 * the description, comments and documentation.
 * 
 * @author hmeuss
 * 
 */
public class RepeatGuard {

	int evaluationMethod; // specifies whether time interval and number of

	// repetitions

	// should be evaluated conjunctive or diskunctive. Must be one
	// of the following values:

	public static final int AND = 0;

	public static final int OR = 1;

	public static final int TIMER = 2;

	public static final int COUNTER = 3;

	int maxRepetitions;

	long endTime, interval;

	int counter;

	int counterAtLastCheck;

	/**
	 * ctor
	 * 
	 * @param interval
	 *            Time interval in 100 nanosecond units!
	 * @param maxRepetitions
	 * @param or_or_and
	 *            0 for AND, 1 for OR
	 */
	RepeatGuard(long interv, int maxRep, int or_or_and) {
		evaluationMethod = or_or_and;
		if (interv == 0) {
			evaluationMethod = COUNTER;
		}
		if (maxRep == 0) {
			evaluationMethod = TIMER;
		}
 
		maxRepetitions = maxRep;
		counter = 0;
		counterAtLastCheck = 0;
		interval = interv / 100;// interval is measured
		// in 100 nanosconds as
		// base unit
		endTime = System.nanoTime() + interval;
    }

	/**
	 * ctor, convenience fo rthe above, using AND evaluationMethod
	 * 
	 * @param interval
	 *            Time interval in 100 nanosecond units!
	 * @param maxRepetitions
	 */
	RepeatGuard(long interv, int maxRep) {
		this(interv, maxRep, 0);
	}

	// check returns true, iff the last call for check was longer ago than
	// interval, and (or) increment has been called more than maxRepetitions
	synchronized boolean check() {
		long now = System.nanoTime();
        switch (evaluationMethod) {
		case AND:
			if ((now >= endTime) && (counter >= maxRepetitions)) {
                counterAtLastCheck = counter;
				counter = 0;
				endTime = now + interval;
				return true;
			}
			return false;
		case OR:
			if ((now >= endTime) || (counter >= maxRepetitions)) {
				counterAtLastCheck = counter;
				counter = 0;
				endTime = now + interval;
				return true;
			}
			return false;
		case TIMER:
			if (System.nanoTime() >= endTime) {
				counterAtLastCheck = counter;
				counter = 0;
				endTime = now + interval;
				return true;
			}
			return false;
		case COUNTER:
			if (counter >= maxRepetitions) {
				counterAtLastCheck = counter;
				counter = 0;
				endTime = now + interval;
				return true;
			}
			return false;
		}
		return false;
	}

	synchronized boolean checkAndIncrement() {
		counter++;
		return check();
	}

	synchronized void increment() {
		counter++;
	}

	int count() {
		return counterAtLastCheck;
	}

	synchronized void reset() {
		counter = 0;
		counterAtLastCheck = 0;
		endTime = System.nanoTime() + interval;
	}

	synchronized void reset(long interv, int maxRep) {
		reset(interv, maxRep, 0);
	}

	synchronized void reset(long interv, int maxRep, int or_or_and) {
		evaluationMethod = or_or_and;
		if (interv == 0) {
			evaluationMethod = COUNTER;
		}
		if (maxRep == 0) {
			evaluationMethod = TIMER;
		}
		maxRepetitions = maxRep;
		counter = 0;
		counterAtLastCheck = 0;
		interval = interv / 100;// interval is measured
		// in 100 nanosconds as
		// base unit
		endTime = System.nanoTime() + interval;
	}
}
