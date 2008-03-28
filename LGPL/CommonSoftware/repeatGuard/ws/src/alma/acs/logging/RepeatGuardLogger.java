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

// $Author: msekoran $
// $Date: 2008/03/28 13:05:33 $
// $Log: RepeatGuardLogger.java,v $
// Revision 1.2  2008/03/28 13:05:33  msekoran
// Java code cleanup.
//
// Revision 1.1  2007/07/10 14:59:18  hmeuss
// Added Java implementation, but for some reason TAT does not work for the test here. Needs repair!
// 
package alma.acs.logging;

import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * copied and translated from the corresponding C++ implementation. See there for comments and documentation.
 * Only the methods I needed are implemented, the rest can be added if necessary.
 * 
 * @author hmeuss
 * 
 */
public class RepeatGuardLogger {

	protected RepeatGuard guard;

	/**
	 * Constructor.
	 * 
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of repetitions.
	 */
	public RepeatGuardLogger(long interval, TimeUnit timeUnit, int maxRepetitions) {
		guard = new RepeatGuard(interval, timeUnit, maxRepetitions);
	}

	public void log(Logger logger, Level priority, String message) {
		if (guard.check()) {
			logger.log(priority, message);
		}
	}

	public void logAndIncrement(Logger logger, Level priority, String message) {
		if (guard.checkAndIncrement()) {
			logger.log(priority, message);
		}
	}
}
