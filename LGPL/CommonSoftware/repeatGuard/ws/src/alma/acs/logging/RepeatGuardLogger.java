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

// $Author: hsommer $
// $Date: 2011/09/20 16:21:39 $
// $Log: RepeatGuardLogger.java,v $
// Revision 1.5  2011/09/20 16:21:39  hsommer
// Removed the deprecated (since ACS 8.0) variant of methods "log", "logAndIncrement" which takes a logger as parameter. Now the logger must be passed in the constructor.
//
// Revision 1.4  2008/12/16 16:39:31  hsommer
// - renamed logAndIncrement(Level, String, Throwable) to "log(..)" to be consistent with the other methods that also don't mention the incrementing in their name
// - improved javadoc
// - new method isLoggingEnabled to allow using more exotic Logger API methods or batches of logs controlled by this one RepeatGuardLogger
//
// Revision 1.3  2008/12/11 10:20:06  hsommer
// - Fixed a bug that caused the class and method name and the line-of-code to be inferred from this RepeatGuardLogger itself instead of from the client class that actually does the logging.
// - For that fix to work, an AcsLogger instead of a JDK Logger must be passed to the existing (and now deprecated) methods "log(Logger logger, Level priority, String message)", "logAndIncrement(Logger logger, Level priority, String message)"
// - Added a constructor that takes an AcsLogger, and the new method "log(Level level, String message)". This pair of ctor and log method will replace the old constructor without AcsLogger and the two deprecated log methods with a Logger parameter.
// - Added method "logAndIncrement(Level level, String message, Throwable thr)" that allows logging execeptions correctly (i.e. as a parameter)
// - Added factory methods "createTimeBasedRepeatGuardLogger" and "createCounterBasedRepeatGuardLogger" to give alternatives to using non-positive values in the generic constructor.
// - Wrote no-nonsense javadoc descriptions;
//
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
 * This class wraps a logger so that logging of records is skipped until a given number of logging attempts or given time has passed.
 * It is based on {@link RepeatGuard}.
 * Typically you want to create one <code>RepeatGuardLogger</code> for each guarded kind of log message (= logging line in your code). 
 * Only if you want to control the repetition of different kinds of log messages (at different places in your code) using the same counter and timer
 * (which could mean that one kind of log may always be suppressed while another type may always be logged) then you should reuse one instance 
 * of this class for different lines of logging in your code.
 * <p>
 * There seems to be no real use case for passing different loggers to one repeat guard, even though this was described in the original design.
 * Therefore we plan to remove {@link #log(Logger, Level, String)} and {@link #logAndIncrement(Logger, Level, String)} in the future. 
 * If you feel they are useful (because you have a case where the logger cannot or should not be passed in the constructor) then 
 * we can keep these methods as alternatives to the recommended passing of the logger in the constructor. Please report this to ACS.
 */
public class RepeatGuardLogger {
	
	protected final RepeatGuard guard;

	private final AcsLogger logger;

	/**
	 * Constructor for a time and counter based repeat guard logger.
	 * <p>
	 * If only time or counter should apply, but not both together, it is better to use the factory methods 
	 * {@link #createTimeBasedRepeatGuardLogger(AcsLogger, long, TimeUnit)}
	 * or {@link #createCounterBasedRepeatGuardLogger(AcsLogger, int)}.
	 * However it is also possible to use negative values for the quantities that should not be considered.
	 * <p>
	 * @param logger The logger to be used in 
	 * @param interval Time interval (in <code>timeUnit</code> units).
	 * @param timeUnit Time unit of <code>interval</code> parameter.
	 * @param maxRepetitions Maximum number of skipped repetitions.
	 * @throws IllegalArgumentException if interval <= 0 && maxRepetitions <= 0
	 */
	public RepeatGuardLogger(AcsLogger logger, long interval, TimeUnit timeUnit, int maxRepetitions) {
		guard = new RepeatGuard(interval, timeUnit, maxRepetitions);
		this.logger = logger;
		logger.addLoggerClass(getClass());
	}
	
	/**
	 * Factory method for a repeat guard logger which only uses a timer, regardless of the number of skipped logs.
	 * @throws IllegalArgumentException if interval <= 0
	 * @see #RepeatGuardLogger(AcsLogger, long, TimeUnit, int)
	 */
	public static RepeatGuardLogger createTimeBasedRepeatGuardLogger(AcsLogger logger, long interval, TimeUnit timeUnit) {
		return new RepeatGuardLogger(logger, interval, timeUnit, -1);
	}
	
	/**
	 * Factory method for a repeat guard logger which only uses a counter of skipped logs, regardless of the time passed since the last log.
	 * @throws IllegalArgumentException if maxRepetitions <= 0
	 * @see #RepeatGuardLogger(AcsLogger, long, TimeUnit, int)
	 */
	public static RepeatGuardLogger createCounterBasedRepeatGuardLogger(AcsLogger logger, int maxRepetitions) {
		return new RepeatGuardLogger(logger, -1, TimeUnit.SECONDS, maxRepetitions);
	}
	
	

	/**
	 * Logs the message at the given level, unless the internal <code>RepeatGuard</code> prevents this based on the timer and/or log record counter.
	 * If a log record counter is active, it will be advanced, which corresponds to {@link RepeatGuard#checkAndIncrement()}.
	 * (Note that following the same terminology as {@link RepeatGuard}, this method would have to be called <code>logAndIncrement</code>;
	 * it is simply called <code>log</code> though because here we don't support the variant of having a counter enabled without using it.)
	 * 
	 * @see Logger#log(Level, String)
	 * @since ACS 8.0.0
	 */
	public void log(Level level, String message) {
		if (guard.checkAndIncrement()) {
			logger.log(level, message);
		}
	}
	
	/**
	 * Same as {@link #log(Level, String)} but with additional <code>Throwable</code> to be logged.
	 * @see Logger#log(Level, String, Throwable)
	 * @since ACS 8.0.0
	 */
	public void log(Level level, String message, Throwable thr) {
		if (guard.checkAndIncrement()) {
			logger.log(level, message, thr);
		}
	}
	
	/**
	 * Checks if the internal repeat guard allows a log.
	 * This method is an alternative to the various <code>log</code> methods to be used if
	 * <ol>
	 * <li>You want the repeat guard to skip or enable different log messages together (all or none).
	 * <li>You want to use specialized methods of the guarded Logger, such as {@link Logger#log(java.util.logging.LogRecord)},
	 *     which are not exposed by this <code>RepeatGuardLogger</code>
	 * </ol>
	 * In either of these cases, simply surround a call to the normal logger with an <br>
	 * <code>if (isLoggingEnabled()) {myLogger.blabla}</code>.
	 * <p>
	 * The internal log counter (if present) will be incremented the same way as for one call to, say, {@linkplain #log(Level, String)}.
	 * <p>
	 * Note that you might be better off using {@link RepeatGuard} directly if this method is the only one you need from this <code>RepeatGuardLogger</code>. 
	 * Using this method makes sense though for a mix of logs, some to be made via the <code>RepeatGuardLogger.log</code> methods, 
	 * and others directly via the logger surrounded by a call to this <code>isLoggingEnabled</code> 
	 * @return true if the counter/timer allows a log message (or a batch of log messages that must appear together)
	 */
	public boolean isLoggingEnabled() {
		return guard.checkAndIncrement();
	}
	

}
