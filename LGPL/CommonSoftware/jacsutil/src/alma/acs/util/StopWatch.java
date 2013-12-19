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
 */
package alma.acs.util;

import java.text.DecimalFormat;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.TimeUnit;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Class that facilitates elapsed-time runtime profiling.
 * For per-thread time profiling better use an external profiler.
 * <p>
 * This class requires external synchronization if used from different threads.
 * <p>
 * About system clock issues and the JVM, see 
 * https://blogs.oracle.com/dholmes/entry/inside_the_hotspot_vm_clocks
 * <p>
 * Over the years also other open source libs have developed similar classes, but as of ACS 12.2
 * we do not see a need to replace our StopWatch or change its API. 
 * See http://docs.guava-libraries.googlecode.com/git/javadoc/com/google/common/base/Stopwatch.html,
 * http://jamonapi.sourceforge.net/, 
 * http://commons.apache.org/proper/commons-lang/javadocs/api-release/index.html?org/apache/commons/lang3/time/StopWatch.html
 * 
 * @author hsommer May 7, 2003 2:18:39 PM
 */
public class StopWatch
{
	// todo: set through Java property etc.
	private static boolean s_shutup = false; 
	
	
	/**
	 * When printing elapsed time in millisec, we only want to show one fractional digit.
	 * As hardware gets more accurate, the number of decimal digits could be increased in the future. 
	 * <p>
	 * For a fixed size of decimal digits (padding zeros if needed), we could change the current format
	 * to something like "#.000".
	 */
	private final NumberFormat millisecFormatter = new DecimalFormat("#.#");
	
	/**
	 * Optional logger. If null, then we write log messages to stdout instead.
	 */
	private Logger m_logger;
	
	/**
	 * Name of the subtask that this StopWatch was created for (see {@link #createStopWatchForSubtask(String)},
	 * or <code>null</code> if this is a top-level StopWatch.
	 */
	private String subtaskName;
	
	/**
	 * List of subtask StopWatches. Created on demand.
	 */
	private List<StopWatch> subtaskStopWatches;
	
	private long startTimeNanos;
	
	/**
	 * Set by the optional call to {@link #stop()}, otherwise marked unset by value -1.
	 */
	private long stopTimeNanos = -1;
	
	
	/**
	 * Ctor without logger provided.
	 * Either call <code>setLogger</code> later, or logs will be printed to stdout.
	 */
	public StopWatch() {
		this(null);
	}

	/**
	 * Constructor that resets the time count.
	 * 
	 * @param logger
	 *            the logger to be used in {@link #logLapTime(String)}. If <code>null</code>, logs will be printed to
	 *            stdout.
	 */
	public StopWatch(Logger logger) {
		this(logger, null);
	}

	/**
	 * @see #createStopWatchForSubtask(String)
	 * @since ACS 12.3
	 */
	private StopWatch(Logger logger, String subtaskName) {
		m_logger = logger;
		this.subtaskName = subtaskName;
		reset();
	}
	
	
	public void setLogger(Logger logger) {
		m_logger = logger;
	}

	public void reset()
	{
		startTimeNanos = System.nanoTime();
		stopTimeNanos = -1;
		if (subtaskStopWatches != null) {
			subtaskStopWatches.clear();
		}
	}

	/**
	 * Creates a StopWatch for a subtask. 
	 * The returned StopWatch can be used to profile blocks of code 
	 * to get a single log in the end that contains details about how the total time was spent.
	 * The returned StopWatch can be used to create more sub-StopWatches, which is convenient 
	 * if you pass it to a called method which itself uses subtask timing.
	 * <br>
	 * When a subtask is done, you must call {@link #stop()} on the subtask's StopWatch.
	 * <p>
	 * Do not expect that subtask elapsed times add up exactly to the total elapsed time. 
	 * The system-dependent timing granularity does not allow this.
	 */
	public StopWatch createStopWatchForSubtask(String newSubtaskName) {
		StopWatch sw = new StopWatch(m_logger, newSubtaskName);
		if (subtaskStopWatches == null) {
			subtaskStopWatches = new ArrayList<StopWatch>();
		}
		subtaskStopWatches.add(sw);
		return sw;
	}

	/**
	 * Stops the StopWatch. 
	 * This method must be called for subtask StopWatches (see {@link #createStopWatchForSubtask(String)}).
	 * It is optional in case of top-level StopWatches where it makes little difference whether or not 
	 * we first stop the watch right before retrieving or logging the lap time. 
	 */
	public void stop() {
		if (stopTimeNanos < 0) {
			stopTimeNanos = System.nanoTime();
			
			// Stop subtask watches recursively just in case. The user should have done this already.
			if (subtaskStopWatches != null) {
				for (StopWatch sw : subtaskStopWatches) {
					sw.stop();
				}
			}
		}
	}
	
	/**
	 * Gets the time in milliseconds that has elapsed since this object was created
	 * or <code>reset()</code> was called. 
	 * <p>
	 * The implementation relies on <code>System.nanoTime()</code>,
	 * so that the granularity of measurements is OS dependent and can be more than one ms.
	 * Also, note that the time spent on all threads together is returned, which might
	 * be a poor measure for runtime profiling of a particular method.
	 * <p>
	 * @return  elapsed time in milliseconds
	 */
	public long getLapTimeMillis() {
		return TimeUnit.NANOSECONDS.toMillis(getLapTimeNanos());
	}

	/**
	 * Converts {@link #getLapTimeNanos()} to fractional milliseconds,
	 * using the format from {@link #millisecFormatter}. 
	 * @return
	 */
	public String formattedMillis() {
		return millisecFormatter.format(getLapTimeNanos() * 1E-6D);
	}

	/**
	 * Gets the time in nanoseconds that has elapsed since this object was created
	 * or <code>reset()</code> was called. 
	 * <p>
	 * The implementation simply relies on <code>System.nanoTime()</code>,
	 * so that the granularity of measurements is OS dependent.
	 * Also, note that the time spent on all threads together is returned, which might
	 * be a poor measure for runtime profiling a particular method.   
	 * <p>
	 * @return  elapsed time in nanoseconds
	 */
	public long getLapTimeNanos() {
		long endTime = ( stopTimeNanos > 0 ? stopTimeNanos : System.nanoTime() );
		return (endTime - startTimeNanos);
	}
	
	/**
	 * Logs a message about the elapsed time for a certain task.
	 * It uses log level <code>Level.FINE</code> and inserts the supplied description: 
	 * "<code>elapsed time in ms to </code><i>taskDescription</i><code>: </code><i>elapsed-time</i>".
	 * <p>
	 * If no logger has been supplied, it will get one using {@link Logger#getLogger(java.lang.String)}.
	 * This ad-hoc logger will likely not work in an ACS environment where log handlers are configured
	 * for the needs of containers and remote logging.
	 * <p>
	 * todo: provide nicer text mask for message
	 * 
	 * @param	taskDesc the message to be include in the standard message
	 * 			which should describe the task for which the lap time is taken.
	 * @see #getLapTimeMillis()
	 */
	public void logLapTime(String taskDesc) {
		logLapTime(taskDesc, Level.FINE);
	}
	
	/**
	 * Variant of {@link #logLapTime(String)} that allows the user to specify
	 * the log level. Generally the FINE (=DEBUG) level is appropriate for performance logs, 
	 * but in special cases such as unit tests it can be necessary to log at INFO or other levels.
	 */
	public void logLapTime(String taskDesc, Level logLevel) {
		logLapTime(taskDesc, logLevel, false);
	}

	/**
	 * @since ACS 12.3
	 */
	public void logLapTimeWithSubtaskDetails(String taskDesc, Level logLevel) {
		logLapTime(taskDesc, logLevel, true);
	}
	
	
	private void logLapTime(String taskDesc, Level logLevel, boolean withSubtaskDetails) {
		if (!s_shutup) {
			try {
				StringBuilder sb = new StringBuilder();
				sb.append("elapsed time in ms to ").append(taskDesc).append(": ");
				if (withSubtaskDetails && subtaskStopWatches != null) {
					recursivePrintLapTimes(sb, 0);
				}
				else {
					sb.append(formattedMillis());
				}
				
				if (m_logger != null) {
					m_logger.log(logLevel, sb.toString());
				}
				else {
					System.out.println(sb.toString());
				}
			}
			catch (Throwable thr) {
				// just to be safe -- really don't want to mess with the running application...
			} 
		}
	}

	
	/**
	 * @param sb
	 * @param depth Could be used for formatting (indentation etc)
	 */
	private void recursivePrintLapTimes(StringBuilder sb, int depth) {
		// print our total time first
		if (this.subtaskName != null) {
			sb.append(subtaskName).append("=");
		}
		sb.append(formattedMillis());
		
		// then print subtask details
		if (subtaskStopWatches != null) {
			sb.append(" { ");
			for (int i = 0; i < subtaskStopWatches.size(); i++) {
				StopWatch sw = subtaskStopWatches.get(i);
				sw.recursivePrintLapTimes(sb, depth + 1);
				if (i < subtaskStopWatches.size() - 1) {
					sb.append(',');
				}
				sb.append(' ');
			}
			sb.append('}');
		}
	}
}
