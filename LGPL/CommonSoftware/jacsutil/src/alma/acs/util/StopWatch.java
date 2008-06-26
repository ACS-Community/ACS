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
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Class that facilitates runtime profiling.
 * 
 * @author hsommer May 7, 2003 2:18:39 PM
 */
public class StopWatch
{
	// todo: set through Java property etc.
	private static boolean s_shutup = false; 
	
	
	/**
	 * When printing elapsed time in millisec, we only want to show two fractional digits.
	 */
	private static final NumberFormat millisecFormatter = new DecimalFormat("#.##");
	
	private Logger m_logger;
	
	private long m_startTime;
	private long m_startTimeNanos;
	
	
	/**
	 * Ctor without logger provided.
	 * Either call <code>setLogger</code> later, or there will be a JDK logger used
	 * on demand.
	 */
	public StopWatch()
	{
		this(null);
	}

	/**
	 * Constructor that resets the time count.
	 * @param logger  the logger to be used in {@link #logLapTime(String)}. 
	 * 					If <code>null</code>, a new JDK logger will be constructed on demand.
	 */
	public StopWatch(Logger logger)
	{
		m_logger = logger;
		reset();
	}

	public void setLogger(Logger logger)
	{
		m_logger = logger;
	}

	public void reset()
	{
		m_startTime = System.currentTimeMillis();
		m_startTimeNanos = System.nanoTime();
	}


	/**
	 * Gets the time in milliseconds that has elapsed since this object was created
	 * or <code>reset()</code> was called. 
	 * <p>
	 * The implementation simply relies on <code>System.currentTimeMillis()</code>,
	 * so that the granularity of measurements is OS dependent and can be much more than one ms.
	 * Also, note that the time spent on all threads together is returned, which might
	 * be a poor measure for runtime profiling a particular method.   
	 * <p>
	 * @return  elapsed time in milliseconds
	 */
	public long getLapTimeMillis()
	{
		long now = System.currentTimeMillis();
		return (now - m_startTime);
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
	public long getLapTimeNanos()
	{
		long now = System.nanoTime();
		return (now - m_startTimeNanos);
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
	public void logLapTime(String taskDesc)
	{
		if (!s_shutup) {
			try {
				if (m_logger == null) {
					m_logger = Logger.getLogger("StopWatchLogger");
				}
				String elapsed = null;
				synchronized (millisecFormatter) {
					elapsed = millisecFormatter.format(getLapTimeNanos() * 1E-6);
				}
				m_logger.log(Level.FINE, "elapsed time in ms to " + taskDesc + ": " + elapsed);
			}
			catch (Throwable thr) {
				// just to be safe -- really don't want to mess with the running application...
			} 
		}
	}
}
