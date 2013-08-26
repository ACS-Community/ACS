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
package alma.acs.shutdown;

import java.util.logging.Logger;

/**
 * A subclass can be registered to the JVM Runtime as a shutdown thread to be run at shutdown, 
 * both for regular termination and for interruption like Ctrl-C (SIGINT).
 * Make sure to call {@link #setRegularShutdownExpected()} before regularly exiting the JVM, 
 * as otherwise {@link #interruptDetected()} would be called erroneously.
 * <p>
 * Recommended reading: http://www-106.ibm.com/developerworks/ibm/library/i-signalhandling/ 
 * (also talks about signals for tougher mishaps than Ctrl-c).
 * See also {@link java.lang.Runtime#addShutdownHook(Thread)}
 * <p>
 * Abstracted from the java container's shutdown hook, to simplify registering such hooks also in jlog etc.
 * @author hsommer
 */
public abstract class ShutdownHookBase extends Thread
{
	protected volatile boolean m_regularShutdownExpected = false;

	protected final Logger logger;
	protected final String processName;

	/**
	 * @param logger
	 *           The logger to be used by this object.
	 * @param processName
	 *           Used as the thread name, with "ShutdownHook" appended. 
	 */
	public ShutdownHookBase(Logger logger, String processName) {
		super(processName + "ShutdownHook");
		this.processName = processName;
		this.logger = logger;
	}

	/**
	 * @see java.lang.Runnable#run()
	 */
	public void run() {
		try {
			if (m_regularShutdownExpected) {
				regularTermination();
			} else {
				interruptDetected();
			}
		} catch (Throwable thr) {
			System.err.println("failure in ShutdownHook#run: ");
			// at this stage it's too risky to use the logger
			thr.printStackTrace();
		}

	}

	/**
	 * Subclasses can override this method to log nothing or something else.
	 * No other action is expected in the overridden method. 
	 */
	protected void regularTermination() {
		// we can assume a regular exit (unless something went wrong after setRegularShutdownExpected() had been called
		logger.info("about to exit the JVM of " + processName);
	}

	/**
	 * Is called when the JVM is about to terminate due to an interrupt signal. 
	 * Subclasses should perform emergency cleanup.
	 */
	abstract protected void interruptDetected();

	/**
	 * Must be called right before the program ends. This helps the ShutdownHook to decide 
	 * whether it's being called for a regular program termination or because of an interrupt.
	 */
	public void setRegularShutdownExpected() {
		m_regularShutdownExpected = true;
	}

}
