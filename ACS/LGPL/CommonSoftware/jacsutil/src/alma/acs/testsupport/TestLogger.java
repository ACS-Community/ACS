/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2004
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
package alma.acs.testsupport;

import java.util.logging.ConsoleHandler;
import java.util.logging.Handler;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * For logging in module tests which don't have the acsjlog module available
 * because they come before in the build list.
 * 
 * @author hsommer
 * created Aug 25, 2004 1:03:43 PM
 */
public class TestLogger
{
	/**
	 * Gets a console logger with <code>Level.FINEST</code>.
	 * @see #getLogger(String, Level)
	 */
	public static Logger getLogger(String name) {
		return getLogger(name, Level.FINEST);
	}
		
	/**
	 * Returns a simple JDK logger that's configured to use a <code>ConsoleHandler</code>.
	 * It will log messages to the console (syserr) and ignore log levels below <code>level</code>. 
	 * @param name  logger name
	 * @param level minimum level of messages to be not discarded
	 */
	public static Logger getLogger(String name, Level level) {
		Logger logger = Logger.getLogger(name);
		logger.setLevel(level);
		logger.setUseParentHandlers(false);
		// in JUnit tests, the intended fresh logger may actually be recycled and already have the handler set.
		Handler logHandler = null;
		for (Handler handler : logger.getHandlers()) {
			if (handler instanceof ConsoleHandler) {
				logHandler = handler;
				break;
			}
		}
		if (logHandler == null) {
			logHandler = new ConsoleHandler();
			logger.addHandler(logHandler);
		}
		logHandler.setLevel(level);
		return logger;
	}
}
