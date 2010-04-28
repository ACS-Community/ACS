/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2005
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

package alma.acs.logging.adapters;

import java.io.IOException;
import java.util.logging.Level;

import org.apache.avalon.framework.configuration.Configuration;
import org.apache.avalon.framework.configuration.ConfigurationException;
import org.apache.avalon.framework.logger.Jdk14Logger;
import org.jacorb.config.LoggerFactory;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * Logger factory that can be forced upon JacORB version 2.x via the <code>jacorb.log.loggerFactory</code> property.
 * It creates Loggers through {@link alma.acs.logging.ClientLogManager}, 
 * which ensures that JacORB logs will be inserted in the central ACS log stream.
 * <p>
 * These loggers will be configured through the normal ACS logging configuration mechanisms, 
 * whereas the properties <code>jacorb.log.default.verbosity</code>, <code>jacorb.logfile.append</code>, and <code>jacorb.log.default.log_pattern</code>
 * will be ignored.
 * <b>Note that at the moment we do use <code>jacorb.log.default.verbosity</code> as a workaround!</b>
 *  
 * @author hsommer
 */
public class JacORBLoggerFactory implements LoggerFactory {

    private final static String BACKEND_NAME = "jdk14";
    
    private Jdk14Logger delegate;
    
    /** verbosity level 0-4 from jacorb property */
	private int jacOrbVerbosity;

    
    public JacORBLoggerFactory() {
//    	System.out.println("**** JacORBLoggerFactory created");
    }
    
	public final String getLoggingBackendName() {
		return BACKEND_NAME;
	}

	/**
	 * Returns an avalon logger which wraps the JDK logger obtained from {@link ClientLogManager}.
	 * @param name the name (e.g. "jacorb.poa") is ignored, since we treat all jacorb logging with just one logger.
	 * @see org.jacorb.config.LoggerFactory#getNamedLogger(java.lang.String)
	 */
	public org.apache.avalon.framework.logger.Logger getNamedLogger(String name) {
		if (delegate == null) {  // reuse one acs logger for all jacorb loggers
			AcsLogger acsDelegate = ClientLogManager.getAcsLogManager().getLoggerForCorba("jacorb", true);

			// @TODO The following "if" was merged from the 8.2 branch (TMCDB FBT), 
			// and now while merging looks a bit too specific for the case of the hibernate loggers.
			// Check if it makes sense also for a jacorb logger that has a custom configuration at creation time!
			if (!ClientLogManager.getAcsLogManager().getLogConfig().hasCustomConfig(acsDelegate.getName())) {
				
				// we can't use the same log levels that the container logger uses, because JacORB is much too verbose compared with ALMA code. 
				// Thus we restrict this Logger's level, assuming that the log handler will be generous enough always.
				// TODO: use custom Logger configuration in the CDB for Corba logger, instead of JacORB property
				// TODO: use a custom logger adapter, which translates the given avalon log levels to lower jdk log levels than is done now.
				
				// Log level description in orb.properties:
				// 0 = fatal errors only = "almost off" (FATAL ERRORS)
				// 1 = non-fatal errors and exceptions (ERROR)
				// 2 = important messages (WARN)
				// 3 = informational messages and exceptions (INFO)
				// 4 = debug-level output (DEBUG) (may confuse the unaware user :-)
				Level[] levelMap = new Level[] {Level.SEVERE, Level.WARNING, Level.INFO, Level.FINE, Level.ALL};
				
				if (jacOrbVerbosity >= 0 && jacOrbVerbosity <=4) {
					Level level = levelMap[jacOrbVerbosity];
					acsDelegate.setLevel(level);
				}
				else {
					acsDelegate.warning("Failed to adjust Corba logger level based on 'jacorb.log.default.verbosity'");
				}
			}
			acsDelegate.addLoggerClass(Jdk14Logger.class);
			JacORBFilter logFilter = new JacORBFilter();
			logFilter.setLogLevel(acsDelegate.getLevel()); // AcsLogger will later update the filter log level if there are changes
			acsDelegate.setFilter(logFilter);
			
			delegate = new Jdk14Logger(acsDelegate);
		}
		
		return delegate;
	}

	/**
	 * Same as {@link #getNamedLogger(String)}. The file is ignored, and a respective warning message gets logged.
	 * @see org.jacorb.config.LoggerFactory#getNamedLogger(java.lang.String, java.lang.String, long)
	 */
	public org.apache.avalon.framework.logger.Logger getNamedLogger(String name, String fileName, long maxFileSize) throws IOException {
		org.apache.avalon.framework.logger.Logger logger = getNamedLogger(name);
		logger.warn("This logger was created for logging to a file, which it will not do. Better use method 'getNamedLogger(String name)' instead!");
		return logger;
	}

	/**
	 * Returns {@link #getNamedLogger(String)}.
	 * The distinction between root loggers and other loggers we don't need here,
	 * since the questions of console vs. file vs. remote logging are left to ACS.
	 * @see org.jacorb.config.LoggerFactory#getNamedRootLogger(java.lang.String)
	 */
	public org.apache.avalon.framework.logger.Logger getNamedRootLogger(String name) {
		return getNamedLogger(name);
	}

	/**
	 * We ignore the log file, since ACS loggers forward to the central Log service.
	 * @see org.jacorb.config.LoggerFactory#setDefaultLogFile(java.lang.String, long)
	 */
	public void setDefaultLogFile(String fileName, long maxLogSize) throws IOException {
		// nothing
	}

	/**
	 * @see org.apache.avalon.framework.configuration.Configurable#configure(org.apache.avalon.framework.configuration.Configuration)
	 */
	public void configure(Configuration conf) throws ConfigurationException {		
		jacOrbVerbosity = conf.getAttributeAsInteger("jacorb.log.default.verbosity", 0);
//		System.out.println("******** JacORB Log configure called. Verbosity=" + jacOrbVerbosity);
	}

}
