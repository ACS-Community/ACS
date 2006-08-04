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
import java.util.logging.Logger;

import org.apache.avalon.framework.configuration.Configuration;
import org.apache.avalon.framework.configuration.ConfigurationException;
import org.apache.avalon.framework.logger.Jdk14Logger;
import org.jacorb.config.LoggerFactory;

import alma.acs.logging.ClientLogManager;

/**
 * Logger factory that can be forced upon JacORB version 2.x via the <code>jacorb.log.loggerFactory</code> property.
 * It creates Loggers through {@link alma.acs.logging.ClientLogManager}, 
 * which ensures that JacORB logs will be inserted in the central ACS log stream.
 * <p>
 * These loggers will be configured through the normal ACS logging configuration mechanisms, 
 * whereas the properties <code>jacorb.log.default.verbosity</code>, <code>jacorb.logfile.append</code>, and <code>jacorb.log.default.log_pattern</code>
 * will be ignored.
 *  
 * @author hsommer
 */
public class JacORBLoggerFactory implements LoggerFactory {

    private final static String BACKEND_NAME = "jdk14";
    
    private Logger delegate;

    
    public JacORBLoggerFactory() {
    	System.out.println("**** JacORBLoggerFactory created");
    }
    
	public final String getLoggingBackendName() {
		return BACKEND_NAME;
	}

	/**
	 * @see org.jacorb.config.LoggerFactory#getNamedLogger(java.lang.String)
	 */
	public org.apache.avalon.framework.logger.Logger getNamedLogger(String name) {
		org.apache.avalon.framework.logger.Logger wrapper = new Jdk14Logger(getDelegate());
		return wrapper;
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
	 * We ignore the avalon logger configuration, since our underlying jdk14 logger and its handlers decide everything.
	 * @see org.apache.avalon.framework.configuration.Configurable#configure(org.apache.avalon.framework.configuration.Configuration)
	 */
	public void configure(Configuration conf) throws ConfigurationException {
		// nothing
	}

	
	// ---------------------------
	
	/**
	 * Lazy fetching of the shared jdk14 Logger to which all avalon loggers created by this factory will send their output.
	 * @return
	 */
	private synchronized Logger getDelegate() {
		if (delegate == null) {
			delegate = ClientLogManager.getAcsLogManager().getLoggerForContainer("JacORB"); // todo: separate getLoggerForCorba
		}
		return delegate;
	}
}
