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

import org.apache.avalon.framework.configuration.Configuration;
import org.apache.avalon.framework.configuration.ConfigurationException;
import org.apache.avalon.framework.logger.NullLogger;
import org.jacorb.config.LoggerFactory;

/**
 * A hack for COMP-1996 workaround.
 * 
 * @TODO remove this once COMP-1996 is resolved!
 * 
 * @author hsommer
 */
public class MuteJacORBLoggerFactory implements LoggerFactory {

    private final static String BACKEND_NAME = "null logger";
    
    private final NullLogger delegate = new NullLogger();

	public final String getLoggingBackendName() {
		return BACKEND_NAME;
	}

	public org.apache.avalon.framework.logger.Logger getNamedLogger(String name) {
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
		// nada
	}

}
