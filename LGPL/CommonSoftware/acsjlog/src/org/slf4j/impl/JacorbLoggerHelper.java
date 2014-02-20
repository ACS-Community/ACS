package org.slf4j.impl;

import org.slf4j.Logger;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;
import alma.acs.logging.adapters.JacORBFilter;

/**
 * This class creates and administrates loggers for jacorb 
 * on behalf of the slf4j-ACS logger factory {@link ACSLoggerFactory}.
 * <p>
 * Simplification of the large number of jacorb loggers:
 * All jacorb requests get the same ACS logger instance
 * that uses the name "jacorb" as given in {@link #JACORB_LOGGER_NAME},
 * or "jacorb@&lt;process-name&gt;" if a process/container name is known to the ACS logging libs.
 */ 
public class JacorbLoggerHelper {
	
	public static final String JACORB_LOGGER_NAME = "jacorb";
	
	/**
	 * The slf4j adapter for the jacorb logger.
	 */
	private Logger jdkAdapter = null;
	
	/**
	 * @see ACSLoggerFactory#getLogger(String)
	 */
	public Logger getLogger(String name) {
		if (jdkAdapter == null) {  // reuse one acs logger for all jacorb loggers
			AcsLogger acsLoggerDelegate = ClientLogManager.getAcsLogManager().getLoggerForCorba(JACORB_LOGGER_NAME, true);
			
			acsLoggerDelegate.addLoggerClass(JDK14LoggerAdapter.class);
			JacORBFilter logFilter = new JacORBFilter();
			logFilter.setLogLevel(acsLoggerDelegate.getLevel()); // AcsLogger will later update the filter log level if there are changes
			acsLoggerDelegate.setFilter(logFilter);
			
			jdkAdapter = new JDK14LoggerAdapter(acsLoggerDelegate);
		}
		return jdkAdapter;
	}
}
