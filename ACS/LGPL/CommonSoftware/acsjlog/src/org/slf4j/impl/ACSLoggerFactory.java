/*******************************************************************************
 * ALMA - Atacama Large Millimeter Array
 * Copyright (c) ESO - European Southern Observatory, 2011
 * (in the framework of the ALMA collaboration).
 * All rights reserved.
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version.
 * 
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * 
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307  USA
 *******************************************************************************/

package org.slf4j.impl;

import org.slf4j.ILoggerFactory;
import org.slf4j.Logger;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * ACSLoggerFactory is an implementation of slf4j's {@link ILoggerFactory} based on {@link AcsLogger}s, 
 * allowing for hibernate logs to be streamed into the ACS logging system.
 * This class gets connected to the slf4j framework via {@link StaticLoggerBinder}, which is pulled in by slf4j from the classpath.
 * Therefore no other slf4j bindings to different logging frameworks are allowed on the classpath.
 * <p>
 * A side effect of this is that any other software package which in the future might 
 * also rely on slf4j logging will get a logger called "hibernate" or "hibernate@&lt;process name&gt;".
 * <p>
 * Simplification of the large number of hibernate loggers:
 * The hibernate framework tries to use separate loggers with names being those of its java classes, 
 * e.g. "org.hibernate.cfg.Ejb3Column".
 * ACS maps all of these logger requests to only 2 different loggers, which can be configured separately:
 * <ul>
 *   <li>The default logger is called "hibernate", 
 *       or "hibernate@&lt;process-name&gt;" if a process/container name is known to the ACS logging libs.
 *       See also {@link #HIBERNATE_LOGGER_NAME_PREFIX}.</li>
 *   <li>SQL related hibernate loggers are mapped to "hibernateSQL", 
 *       or "hibernateSQL@&lt;process-name&gt;" if a process/container name is known to the ACS logging libs.
 *       See also {@link #HIBERNATE_SQL_LOGGER_NAME_PREFIX}.</li>
 * </ul>
 * A similar name reduction is used for JacORB loggers, see 
 * {@link alma.acs.logging.adapters.JacORBLoggerFactory#getNamedLogger(String)}.
 * 
 * @author msekoranja, hsommer
 */
public class ACSLoggerFactory implements ILoggerFactory
{
	private AcsLogger acsLoggerDelegateDefault;
	private Logger jdkAdapterDefault;

	private AcsLogger acsLoggerDelegateSql;
	private Logger jdkAdapterSql;

	/**
	 * Mapped to all hibernate loggers except those from {@link #HIBERNATE_SQL_LOGGER_NAME_PREFIX}.
	 */
	public static final String HIBERNATE_LOGGER_NAME_PREFIX = "hibernate";
	
	/**
	 * Mapped to hibernate loggers "org.hibernate.SQL" (for SQL statements)
	 * and "org.hibernate.type.xyz" (for SQL binding parameters).
	 * <p>
	 * Hibernate announces that version 4 will rename the SQL logger to "org.hibernate.jdbc.util.SQLStatementLogger"
	 * which is why we already map that currently not existing logger name to the same sql logger.
	 */
	public static final String HIBERNATE_SQL_LOGGER_NAME_PREFIX = "hibernateSQL";
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.slf4j.ILoggerFactory#getLogger(java.lang.String)
	 */
	public synchronized Logger getLogger(String name) {
		// protect against concurrent access of acsLoggerDelegateXyz
		synchronized (this) {
			if (name.equals("org.hibernate.SQL") || 
				name.startsWith("org.hibernate.type") || // there is an inconsistency in hibernate's parameter logging, where first "org.hibernate.type" is checked in NullableType#IS_VALUE_TRACING_ENABLED, but later loggers "org.hibernate.type.XYZ" get used.
				name.equals("org.hibernate.jdbc.util.SQLStatementLogger")) {
				if (acsLoggerDelegateSql == null) {
					acsLoggerDelegateSql = ClientLogManager.getAcsLogManager().getLoggerForCorba(HIBERNATE_SQL_LOGGER_NAME_PREFIX, true);
					acsLoggerDelegateSql.addLoggerClass(JDK14LoggerAdapter.class);
					jdkAdapterSql = new JDK14LoggerAdapter(acsLoggerDelegateSql);
				}
				return jdkAdapterSql;
			}
			else {
				if (acsLoggerDelegateDefault == null) {
					acsLoggerDelegateDefault = ClientLogManager.getAcsLogManager().getLoggerForCorba(HIBERNATE_LOGGER_NAME_PREFIX, true);
					acsLoggerDelegateDefault.addLoggerClass(JDK14LoggerAdapter.class);
					jdkAdapterDefault = new JDK14LoggerAdapter(acsLoggerDelegateDefault);
				}
				return jdkAdapterDefault;
				//System.out.println("**** Got hibernate logger '" + acsLoggerDelegate.getName() + "' for requested logger '" + name + "' ****");
			}
		}
	}
	
	/**
	 * Should only be called by tests. 
	 * For example, JUnit test cases that use different logger names for every test
	 * need to call this in order for hibernate loggers to be re-created.
	 */
	public synchronized void clearDelegateLoggers() {
		acsLoggerDelegateDefault = null;
		acsLoggerDelegateSql = null;
	}
}
