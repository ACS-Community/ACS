package org.slf4j.impl;

import org.slf4j.Logger;

import alma.acs.logging.AcsLogger;
import alma.acs.logging.ClientLogManager;

/**
 * This class creates and administrates loggers for the hibernate framework
 * on behalf of the slf4j-ACS logger factory {@link ACSLoggerFactory}.
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
 * 
 * @author msekoranja, hsommer
 */
public class HibernateLoggerHelper {
	
	/**
	 * The slf4j adapter for the default hibernate logger.
	 */
	private Logger jdkAdapterDefault;

	/**
	 * The slf4j adapter for the sql hibernate logger.
	 */
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
	
	
	/**
	 * @see ACSLoggerFactory#getLogger(String)
	 */
	public Logger getLogger(String name) {

		if (name.equals("org.hibernate.SQL") || 
				name.startsWith("org.hibernate.type") || // there is an inconsistency in hibernate's parameter logging, where first "org.hibernate.type" is checked in NullableType#IS_VALUE_TRACING_ENABLED, but later loggers "org.hibernate.type.XYZ" get used.
				name.equals("org.hibernate.jdbc.util.SQLStatementLogger")) {
			if (jdkAdapterSql == null) {
				AcsLogger acsLoggerDelegateSql = ClientLogManager.getAcsLogManager().getLoggerForCorba(HIBERNATE_SQL_LOGGER_NAME_PREFIX, true);
				acsLoggerDelegateSql.addLoggerClass(JDK14LoggerAdapter.class);
				jdkAdapterSql = new JDK14LoggerAdapter(acsLoggerDelegateSql);
			}
			return jdkAdapterSql;
		}
		else {
			if (jdkAdapterDefault == null) {
				AcsLogger acsLoggerDelegateDefault = ClientLogManager.getAcsLogManager().getLoggerForCorba(HIBERNATE_LOGGER_NAME_PREFIX, true);
				acsLoggerDelegateDefault.addLoggerClass(JDK14LoggerAdapter.class);
				jdkAdapterDefault = new JDK14LoggerAdapter(acsLoggerDelegateDefault);
			}
			return jdkAdapterDefault;
		}
	}

	/**
	 * Should only be called by tests. 
	 * For example, JUnit test cases that use different logger names for every test
	 * need to call this in order for hibernate loggers to be re-created.
	 */
	public void clearDelegateLoggers() {
		jdkAdapterDefault = null;
		jdkAdapterSql = null;
	}
}
