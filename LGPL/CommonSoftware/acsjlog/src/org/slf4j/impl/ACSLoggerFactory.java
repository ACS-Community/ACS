/* 
 * Copyright (c) 2004-2005 SLF4J.ORG
 * Copyright (c) 2004-2005 QOS.ch
 *
 * All rights reserved.
 * 
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to  deal in  the Software without  restriction, including
 * without limitation  the rights to  use, copy, modify,  merge, publish,
 * distribute, and/or sell copies of  the Software, and to permit persons
 * to whom  the Software is furnished  to do so, provided  that the above
 * copyright notice(s) and this permission notice appear in all copies of
 * the  Software and  that both  the above  copyright notice(s)  and this
 * permission notice appear in supporting documentation.
 * 
 * THE  SOFTWARE IS  PROVIDED  "AS  IS", WITHOUT  WARRANTY  OF ANY  KIND,
 * EXPRESS OR  IMPLIED, INCLUDING  BUT NOT LIMITED  TO THE  WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR  A PARTICULAR PURPOSE AND NONINFRINGEMENT
 * OF  THIRD PARTY  RIGHTS. IN  NO EVENT  SHALL THE  COPYRIGHT  HOLDER OR
 * HOLDERS  INCLUDED IN  THIS  NOTICE BE  LIABLE  FOR ANY  CLAIM, OR  ANY
 * SPECIAL INDIRECT  OR CONSEQUENTIAL DAMAGES, OR  ANY DAMAGES WHATSOEVER
 * RESULTING FROM LOSS  OF USE, DATA OR PROFITS, WHETHER  IN AN ACTION OF
 * CONTRACT, NEGLIGENCE  OR OTHER TORTIOUS  ACTION, ARISING OUT OF  OR IN
 * CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 * 
 * Except as  contained in  this notice, the  name of a  copyright holder
 * shall not be used in advertising or otherwise to promote the sale, use
 * or other dealings in this Software without prior written authorization
 * of the copyright holder.
 *
 */

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
