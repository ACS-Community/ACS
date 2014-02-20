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
 * ACSLoggerFactory is an implementation of slf4j's {@link ILoggerFactory} based on {@link alma.acs.logging.AcsLogger}s, 
 * allowing for hibernate and jacorb 3.x logs to be streamed into the ACS logging system.
 * This class gets connected to the slf4j framework via {@link StaticLoggerBinder}, which is pulled in by slf4j from the classpath.
 * Therefore no other slf4j bindings to different logging frameworks are allowed on the classpath.
 * <p>
 * For clarity we delegate the hibernate and jacorb logger creation and lookup to a helper class each.
 */
public class ACSLoggerFactory implements ILoggerFactory
{
	/**
	 * Created on demand.
	 */
	private HibernateLoggerHelper hibernateLoggerHelper;

	/**
	 * Created on demand.
	 */
	private JacorbLoggerHelper jacorbLoggerHelper;
	
	/**
	 * The slf4j adapter for the default logger that should never be needed.
	 */
	private Logger jdkAdapterDefault = null;


	@Override
	public synchronized Logger getLogger(String name) {
		if (name.contains("hibernate")) {
			if (hibernateLoggerHelper == null) {
				hibernateLoggerHelper = new HibernateLoggerHelper();
			}
			return hibernateLoggerHelper.getLogger(name);
		}
		else if (name.startsWith("org.jacorb")) {
			if (jacorbLoggerHelper == null) {
				jacorbLoggerHelper = new JacorbLoggerHelper();
			}
			return jacorbLoggerHelper.getLogger(name);
		}
		else {
			// An unrecognized framework requested an slf4j logger. This should never happen. 
			// Let's return a default logger anyway.
			if (jdkAdapterDefault == null) {
				AcsLogger acsLoggerDelegateDefault = ClientLogManager.getAcsLogManager().getLoggerForCorba("UnknownSlf4j", true);
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
	public synchronized void clearDelegateLoggers() {
		if (hibernateLoggerHelper != null) {
			hibernateLoggerHelper.clearDelegateLoggers();
		}
	}
}
