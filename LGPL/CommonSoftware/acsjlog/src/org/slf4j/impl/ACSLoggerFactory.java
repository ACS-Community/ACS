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
 * ACSLoggerFactory is an implementation of {@link ILoggerFactory} returning
 * the same {@link JDK14LoggerAdapter} instance with the same underlying ACS logger,
 * regardless of the "name" parameter for the requested logger.
 * 
 * The hibernate framework tries to use separate loggers with names being those of its java classes, 
 * e.g. "org.hibernate.cfg.Ejb3Column".
 * All of these logger requests are now served with the same logger called "hibernate"
 * or "hibernate@<container name>" if a process/container name is known to the ACS logging libs.
 * The same name reduction is used for JacORB logs, see 
 * {@link alma.acs.logging.adapters.JacORBLoggerFactory#getNamedLogger(String)}.
 * 
 * @author msekoranja
 */
public class ACSLoggerFactory implements ILoggerFactory
{
	private AcsLogger acsLoggerDelegate;
	private Logger jdkAdapter;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.slf4j.ILoggerFactory#getLogger(java.lang.String)
	 */
	public synchronized Logger getLogger(String name) {
		// protect against concurrent access of acsLoggerDelegate
		synchronized (this) {
			if (acsLoggerDelegate == null) {
				acsLoggerDelegate = ClientLogManager.getAcsLogManager().getLoggerForCorba("hibernate", true);
				jdkAdapter = new JDK14LoggerAdapter(acsLoggerDelegate);
			}
		}
//		System.out.println("**** Got hibernate logger " + acsLoggerDelegate.getName() + " -- " + name);
		return jdkAdapter;
	}
}
