/*
 *    ALMA - Atacama Large Millimiter Array
 *    (c) European Southern Observatory, 2002
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
package alma.jconttest.DummyComponentImpl;

import java.util.logging.Logger;

import alma.acs.component.ComponentImplBase;
import alma.acs.logging.ClientLogManager;
import alma.jconttest.DummyComponentOperations;
import alma.maciErrType.wrappers.AcsJComponentCleanUpEx;

/**
 * @author hsommer
 * created Nov 12, 2003 6:47:00 PM
 */
public class DummyComponentImpl extends ComponentImplBase implements DummyComponentOperations
{
	// super.m_logger is not reliable because no container services may be set during some special test execution.
	// Only access this field through methods getLogger().
	private Logger myLogger;
	
	/** 
	 * Logs a message, which should facilitate debugging the logs when the synchronization of deactivation with long-running methods is being tested.
	 */
	public void cleanUp() throws AcsJComponentCleanUpEx {
		getLogger().info("cleanUp called.");
		super.cleanUp();
	}

	
	
	/**
	 * @see alma.jconttest.DummyComponentOperations#dymmyComponentsCanDoCloseToNothing()
	 */
	public void dummyComponentsCanDoCloseToNothing()
	{
		// or actually nothing
	}

	public void callThatTakesSomeTime(int timeInMillisec) {
		if (timeInMillisec > 0) {
			getLogger().info("Called in thread " + Thread.currentThread().getName() + ". Will sleep for " + timeInMillisec + " millisec");
			long sleepTime = System.currentTimeMillis();
			try {
				Thread.sleep(timeInMillisec);
			} catch (InterruptedException e) {
				long wakeTime = System.currentTimeMillis();
				getLogger().warning("Sleep was interrupted after just " + (wakeTime - sleepTime) + " ms instead of " + timeInMillisec + " ms.");
			}
		}
		getLogger().info("slept enough.");
	}

	/**
	 * Gets a logger, even if this component does not run inside a container and thus no ContainerServices are provided.
	 * This is a special hack for this test component which should never be used for any real components.
	 */
	private Logger getLogger() {
		if (myLogger == null) {
			if (m_logger != null) {
				myLogger = m_logger;
			}
			else {
				myLogger = ClientLogManager.getAcsLogManager().getLoggerForApplication("DummyComponentImpl running outside of container", false);
			}
		}
		return myLogger;
	}
}
